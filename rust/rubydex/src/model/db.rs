use rusqlite::Connection;
use std::{collections::HashSet, error::Error, fs, path::PathBuf};

use crate::model::{
    graph::{CachedDocument, Graph},
    ids::UriId,
};

#[derive(Debug)]
pub struct Db {
    path: PathBuf,
    connection: Option<Connection>,
}

impl Db {
    const SCHEMA_VERSION: u8 = 1;

    #[must_use]
    pub fn new(path: PathBuf) -> Self {
        Self { path, connection: None }
    }

    /// Establish a connection to the database. If the schema version has been updated since the last connection, it
    /// will be re-create from scratch
    ///
    /// # Errors
    ///
    /// Will return an Error if we fail to establish or set a connection
    pub fn connect(&mut self) -> Result<(), Box<dyn Error>> {
        let conn = Connection::open(&self.path)?;
        let current_version: u8 = conn.query_row("PRAGMA user_version", [], |row| row.get(0))?;

        let conn = if current_version < Self::SCHEMA_VERSION {
            drop(conn);
            self.setup_database()?;
            Connection::open(&self.path)?
        } else {
            conn
        };

        conn.execute_batch(
            "
            PRAGMA synchronous = OFF;
            PRAGMA foreign_keys = OFF;
            PRAGMA cache_size = -262144;
            PRAGMA mmap_size = 1073741824;
            PRAGMA temp_store = MEMORY;
            PRAGMA threads = 4;
            ",
        )?;

        self.connection = Some(conn);
        Ok(())
    }

    /// Updates the database with the modified graph data, which on launch is the entire graph. Receives the list of
    /// modified documents and the graph, so that it can fetch global data related to documents like names, declarations
    /// and strings.
    ///
    /// # Errors
    ///
    /// Will return an Error if we fail to establish or set a connection
    pub fn save_modifications(&mut self, graph: &Graph, modified_documents: &[UriId]) -> Result<(), Box<dyn Error>> {
        let connection = self.connection.as_mut().ok_or("No connection established")?;
        let tx = connection.transaction()?;

        {
            let mut statement = tx.prepare_cached(
                "INSERT OR REPLACE INTO documents (id, content_hash, dependent_documents, data) VALUES (?, ?, ?, ?)",
            )?;

            for uri_id in modified_documents {
                // The built-in document is re-seeded by `Graph::new`, so it is never cached (and
                // `build_cached_document` returns `None` for it).
                let Some((cached_document, dependent_documents)) = graph.build_cached_document(*uri_id) else {
                    continue;
                };

                let content_hash = cached_document.content_hash();
                let data = postcard::to_stdvec(&cached_document)?;
                let dependent_documents = postcard::to_stdvec(&dependent_documents)?;

                statement.execute(rusqlite::params![
                    uri_id.get().to_string(),
                    // SQLite integers are signed; store the hash's bit pattern losslessly.
                    i64::from_le_bytes(content_hash.to_le_bytes()),
                    dependent_documents,
                    data,
                ])?;
            }
        }

        let transaction_result = tx.commit();

        // Optimize after large writes
        if modified_documents.len() > 10_000 {
            connection.execute_batch(
                "
                PRAGMA optimize;
                ",
            )?;
        }

        transaction_result.map_err(|e| Box::new(e) as Box<dyn Error>)
    }

    /// Loads the given list of documents, mutating the graph to insert the data coming from the database.
    ///
    /// # Errors
    ///
    /// Will return an Error if we fail to establish or set a connection, or if we fail to load the documents from the
    /// database.
    pub fn load_documents(&mut self, graph: &mut Graph, ids: &[UriId]) -> Result<(), Box<dyn Error>> {
        let connection = self.connection.as_ref().ok_or("No connection established")?;

        // Breadth-first traversal of the dependent closure. A document and its dependents must be
        // loaded together so the restored graph is internally consistent (co-declarations, members
        // contributed from other files, resolved references). `seen` is keyed by the numeric row id
        // (`UriId::get`) and marks entries when they are enqueued so a document is loaded once even
        // if several others depend on it (the graph is cyclic).
        let mut seen: HashSet<u64> = HashSet::new();
        let mut frontier: Vec<u64> = Vec::new();
        for id in ids {
            if seen.insert(id.get()) {
                frontier.push(id.get());
            }
        }

        while !frontier.is_empty() {
            let placeholders = vec!["?"; frontier.len()].join(",");
            let mut statement =
                connection.prepare(&format!("SELECT dependent_documents, data FROM documents WHERE id IN ({placeholders})"))?;

            let parameters = frontier.iter().map(u64::to_string).collect::<Vec<_>>();
            let rows = statement.query_map(rusqlite::params_from_iter(&parameters), |row| {
                let dependent_documents: Vec<u8> = row.get(0)?;
                let data: Vec<u8> = row.get(1)?;
                Ok((dependent_documents, data))
            })?;

            let mut next_frontier: Vec<u64> = Vec::new();
            for row in rows {
                let (dependent_documents, data) = row?;

                let cached_document: CachedDocument = postcard::from_bytes(&data)?;
                graph.merge_cached_document(cached_document);

                for dependent in postcard::from_bytes::<Vec<UriId>>(&dependent_documents)? {
                    if seen.insert(dependent.get()) {
                        next_frontier.push(dependent.get());
                    }
                }
            }

            frontier = next_frontier;
        }

        Ok(())
    }

    /// Creates a fresh file database file
    fn setup_database(&mut self) -> Result<(), Box<dyn Error>> {
        // Remove the file if it exists
        match fs::remove_file(&self.path) {
            Ok(()) => (),
            Err(e) if e.kind() == std::io::ErrorKind::NotFound => (),
            Err(e) => return Err(Box::new(e)),
        }

        let mut connection = Connection::open(&self.path)?;
        connection.execute_batch(
            "
            PRAGMA page_size = 8192;
            PRAGMA auto_vacuum = NONE;
            PRAGMA journal_mode = MEMORY;
            ",
        )?;

        // Embed the schema in the binary: reading it from the source tree at runtime
        // (`env!("CARGO_MANIFEST_DIR")`) would fail on any machine that only has the compiled
        // artifact, e.g. the shipped gem.
        let schema = include_str!("db/schema.sql");
        let tx = connection.transaction()?;
        tx.execute_batch(schema)?;
        tx.execute(&format!("PRAGMA user_version = {}", Self::SCHEMA_VERSION), [])?;
        tx.commit()?;
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        assert_ancestors_eq, assert_declaration_exists, assert_declaration_references_count_eq, assert_members_eq,
        test_utils::GraphTest,
    };

    #[test]
    fn saving_the_complete_graph() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            class Foo
              def bar; end
            end
            "
        });
        context.index_uri("file:///foo2.rb", {
            r"
            class Foo
              def baz; end
            end
            "
        });
        context.resolve();

        let temp_dir = tempfile::tempdir().expect("Failed to create temp dir");
        let mut db = Db::new(temp_dir.path().join("cache.db"));
        db.connect().expect("Failed to connect to database");
        db.save_modifications(
            context.graph(),
            &context.graph().documents().keys().copied().collect::<Vec<_>>(),
        )
        .expect("Failed to save modifications");
    }

    #[test]
    fn saving_a_document_modification() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            class Foo
              def bar; end
            end
            "
        });
        context.index_uri("file:///foo2.rb", {
            r"
            class Foo
              def baz; end
            end
            "
        });
        context.resolve();
        let temp_dir = tempfile::tempdir().expect("Failed to create temp dir");
        let mut db = Db::new(temp_dir.path().join("cache.db"));
        db.connect().expect("Failed to connect to database");
        db.save_modifications(
            context.graph(),
            &context.graph().documents().keys().copied().collect::<Vec<_>>(),
        )
        .expect("Failed to save modifications");

        context.index_uri("file:///foo2.rb", {
            r"
            class Qux; end

            class Foo < Qux
              def baz; end
            end
            "
        });
        context.resolve();

        db.save_modifications(context.graph(), &[UriId::from("file:///foo2.rb")])
            .expect("Failed to save modifications");
    }

    #[test]
    fn loading_a_document_with_dependencies_from_the_database() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            class Foo
              def self.bar; end
            end

            Foo.bar
            "
        });
        context.index_uri("file:///foo2.rb", {
            r"
            class Foo
              def baz; end
            end
            "
        });
        context.resolve();
        let temp_dir = tempfile::tempdir().expect("Failed to create temp dir");
        let mut db = Db::new(temp_dir.path().join("cache.db"));
        db.connect().expect("Failed to connect to database");
        db.save_modifications(
            context.graph(),
            &context.graph().documents().keys().copied().collect::<Vec<_>>(),
        )
        .expect("Failed to save modifications");

        // Load into a brand-new graph (only built-ins seeded) to prove the cache restores the
        // resolved state without re-running resolution.
        let mut fresh_context = GraphTest::new();

        db.load_documents(fresh_context.graph_mut(), &[UriId::from("file:///foo2.rb")])
            .expect("Failed to load documents");

        assert_declaration_exists!(fresh_context, "Foo");
        assert_declaration_exists!(fresh_context, "Foo::<Foo>");
        assert_declaration_exists!(fresh_context, "Foo::<Foo>#bar()");
        assert_declaration_exists!(fresh_context, "Foo#baz()");
        assert_ancestors_eq!(fresh_context, "Foo", ["Foo", "Object", "Kernel", "BasicObject"]);
        // `bar` is a singleton method (`def self.bar`), so it is a member of `Foo::<Foo>`, not `Foo`.
        assert_members_eq!(fresh_context, "Foo", ["baz()"]);
        assert_members_eq!(fresh_context, "Foo::<Foo>", ["bar()"]);
        assert_declaration_references_count_eq!(fresh_context, "Foo", 1);
    }
}
