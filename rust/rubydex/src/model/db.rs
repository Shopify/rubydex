use rusqlite::Connection;
use std::{error::Error, fs, path::PathBuf};

use crate::model::{graph::Graph, ids::UriId};

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

        // TODO: Serialize the document + all data that can be reached through it (declarations, names, strings,
        // definitions) and save it to the database. In the process of serializing the data, we also need to track which
        // documents depend on which other documents, which is used to determine all of the data that needs to be loaded
        // together

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
    pub fn load_documents(&mut self, graph: &mut Graph, id: &[UriId]) -> Result<(), Box<dyn Error>> {}

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

        let schema = std::fs::read_to_string(
            std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
                .join("src")
                .join("db")
                .join("schema.sql"),
        )?;
        let tx = connection.transaction()?;
        tx.execute_batch(&schema)?;
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

        let mut db = Db::new(std::env::temp_dir().join("test.db"));
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
        let mut db = Db::new(std::env::temp_dir().join("test.db"));
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
        let mut db = Db::new(std::env::temp_dir().join("test.db"));
        db.connect().expect("Failed to connect to database");
        db.save_modifications(
            context.graph(),
            &context.graph().documents().keys().copied().collect::<Vec<_>>(),
        )
        .expect("Failed to save modifications");

        let mut fresh_graph = Graph::new();

        db.load_documents(&mut fresh_graph, &[UriId::from("file:///foo2.rb")])
            .expect("Failed to load documents");

        assert_declaration_exists!(fresh_graph, "Foo");
        assert_declaration_exists!(fresh_graph, "Foo::<Foo>");
        assert_declaration_exists!(fresh_graph, "Foo::<Foo>#bar()");
        assert_declaration_exists!(fresh_graph, "Foo#baz()");
        assert_ancestors_eq!(fresh_graph, "Foo", ["Foo", "Object", "Kernel", "BasicObject"]);
        assert_members_eq!(fresh_graph, "Foo", ["bar", "baz"]);
        assert_declaration_references_count_eq!(fresh_graph, "Foo", 1);
    }
}
