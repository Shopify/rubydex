use crate::model::document::Document;
use crate::model::graph::{Graph, RemovedIds};
use crate::model::{
    definitions::Definition,
    ids::{DeclarationId, DefinitionId, UriId},
};
use rusqlite::{Connection, params};
use std::{cell::RefCell, error::Error, fs, path::Path};

use super::identity_maps::IdentityHashMap;

const SCHEMA_VERSION: u16 = 1;

const TABLE_DEFINITIONS: &str = "definitions";
const TABLE_DECLARATIONS: &str = "declarations";
const TABLE_DOCUMENTS: &str = "documents";

const ALL_TABLES: &[&str] = &[TABLE_DEFINITIONS, TABLE_DECLARATIONS, TABLE_DOCUMENTS];

pub struct DocumentData {
    pub document: Document,
    pub definitions: Vec<DefinitionData>,
}

pub struct DefinitionData {
    pub declaration_id: DeclarationId,
    pub name: String,
    pub definition_id: DefinitionId,
    pub definition: Definition,
}

#[derive(Default, Debug)]
pub struct Db {
    connection: RefCell<Option<Connection>>,
}

impl Db {
    const SCHEMA: &str = include_str!("../db/schema.sql");
    const LOAD_DOCUMENT: &str = include_str!("../db/load_document.sql");

    #[must_use]
    pub fn new() -> Self {
        Self {
            connection: RefCell::new(None),
        }
    }

    /// Initialize the database connection. To use an in memory database, pass `None`
    ///
    /// # Errors
    ///
    /// Initializing the connection may fail due to IO errors
    pub fn initialize_connection(&mut self, maybe_path: Option<String>) -> Result<(), Box<dyn Error>> {
        let conn = if let Some(path) = maybe_path {
            let mut conn = Connection::open(&path)?;
            let current_version: u16 = conn.query_row("PRAGMA user_version", [], |row| row.get(0))?;

            // If we bumped the schema version, recreate the database file from scratch since it's easier than
            // trying to run incremental migrations
            if current_version < SCHEMA_VERSION {
                drop(conn);
                if Path::new(&path).exists() {
                    fs::remove_file(&path)?;
                }

                conn = Connection::open(&path)?;
                Self::initialize_database(&mut conn)?;
            }

            conn
        } else {
            let mut conn = Connection::open_in_memory()?;
            Self::initialize_database(&mut conn)?;
            conn
        };

        // Set default connection pragmas
        conn.execute_batch(
            "
            PRAGMA synchronous = NORMAL;
            PRAGMA locking_mode = EXCLUSIVE;
            ",
        )?;

        self.connection.borrow_mut().replace(conn);
        Ok(())
    }

    /// Loads all of the graph data related to the given URI ID
    ///
    /// # Errors
    ///
    /// Loading data for a URI may fail if querying the database fails
    ///
    /// # Panics
    ///
    /// Will panic if the database connection is not initialized before trying to interact with it
    pub fn load_uri(&self, uri_id: UriId) -> Result<DocumentData, Box<dyn Error>> {
        self.with_connection(|connection| {
            let mut statement = connection.prepare(Self::LOAD_DOCUMENT)?;
            let mut document: Option<Document> = None;

            let rows = statement.query_map([*uri_id], |row| {
                // Content hash is the same for all rows, so we only need to read it once
                if document.is_none() {
                    let document_data = row.get::<_, Vec<u8>>(0)?;
                    document = Some(Document::deserialize(&document_data));
                }

                if let Some(declaration_id) = row.get::<_, Option<DeclarationId>>(1)? {
                    // If we have a declaration_id, then we have definition data
                    let name = row.get::<_, String>(2)?;
                    let definition_id: DefinitionId = row.get(3)?;
                    let data = row.get::<_, Vec<u8>>(4)?;

                    Ok(Some(DefinitionData {
                        declaration_id,
                        name,
                        definition_id,
                        definition: Definition::deserialize(&data),
                    }))
                } else {
                    // No definition data for this document
                    Ok(None)
                }
            })?;

            // Collect the rows, filtering out None values
            let definitions = rows.collect::<Result<Vec<_>, _>>()?.into_iter().flatten().collect();

            Ok(DocumentData {
                document: document.expect("Document must be present if loading data for a URI"),
                definitions,
            })
        })
    }

    /// Saves the full graph to the database. This is ONLY meant to be used for newly created database files and not
    /// synchronization as it will not try to update existing records for better insertion performance
    ///
    /// # Errors
    ///
    /// Errors on any type of database connection or operation failure
    ///
    /// # Panics
    ///
    /// Will panic if the database connection is not initialized before trying to interact with it
    pub fn save_full_graph(&self, graph: &Graph) -> Result<(), Box<dyn Error>> {
        self.with_connection(|connection| {
            let tx = connection.transaction()?;

            Self::batch_insert_documents(&tx, graph)?;
            Self::batch_insert_names(&tx, graph)?;
            Self::batch_insert_definitions(&tx, graph)?;

            tx.commit()?;
            Ok(())
        })
    }

    /// Deletes all of the data related to the given URI ID along with the removed definitions and names.
    ///
    /// # Errors
    ///
    /// Errors on any type of database connection or operation failure
    pub fn delete_data_for_uri(&self, uri_id: UriId, removed_ids: &RemovedIds) -> Result<(), Box<dyn Error>> {
        self.with_connection(|connection| {
            let tx = connection.transaction()?;

            {
                tx.prepare_cached(&format!("DELETE FROM {TABLE_DOCUMENTS} WHERE id = ?"))?
                    .execute([*uri_id])?;

                let mut stmt = tx.prepare_cached(&format!("DELETE FROM {TABLE_DEFINITIONS} WHERE id = ?"))?;
                for id in &removed_ids.definition_ids {
                    stmt.execute([**id])?;
                }

                stmt = tx.prepare_cached(&format!("DELETE FROM {TABLE_DECLARATIONS} WHERE id = ?"))?;
                for id in &removed_ids.declaration_ids {
                    stmt.execute([**id])?;
                }
            }

            tx.commit()?;
            Ok(())
        })
    }

    /// # Errors
    ///
    /// Errors on any type of database connection or operation failure
    pub fn get_all_content_hashes(&self) -> Result<IdentityHashMap<UriId, u16>, Box<dyn Error>> {
        self.with_connection(|connection| {
            let mut statement = connection.prepare("SELECT id, content_hash FROM documents")?;
            statement
                .query_map([], |row| Ok((row.get(0)?, row.get(1)?)))?
                .collect::<Result<IdentityHashMap<UriId, u16>, _>>()
                .map_err(Into::into)
        })
    }

    /// Clears all data from the database tables
    ///
    /// # Errors
    ///
    /// Errors on any type of database connection or operation failure
    ///
    /// # Panics
    ///
    /// Will panic if the database connection is not initialized before trying to interact with it
    pub fn clear_database(&self) -> Result<(), Box<dyn Error>> {
        self.with_connection(|connection| {
            let tx = connection.transaction()?;

            for table in ALL_TABLES {
                tx.execute(&format!("DELETE FROM {table}"), [])?;
            }

            tx.commit()?;
            Ok(())
        })
    }

    // Run the given closure with a connection to the database. In the future, we can make the database abstraction more
    // robust by handling the different rusqlite error codes and trying to recover when possible
    fn with_connection<F, T>(&self, f: F) -> Result<T, Box<dyn Error>>
    where
        F: FnOnce(&mut Connection) -> Result<T, Box<dyn Error>>,
    {
        let mut borrow = self.connection.borrow_mut();
        let connection: &mut Connection = borrow
            .as_mut()
            .expect("Database connection has to be initialized ahead of time");

        f(connection)
    }

    /// Initializes a fresh database with schema and configuration
    fn initialize_database(conn: &mut Connection) -> Result<(), Box<dyn Error>> {
        // Set Pragmas first
        conn.execute_batch(
            "
            PRAGMA journal_mode = WAL;
            ",
        )?;

        // Perform remaining DB init within at tx
        let tx = conn.transaction()?;
        tx.execute_batch(Self::SCHEMA)?;
        tx.execute(&format!("PRAGMA user_version = {SCHEMA_VERSION}"), [])?;
        tx.commit()?;
        Ok(())
    }

    /// Performs batch insert of documents (URIs) to the database
    fn batch_insert_documents(conn: &rusqlite::Connection, graph: &Graph) -> Result<(), Box<dyn Error>> {
        let mut stmt = conn.prepare_cached(&format!(
            "INSERT INTO {TABLE_DOCUMENTS} (id, content_hash, data) VALUES (?, ?, ?)"
        ))?;

        for (uri_id, document) in graph.documents() {
            stmt.execute(rusqlite::params![
                *uri_id,
                document.content_hash(),
                document.serialize()
            ])?;
        }

        Ok(())
    }

    /// Performs batch insert of names to the database
    fn batch_insert_names(conn: &rusqlite::Connection, graph: &Graph) -> Result<(), Box<dyn Error>> {
        let mut stmt = conn.prepare_cached(&format!("INSERT INTO {TABLE_DECLARATIONS} (id, name) VALUES (?, ?)"))?;

        for (declaration_id, declaration) in graph.declarations() {
            stmt.execute(rusqlite::params![*declaration_id, declaration.name()])?;
        }

        Ok(())
    }

    /// Performs batch insert of definitions to the database
    fn batch_insert_definitions(conn: &rusqlite::Connection, graph: &Graph) -> Result<(), Box<dyn Error>> {
        let mut stmt = conn.prepare_cached(&format!(
            "INSERT INTO {TABLE_DEFINITIONS} (id, declaration_id, document_id, data) VALUES (?, ?, ?, ?)"
        ))?;

        for (definition_id, definition) in graph.definitions() {
            let declaration_id = *definition.declaration_id();
            let uri_id = *definition.uri_id();

            stmt.execute(params![
                *definition_id,
                *declaration_id,
                *uri_id,
                definition.serialize()
            ])?;
        }

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_utils::GraphTest;

    #[test]
    fn saving_graph_to_the_database() {
        let mut context = GraphTest::new();
        context.index_uri("file:///complex.rb", {
            "
            module Outer
              # Class comment
              # Another class comment
              class Inner
                def method_name
                  puts 'hello'
                end
                CONSTANT = 42
              end
            end
            "
        });

        let mut db = Db::new();
        db.initialize_connection(None).unwrap();
        assert!(db.save_full_graph(&context.graph).is_ok());

        // Test loading the saved data using load_uri
        let uri_id = UriId::from("file:///complex.rb");
        let document_data = db.load_uri(uri_id).unwrap();

        // Should have multiple definitions: Outer, Inner, method_name, CONSTANT
        assert!(document_data.definitions.len() == 4);

        // Verify we can find expected definitions
        let module_def = document_data.definitions.iter().find(|r| r.name == "Outer").unwrap();
        let class_def = document_data
            .definitions
            .iter()
            .find(|r| r.name == "Outer::Inner")
            .unwrap();
        let method_def = document_data
            .definitions
            .iter()
            .find(|r| r.name == "Outer::Inner::method_name")
            .unwrap();
        let constant_def = document_data
            .definitions
            .iter()
            .find(|r| r.name == "Outer::Inner::CONSTANT")
            .unwrap();

        assert!(matches!(module_def.definition, Definition::Module(_)));
        assert!(matches!(class_def.definition, Definition::Class(_)));
        assert!(matches!(method_def.definition, Definition::Method(_)));
        assert!(matches!(constant_def.definition, Definition::Constant(_)));

        let comments = class_def
            .definition
            .comments()
            .iter()
            .map(|c| c.string().to_string())
            .collect::<Vec<String>>();
        assert_eq!(*comments, vec!["# Class comment", "# Another class comment"]);
    }

    #[test]
    fn removing_orphaned_entries() {
        let mut context = GraphTest::new();

        context.index_uri("file:///sample.rb", {
            "
            module TopLevel
              class OuterClass
                module InnerModule
                  # Important constant
                  IMPORTANT_CONSTANT = 'value'

                  class DeepClass
                    def instance_method
                      puts 'instance'
                    end

                    def self.class_method
                      puts 'class'
                    end

                    private

                    def private_method
                      'private'
                    end
                  end

                  def self.module_method
                    'module level'
                  end
                end

                OUTER_CONSTANT = 42

                def outer_method
                  'outer'
                end
              end
            end
            "
        });

        let mut db = Db::new();
        db.initialize_connection(None).unwrap();
        assert!(db.save_full_graph(&context.graph).is_ok());

        let removed_ids = context.graph.unload_uri("file:///sample.rb");

        assert_eq!(removed_ids.definition_ids.len(), 11);
        assert_eq!(removed_ids.declaration_ids.len(), 11);

        assert!(
            db.delete_data_for_uri(UriId::from("file:///sample.rb"), &removed_ids)
                .is_ok()
        );

        let borrow = db.connection.borrow();
        let connection = borrow.as_ref().unwrap();

        let doc_count: i64 = connection
            .prepare("SELECT COUNT(*) FROM documents")
            .unwrap()
            .query_row((), |row| row.get(0))
            .unwrap();
        assert_eq!(doc_count, 0);

        let def_count: i64 = connection
            .prepare("SELECT COUNT(*) FROM definitions")
            .unwrap()
            .query_row((), |row| row.get(0))
            .unwrap();
        assert_eq!(def_count, 0,);

        let name_count: i64 = connection
            .prepare("SELECT COUNT(*) FROM declarations")
            .unwrap()
            .query_row((), |row| row.get(0))
            .unwrap();

        // Only the declaration for <main> should remain
        assert_eq!(name_count, 1);
    }

    #[test]
    fn querying_content_hashes_by_uri() {
        let mut context = GraphTest::new();

        context.index_uri("file:///first.rb", "module FirstModule; end");
        context.index_uri("file:///second.rb", "class SecondClass; end");

        let mut db = Db::new();
        db.initialize_connection(None).unwrap();
        assert!(db.save_full_graph(&context.graph).is_ok());

        let result = db.get_all_content_hashes().unwrap();

        assert_eq!(result.len(), 2);
        let first_uri = UriId::from("file:///first.rb");
        let second_uri = UriId::from("file:///second.rb");
        assert!(result.contains_key(&first_uri));
        assert!(result.contains_key(&second_uri));
    }
}
