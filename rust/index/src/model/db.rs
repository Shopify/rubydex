use crate::model::graph::Graph;
use crate::model::{definitions::Definition, ids::UriId};
use rusqlite::{Connection, params};
use std::{cell::RefCell, error::Error, fs, path::Path};

const SCHEMA_VERSION: u16 = 1;

pub struct LoadResult {
    pub name_id: u64,
    pub name: String,
    pub definition_id: u64,
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
            PRAGMA synchronous = OFF;
            PRAGMA foreign_keys = ON;
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
    pub fn load_uri(&self, uri_id: String) -> Result<Vec<LoadResult>, Box<dyn Error>> {
        self.with_connection(|connection| {
            let mut statement = connection.prepare(Self::LOAD_DOCUMENT)?;

            statement
                .query_map([uri_id], |row| {
                    let name_id = row.get::<_, String>(0)?;
                    let name = row.get::<_, String>(1)?;
                    let definition_id = row.get::<_, String>(2)?;
                    let data = row.get::<_, Vec<u8>>(3)?;
                    let definition = rmp_serde::from_slice::<Definition>(&data)
                        .expect("Deserializing the definition from the DB should always succeed");

                    Ok(LoadResult {
                        name_id: name_id.parse().expect("IDs should always be valid u64s"),
                        name,
                        definition_id: definition_id.parse().expect("IDs should always be valid u64s"),
                        definition,
                    })
                })?
                .collect::<Result<Vec<_>, _>>()
                .map_err(std::convert::Into::into)
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

    /// Deletes all of the data related to the given URI ID. This method relies on the foreign key constraints to
    /// cascade the deletion to definitions and names that are no longer referenced
    ///
    /// # Errors
    ///
    /// Errors on any type of database connection or operation failure
    pub fn delete_data_for_uri(&self, uri_id: UriId) -> Result<(), Box<dyn Error>> {
        self.with_connection(|connection| {
            let id_as_string = uri_id.to_string();
            let mut stmt = connection.prepare_cached("DELETE FROM documents WHERE id = ?")?;
            stmt.execute([id_as_string])?;
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
        conn.execute_batch("PRAGMA journal_mode = memory;")?;

        // Perform remaining DB init within at tx
        let tx = conn.transaction()?;
        tx.execute_batch(Self::SCHEMA)?;
        tx.execute(&format!("PRAGMA user_version = {SCHEMA_VERSION}"), [])?;
        tx.commit()?;
        Ok(())
    }

    /// Performs batch insert of documents (URIs) to the database
    fn batch_insert_documents(conn: &rusqlite::Connection, graph: &Graph) -> Result<(), Box<dyn Error>> {
        let mut stmt = conn.prepare_cached("INSERT INTO documents (id, uri) VALUES (?, ?)")?;

        for (uri_id, document) in graph.documents() {
            stmt.execute(params![uri_id.to_string(), document.uri()])?;
        }

        Ok(())
    }

    /// Performs batch insert of names to the database
    fn batch_insert_names(conn: &rusqlite::Connection, graph: &Graph) -> Result<(), Box<dyn Error>> {
        let mut stmt = conn.prepare_cached("INSERT INTO names (id, name) VALUES (?, ?)")?;

        for (name_id, declaration) in graph.declarations() {
            stmt.execute(params![name_id.to_string(), declaration.name()])?;
        }

        Ok(())
    }

    /// Performs batch insert of definitions to the database
    fn batch_insert_definitions(conn: &rusqlite::Connection, graph: &Graph) -> Result<(), Box<dyn Error>> {
        let mut stmt =
            conn.prepare_cached("INSERT INTO definitions (id, name_id, document_id, data) VALUES (?, ?, ?, ?)")?;

        for (definition_id, definition) in graph.definitions() {
            let data = rmp_serde::to_vec(definition).expect("Serializing definitions should always succeed");
            let name_id = definition.name_id().to_string();
            let uri_id = definition.uri_id().to_string();

            stmt.execute(params![definition_id.to_string(), name_id, uri_id, data])?;
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
        context.index_uri(
            "file:///foo.rb",
            "
            module Foo
              module Bar
                module Baz
                  class Qux
                    def initialize
                      @name = 'hi'
                    end
                  end
                end
              end
            end
            ",
        );

        let mut db = Db::new();
        db.initialize_connection(None).unwrap();
        db.save_full_graph(&context.graph).unwrap();

        // Query to grab all of the data
        let query = "
            SELECT
                names.*,
                definitions.*,
                documents.*
            FROM documents
            JOIN definitions ON documents.id = definitions.document_id
            JOIN names ON names.id = definitions.name_id
            ORDER BY names.name ASC
        ";

        let borrow = db.connection.borrow();
        let connection = borrow.as_ref().unwrap();
        let mut stmt = connection.prepare(query).unwrap();
        let mut data = stmt
            .query_map((), |row| {
                Ok((
                    row.get::<_, String>(0).unwrap(),
                    row.get::<_, String>(1).unwrap(),
                    row.get::<_, String>(2).unwrap(),
                    row.get::<_, String>(3).unwrap(),
                    row.get::<_, String>(4).unwrap(),
                    row.get::<_, Vec<u8>>(5).unwrap(),
                    row.get::<_, String>(6).unwrap(),
                    row.get::<_, String>(7).unwrap(),
                ))
            })
            .unwrap()
            .collect::<Vec<_>>();

        let (
            name_id,
            name,
            _definition_id,
            definition_name_id,
            definition_document_id,
            definition_data,
            document_id,
            document_uri,
        ) = data.remove(0).unwrap();

        let definition = rmp_serde::from_slice::<crate::model::definitions::Definition>(&definition_data).unwrap();

        match definition {
            Definition::Module(_) => {}
            _ => panic!("Expected a module definition"),
        }

        // Verify that the data we saved matches what is expected from the graph
        assert_eq!(name_id, definition_name_id);
        assert_eq!(document_id, definition_document_id);
        assert_eq!(name, String::from("Foo"));
        assert_eq!(0, definition.start());
        assert_eq!(140, definition.end());
        assert_eq!(document_uri, String::from("file:///foo.rb"));
    }

    #[test]
    fn deleting_data_for_a_uri() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", "module Foo; end");

        let mut db = Db::new();
        db.initialize_connection(None).unwrap();
        assert!(db.save_full_graph(&context.graph).is_ok());
        assert!(db.delete_data_for_uri(UriId::from("file:///foo.rb")).is_ok());

        // Query to grab all of the data
        let query = "
            SELECT
                names.*,
                definitions.*,
                documents.*
            FROM documents
            JOIN definitions ON documents.id = definitions.document_id
            JOIN names ON names.id = definitions.name_id
        ";

        let borrow = db.connection.borrow();
        let connection = borrow.as_ref().unwrap();
        let mut stmt = connection.prepare(query).unwrap();
        assert!(stmt.query_map((), |_| Ok(())).unwrap().next().is_none());
    }
}
