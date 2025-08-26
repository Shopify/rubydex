use crate::model::graph::Graph;
use rusqlite::{Connection, params};
use std::error::Error;

const SCHEMA_VERSION: u16 = 1;

#[derive(Default, Debug)]
enum ConnectionType {
    Path(String),
    #[default]
    Memory,
}

#[derive(Default, Debug)]
pub struct Db {
    connection_type: ConnectionType,
}

impl Db {
    const SCHEMA: &str = include_str!("../db/schema.sql");

    #[must_use]
    pub fn new() -> Self {
        Self {
            connection_type: ConnectionType::Memory,
        }
    }

    pub fn set_db_path(&mut self, path: String) {
        self.connection_type = ConnectionType::Path(path);
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

    /// Creates a fresh file database by removing existing file and recreating
    fn recreate_database(&self) -> Result<Connection, Box<dyn Error>> {
        match &self.connection_type {
            ConnectionType::Path(path) => {
                if std::path::Path::new(path).exists() {
                    std::fs::remove_file(path)?;
                }
                let mut conn = Connection::open(path)?;
                Self::initialize_database(&mut conn)?;
                Ok(conn)
            }
            ConnectionType::Memory => Err("Cannot recreate in-memory database".into()),
        }
    }

    /// Saves the full graph to the database. This is ONLY meant to be used for newly created database files and not
    /// synchronization as it will not try to update existing records for better insertion performance
    ///
    /// # Errors
    ///
    /// Errors on any type of database connection or operation failure
    pub fn save_full_graph(&self, graph: &Graph) -> Result<(), Box<dyn Error>> {
        let mut conn = self.get_connection()?;
        let tx = conn.transaction()?;

        Self::batch_insert_documents(&tx, graph)?;
        Self::batch_insert_names(&tx, graph)?;
        Self::batch_insert_definitions(&tx, graph)?;

        tx.commit()?;
        Ok(())
    }

    /// # Errors
    /// Will return an Error if we fail to establish or set a connection
    fn get_connection(&self) -> Result<Connection, Box<dyn Error>> {
        let conn = match &self.connection_type {
            ConnectionType::Path(path) => {
                let conn = Connection::open(path)?;
                // Check if schema needs to be loaded using user_version pragma.
                // The default value for user_version is 0.
                let current_version: u16 = conn.query_row("PRAGMA user_version", [], |row| row.get(0))?;
                if current_version < SCHEMA_VERSION {
                    drop(conn);
                    return self.recreate_database();
                }
                conn
            }
            ConnectionType::Memory => {
                let mut conn = Connection::open_in_memory()?;
                Self::initialize_database(&mut conn)?;
                conn
            }
        };

        // Default connection PRAGMAS
        conn.execute_batch(
            "
            PRAGMA synchronous = NORMAL;
            PRAGMA foreign_keys = ON;
            PRAGMA locking_mode = EXCLUSIVE;
            ",
        )?;

        Ok(conn)
    }

    /// Performs batch insert of documents (URIs) to the database
    fn batch_insert_documents(conn: &rusqlite::Connection, graph: &Graph) -> Result<(), Box<dyn Error>> {
        let mut stmt = conn.prepare_cached("INSERT INTO documents (id, uri) VALUES (?, ?)")?;

        for (uri_id, uri) in graph.uri_pool() {
            stmt.execute([&uri_id.to_string(), uri])?;
        }

        Ok(())
    }

    /// Performs batch insert of names to the database
    fn batch_insert_names(conn: &rusqlite::Connection, graph: &Graph) -> Result<(), Box<dyn Error>> {
        let mut stmt = conn.prepare_cached("INSERT INTO names (id, name) VALUES (?, ?)")?;

        for (name_id, name) in graph.names() {
            stmt.execute([&name_id.to_string(), name])?;
        }

        Ok(())
    }

    /// Performs batch insert of definitions to the database
    fn batch_insert_definitions(conn: &rusqlite::Connection, graph: &Graph) -> Result<(), Box<dyn Error>> {
        let mut stmt = conn.prepare_cached(
            "INSERT INTO definitions (id, name_id, definition_type, document_id, data) VALUES (?, ?, ?, ?, ?)",
        )?;

        for (definition_id, definition) in graph.definitions() {
            let name_id = graph.definition_to_name()[definition_id];
            let uri_id = graph.definition_to_uri()[definition_id];
            let definition_type = definition.type_id();

            let data = rmp_serde::to_vec(definition).expect("Serializing definitions should always succeed");

            stmt.execute(params![
                &definition_id.to_string(),
                &name_id.to_string(),
                &definition_type.to_string(),
                &uri_id.to_string(),
                data,
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
        context.index_uri("file:///foo.rb", "module Foo; end");

        let mut db = Db::new();
        db.set_db_path(String::from("file:batch_insert_graph_test?mode=memory&cache=shared"));
        let connection = db.get_connection().unwrap();
        assert!(db.save_full_graph(&context.graph).is_ok());

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

        let mut stmt = connection.prepare(query).unwrap();
        let mut data = stmt
            .query_map((), |row| {
                Ok((
                    row.get::<_, String>(0).unwrap(),
                    row.get::<_, String>(1).unwrap(),
                    row.get::<_, String>(2).unwrap(),
                    row.get::<_, String>(3).unwrap(),
                    row.get::<_, u8>(4).unwrap(),
                    row.get::<_, String>(5).unwrap(),
                    row.get::<_, Vec<u8>>(6).unwrap(),
                    row.get::<_, String>(7).unwrap(),
                    row.get::<_, String>(8).unwrap(),
                ))
            })
            .unwrap()
            .collect::<Vec<_>>();

        let first = data.pop().unwrap();
        let (
            name_id,
            name,
            definition_id,
            definition_name_id,
            definition_type,
            definition_document_id,
            definition_data,
            document_id,
            document_uri,
        ) = first.unwrap();

        let definition = rmp_serde::from_slice::<crate::model::definitions::Definition>(&definition_data).unwrap();

        // Verify that the data we saved matches what is expected from the graph
        assert_eq!(name_id, definition_name_id);
        assert_eq!(document_id, definition_document_id);
        assert_eq!(name, String::from("Foo"));
        assert_eq!(0, definition.start());
        assert_eq!(15, definition.end());
        assert_eq!(definition_type, 1);
        assert_eq!(document_uri, String::from("file:///foo.rb"));
        assert!(!definition_id.is_empty());
    }
}
