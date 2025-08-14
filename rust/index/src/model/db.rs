use std::collections::HashMap;
use std::sync::OnceLock;

use rusqlite::Connection;

use crate::model::{
    definitions::Definition,
    graph::Graph,
    ids::{DefinitionId, NameId, UriId},
};

#[derive(Debug)]
pub struct Db {
    path: String,
    conn: OnceLock<Connection>,
}

impl Db {
    /// # Errors
    /// Will return `Err` if  there is a failure to load the schema file or if there is
    /// an error openining or using the DB connection.
    pub fn new(path: &str) -> rusqlite::Result<Self> {
        Ok(Self {
            path: path.to_string(),
            conn: OnceLock::new(),
        })
    }

    /// This method should return a new memory DB that can be used for tests
    ///
    /// # Errors
    /// Will return `Err` if  there is a failure to open the DB memory connection.
    pub fn new_memory_db() -> rusqlite::Result<Self> {
        Ok(Self {
            path: ":memory:".to_string(),
            conn: OnceLock::new(),
        })
    }

    /// # Errors
    /// Will return an Error if we fail to establish or set a connection
    pub fn get_connection(&self) -> Result<&Connection, Box<dyn std::error::Error>> {
        if let Some(conn) = self.conn.get() {
            return Ok(conn);
        }

        let conn = if self.path == ":memory:" {
            Connection::open_in_memory()?
        } else {
            Connection::open(&self.path)?
        };

        let schema = std::fs::read_to_string(
            std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
                .join("src/db")
                .join("schema.sql"),
        )?;
        conn.execute_batch(&schema)?;

        match self.conn.set(conn) {
            Ok(()) => self
                .conn
                .get()
                .ok_or_else(|| "Database connection was not properly initialized".into()),
            Err(_) => Err("Failed to initialize database connection".into()),
        }
    }

    /// # Errors
    /// Will return `Err` if there is a failure when performing the DB insert.
    pub fn batch_insert_uris(&self, tx: &rusqlite::Transaction, uris: &HashMap<UriId, String>) -> rusqlite::Result<()> {
        let mut stmt = tx.prepare_cached("INSERT OR REPLACE INTO documents (id, uri) VALUES (?, ?)")?;
        for (uri_id, path) in uris {
            stmt.execute(rusqlite::params![uri_id.to_string(), path])?;
        }
        Ok(())
    }

    pub fn batch_insert_names(
        &self,
        tx: &rusqlite::Transaction,
        names: &HashMap<NameId, String>,
    ) -> rusqlite::Result<()> {
        let mut stmt = tx.prepare_cached("INSERT OR REPLACE INTO names (id, name) VALUES (?, ?)")?;

        for (name_id, name) in names {
            stmt.execute(rusqlite::params![name_id.to_string(), name])?;
        }

        Ok(())
    }

    /// # Errors
    /// Will return `Err` if there is a failure when performing the DB insert.
    pub fn batch_insert_definitions(
        &self,
        tx: &rusqlite::Transaction,
        definitions: &HashMap<DefinitionId, Definition>,
        definition_to_uri: &HashMap<DefinitionId, UriId>,
        definition_to_name: &HashMap<DefinitionId, NameId>,
    ) -> rusqlite::Result<()> {
        let mut stmt = tx.prepare_cached(
                "INSERT INTO definitions (id, name_id, definition_type, document_id, start_offset, end_offset) VALUES (?, ?, ?, ?, ?, ?)"
            )?;

        for (definition_id, definition) in definitions {
            let definition_type = definition.definition_type_value();

            let (start_offset, end_offset) = match definition {
                Definition::Class(class) => (class.offset.start_offset, class.offset.end_offset),
                Definition::Module(module) => (module.offset.start_offset, module.offset.end_offset),
            };
            let uri_id = definition_to_uri[definition_id];
            let name_id = definition_to_name[definition_id];

            stmt.execute(rusqlite::params![
                definition_id.to_string(),
                name_id.to_string(),
                definition_type,
                uri_id.to_string(),
                i64::from(start_offset),
                i64::from(end_offset)
            ])?;
        }
        Ok(())
    }

    /// Dumps all graph data to the database in a single transaction
    /// # Errors
    /// Will return `Err` if there is a failure when performing the DB operations.
    pub fn dump_graph(&self, graph: &Graph) -> Result<(), Box<dyn std::error::Error>> {
        let conn = self.get_connection()?;
        let tx = conn.unchecked_transaction()?;

        self.batch_insert_uris(&tx, &graph.uri_pool)?;
        self.batch_insert_names(&tx, &graph.names)?;
        self.batch_insert_definitions(&tx, &graph.definitions, &graph.definition_to_uri, &graph.definition_to_name)?;

        Ok(tx.commit()?)
    }
}
