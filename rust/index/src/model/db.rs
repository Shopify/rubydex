use rusqlite::Connection;
use std::error::Error;

const SCHEMA_VERSION: i32 = 1;

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
        let tx = conn.transaction()?;
        tx.execute_batch(
            "
            PRAGMA journal_mode = WAL;
            PRAGMA synchronous = NORMAL;
            PRAGMA foreign_keys = ON;
        ",
        )?;

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

    /// # Errors
    /// Will return an Error if we fail to establish or set a connection
    pub fn get_connection(&self) -> Result<Connection, Box<dyn Error>> {
        let conn = match &self.connection_type {
            ConnectionType::Path(path) => {
                let conn = Connection::open(path)?;
                // Check if schema needs to be loaded using user_version pragma.
                // The default value for user_version is 0.
                let current_version: i32 = conn.query_row("PRAGMA user_version", [], |row| row.get(0))?;
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

        Ok(conn)
    }
}
