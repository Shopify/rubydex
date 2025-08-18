use rusqlite::Connection;
use std::error::Error;

const SCHEMA_VERSION: i32 = 1;

#[derive(Debug)]
enum ConnectionType {
    Path(String),
    #[allow(dead_code)]
    Memory,
}

#[derive(Debug)]
pub struct Db {
    connection_type: ConnectionType,
}

impl Db {
    #[must_use]
    pub fn new(path: &str) -> Self {
        Self {
            connection_type: ConnectionType::Path(path.to_string()),
        }
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

        let schema = std::fs::read_to_string(
            std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
                .join("src/db")
                .join("schema.sql"),
        )?;
        tx.execute_batch(&schema)?;
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
