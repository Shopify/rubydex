use rusqlite::Connection;

const SCHEMA_VERSION: i32 = 1;

#[derive(Default, Debug)]
pub struct Db {
    path: String,
}

impl Db {
    #[must_use]
    pub fn new(path: &str) -> Self {
        Self { path: path.to_string() }
    }

    /// This method should return a new memory DB that can be used for local indexing or tests.
    #[must_use]
    pub fn new_memory_db() -> Self {
        Self {
            path: ":memory:".to_string(),
        }
    }

    /// # Errors
    /// Will return an Error if we fail to establish or set a connection
    pub fn get_connection(&self) -> Result<Connection, Box<dyn std::error::Error>> {
        let conn = if self.path == ":memory:" {
            Connection::open_in_memory()?
        } else {
            Connection::open(&self.path)?
        };

        // Configure SQLite pragmas for performance and consistency
        conn.execute_batch(
            "
            PRAGMA journal_mode = WAL;
            PRAGMA synchronous = NORMAL;
            PRAGMA foreign_keys = ON;
        ",
        )?;

        // Check if schema needs to be loaded using user_version pragma.
        // The default value for user_version is 0.
        let current_version: i32 = conn.query_row("PRAGMA user_version", [], |row| row.get(0))?;

        if current_version < SCHEMA_VERSION {
            let schema = std::fs::read_to_string(
                std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
                    .join("src/db")
                    .join("schema.sql"),
            )?;
            conn.execute_batch(&schema)?;

            // Set user_version to indicate schema has been loaded
            conn.execute(&format!("PRAGMA user_version = {SCHEMA_VERSION}"), [])?;
        }
        Ok(conn)
    }
}
