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

    /// Initializes a fresh database with schema and configuration
    fn initialize_database(conn: &mut Connection) -> Result<(), Box<dyn std::error::Error>> {
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
    fn create_fresh_file_database(&self) -> Result<Connection, Box<dyn std::error::Error>> {
        if std::path::Path::new(&self.path).exists() {
            std::fs::remove_file(&self.path)?;
        }
        let mut conn = Connection::open(&self.path)?;
        Self::initialize_database(&mut conn)?;
        Ok(conn)
    }

    /// # Errors
    /// Will return an Error if we fail to establish or set a connection
    pub fn get_connection(&self) -> Result<Connection, Box<dyn std::error::Error>> {
        let mut conn = if self.path == ":memory:" {
            Connection::open_in_memory()?
        } else {
            Connection::open(&self.path)?
        };

        // Check if schema needs to be loaded using user_version pragma.
        // The default value for user_version is 0.
        let current_version: i32 = conn.query_row("PRAGMA user_version", [], |row| row.get(0))?;

        if current_version < SCHEMA_VERSION {
            if self.path == ":memory:" {
                Self::initialize_database(&mut conn)?;
            } else {
                // For file-based databases, delete and recreate for clean schema
                drop(conn);
                return self.create_fresh_file_database();
            }
        }
        Ok(conn)
    }
}
