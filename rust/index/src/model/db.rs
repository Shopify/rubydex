use crate::model::definitions::Definition;
use crate::model::ids::{DefinitionId, NameId, UriId};
use rusqlite::Connection;
use std::collections::HashMap;
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
        // Set Pragmas first
        conn.execute_batch(
            "
            PRAGMA journal_mode = WAL;
            PRAGMA synchronous = NORMAL;
            PRAGMA foreign_keys = ON;
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

    /// Performs batch insert of documents (URIs) to the database
    fn batch_insert_documents(conn: &rusqlite::Connection, documents: &HashMap<UriId, String>) -> Result<(), Box<dyn Error>> {
        let mut stmt = conn.prepare_cached("INSERT OR REPLACE INTO documents (id, uri) VALUES (?, ?)")?;

        for (uri_id, uri) in documents {
            stmt.execute([&uri_id.to_string(), uri])?;
        }

        Ok(())
    }

    /// Performs batch insert of names to the database
    fn batch_insert_names(conn: &rusqlite::Connection, names: &HashMap<NameId, String>) -> Result<(), Box<dyn Error>> {
        let mut stmt = conn.prepare_cached("INSERT OR REPLACE INTO names (id, name) VALUES (?, ?)")?;

        for (name_id, name) in names {
            stmt.execute([&name_id.to_string(), name])?;
        }

        Ok(())
    }

    /// Performs batch insert of definitions to the database
    fn batch_insert_definitions(
        conn: &rusqlite::Connection,
        definitions: &HashMap<DefinitionId, Definition>,
        definition_to_name: &HashMap<DefinitionId, NameId>,
        definition_to_uri: &HashMap<DefinitionId, UriId>,
    ) -> Result<(), Box<dyn Error>> {
        let mut stmt = conn.prepare_cached(
            "INSERT OR REPLACE INTO definitions (id, name_id, definition_type, document_id, start_offset, end_offset) VALUES (?, ?, ?, ?, ?, ?)"
        )?;

        for (definition_id, definition) in definitions {
            let name_id = definition_to_name[definition_id];
            let uri_id = definition_to_uri[definition_id];
            let definition_type = definition.type_id();

            stmt.execute([
                &definition_id.to_string(),
                &name_id.to_string(),
                &definition_type.to_string(),
                &uri_id.to_string(),
                &definition.start_offset().to_string(),
                &definition.end_offset().to_string(),
            ])?;
        }

        Ok(())
    }

    /// # Errors
    /// Returns an Error if writing data to DB is unsuccesful.
    pub fn batch_insert_graph_data(
        &self,
        documents: &HashMap<UriId, String>,
        names: &HashMap<NameId, String>,
        definitions: &HashMap<DefinitionId, Definition>,
        definition_to_name: &HashMap<DefinitionId, NameId>,
        definition_to_uri: &HashMap<DefinitionId, UriId>,
    ) -> Result<(), Box<dyn Error>> {
        let mut conn = self.get_connection()?;
        let tx = conn.transaction()?;

        Self::batch_insert_documents(&tx, documents)?;
        Self::batch_insert_names(&tx, names)?;
        Self::batch_insert_definitions(&tx, definitions, definition_to_name, definition_to_uri)?;

        tx.commit()?;
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::model::definitions::{ClassDefinition, Definition, ModuleDefinition};
    use crate::offset::Offset;

    #[test]
    fn batch_insert_documents() {
        let db = Db::new();

        let uri1_id = UriId::new("file:///foo.rb");
        let uri2_id = UriId::new("file:///bar.rb");

        let mut documents = HashMap::new();
        documents.insert(uri1_id, "file:///foo.rb".to_string());
        documents.insert(uri2_id, "file:///bar.rb".to_string());

        // Test batch insert
        let conn = db.get_connection().unwrap();
        assert!(Db::batch_insert_documents(&conn, &documents).is_ok());

        // Verify row count
        let count: i64 = conn
            .query_row("SELECT COUNT(*) FROM documents", [], |row| row.get(0))
            .unwrap();
        assert_eq!(count, 2);

        // Verify column values for first document
        let (id, uri): (String, String) = conn
            .query_row(
                "SELECT id, uri FROM documents WHERE uri = ?",
                ["file:///foo.rb"],
                |row| Ok((row.get(0)?, row.get(1)?)),
            )
            .unwrap();
        assert_eq!(id, uri1_id.to_string());
        assert_eq!(uri, "file:///foo.rb");

        // Verify column values for second document
        let (id, uri): (String, String) = conn
            .query_row(
                "SELECT id, uri FROM documents WHERE uri = ?",
                ["file:///bar.rb"],
                |row| Ok((row.get(0)?, row.get(1)?)),
            )
            .unwrap();
        assert_eq!(id, uri2_id.to_string());
        assert_eq!(uri, "file:///bar.rb");
    }

    #[test]
    fn batch_insert_names() {
        let db = Db::new();

        let name1_id = NameId::new("Foo");
        let name2_id = NameId::new("Bar");

        let mut names = HashMap::new();
        names.insert(name1_id, "Foo".to_string());
        names.insert(name2_id, "Bar".to_string());

        // Test batch insert
        let conn = db.get_connection().unwrap();
        assert!(Db::batch_insert_names(&conn, &names).is_ok());

        // Verify row count
        let count: i64 = conn
            .query_row("SELECT COUNT(*) FROM names", [], |row| row.get(0))
            .unwrap();
        assert_eq!(count, 2);

        // Verify column values for first name
        let (id, name): (String, String) = conn
            .query_row("SELECT id, name FROM names WHERE name = ?", ["Foo"], |row| {
                Ok((row.get(0)?, row.get(1)?))
            })
            .unwrap();
        assert_eq!(id, name1_id.to_string());
        assert_eq!(name, "Foo");

        // Verify column values for second name
        let (id, name): (String, String) = conn
            .query_row("SELECT id, name FROM names WHERE name = ?", ["Bar"], |row| {
                Ok((row.get(0)?, row.get(1)?))
            })
            .unwrap();
        assert_eq!(id, name2_id.to_string());
        assert_eq!(name, "Bar");
    }

    #[test]
    fn batch_insert_definitions() {
        let db = Db::new();

        // First insert required documents and names
        let uri_id = UriId::new("file:///foo.rb");
        let name_id = NameId::new("Foo");

        let conn = db.get_connection().unwrap();
        
        let mut documents = HashMap::new();
        documents.insert(uri_id, "file:///foo.rb".to_string());
        Db::batch_insert_documents(&conn, &documents).unwrap();
        
        let mut names = HashMap::new();
        names.insert(name_id, "Foo".to_string());
        Db::batch_insert_names(&conn, &names).unwrap();

        // Create definitions
        let def_id = DefinitionId::new(uri_id, 0);
        let class_def = Definition::Class(Box::new(ClassDefinition::new(Offset::new(0, 10))));

        let mut definitions = HashMap::new();
        definitions.insert(def_id, class_def);
        
        let mut definition_to_name = HashMap::new();
        definition_to_name.insert(def_id, name_id);
        
        let mut definition_to_uri = HashMap::new();
        definition_to_uri.insert(def_id, uri_id);

        // Test batch insert
        assert!(Db::batch_insert_definitions(&conn, &definitions, &definition_to_name, &definition_to_uri).is_ok());

        // Verify row count
        let count: i64 = conn
            .query_row("SELECT COUNT(*) FROM definitions", [], |row| row.get(0))
            .unwrap();
        assert_eq!(count, 1);

        // Verify all column values for the definition

        let (id, name_id_result, def_type, document_id, start_offset, end_offset): (String, String, i64, String, i64, i64) = conn
            .query_row("SELECT id, name_id, definition_type, document_id, start_offset, end_offset FROM definitions WHERE id = ?",
            [&def_id.to_string()], 
            |row| {
                Ok((
                    row.get(0)?,
                    row.get(1)?,
                    row.get(2)?,
                    row.get(3)?,
                    row.get(4)?,
                    row.get(5)?,
                ))
            })
           .unwrap();

        assert_eq!(id, def_id.to_string());
        assert_eq!(name_id_result, name_id.to_string());
        assert_eq!(def_type, 1); // 1 = Class
        assert_eq!(document_id, uri_id.to_string());
        assert_eq!(start_offset, 0);
        assert_eq!(end_offset, 10);
    }

    #[test]
    fn batch_insert_graph_data_complete() {
        let mut db = Db::new();
        db.set_db_path(String::from("file:batch_insert_graph_test?mode=memory&cache=shared"));

        // memory DB connection will remain alive as long as one reference to it
        // remains active
        let conn = db.get_connection().unwrap();

        let uri_id = UriId::new("file:///foo.rb");
        let name_id = NameId::new("Foo");
        let def_id = DefinitionId::new(uri_id, 0);
        let module_def = Definition::Module(Box::new(ModuleDefinition::new(Offset::new(0, 15))));

        let mut documents = HashMap::new();
        documents.insert(uri_id, "file:///foo.rb".to_string());
        
        let mut names = HashMap::new();
        names.insert(name_id, "Foo".to_string());
        
        let mut definitions = HashMap::new();
        definitions.insert(def_id, module_def);
        
        let mut definition_to_name = HashMap::new();
        definition_to_name.insert(def_id, name_id);
        
        let mut definition_to_uri = HashMap::new();
        definition_to_uri.insert(def_id, uri_id);

        // Test complete batch insert
        assert!(db.batch_insert_graph_data(&documents, &names, &definitions, &definition_to_name, &definition_to_uri).is_ok());

        let count: i64 = conn
            .query_row("SELECT COUNT(*) FROM documents", [], |row| row.get(0))
            .unwrap();
        assert_eq!(count, 1);

        let count: i64 = conn
            .query_row("SELECT COUNT(*) FROM names", [], |row| row.get(0))
            .unwrap();

        assert_eq!(count, 1);

        let count: i64 = conn
            .query_row("SELECT COUNT(*) FROM definitions", [], |row| row.get(0))
            .unwrap();
        assert_eq!(count, 1);
    }
}
