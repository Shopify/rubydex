-- SQLite schema for the index database
-- Contains tables and indexes for storing code declarations and their relationships

-- Table for storing documents
CREATE TABLE IF NOT EXISTS documents (
    id INTEGER PRIMARY KEY,  -- Blake3 hash converted to hex
    uri TEXT NOT NULL UNIQUE
);

-- Table for storing code declarations (classes, modules, methods, etc.)
CREATE TABLE IF NOT EXISTS names (
    id INTEGER PRIMARY KEY,  -- Blake3 hash converted to hex
    name TEXT NOT NULL
);

-- Table for storing definitions that make up declarations
CREATE TABLE IF NOT EXISTS definitions (
    id INTEGER PRIMARY KEY,  -- Blake3 hash converted to hex
    name_id INTEGER NOT NULL REFERENCES names(id), -- References names.id
    document_id INTEGER NOT NULL REFERENCES documents(id), -- References documents.id
    data BLOB NOT NULL, -- Serialized definition data
    FOREIGN KEY (name_id) REFERENCES names (id) ON DELETE CASCADE,
    FOREIGN KEY (document_id) REFERENCES documents (id) ON DELETE CASCADE
);

-- Indexes for fast lookups
CREATE INDEX IF NOT EXISTS names_name_index ON names (name);
CREATE INDEX IF NOT EXISTS definitions_name_id_index ON definitions (name_id);
CREATE INDEX IF NOT EXISTS definitions_document_id_index ON definitions (document_id);
