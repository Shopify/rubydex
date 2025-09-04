-- SQLite schema for the index database
-- Contains tables and indexes for storing code declarations and their relationships

-- Table for storing documents
CREATE TABLE IF NOT EXISTS documents (
    id INTEGER PRIMARY KEY,  -- Hashed document ID
    uri TEXT NOT NULL UNIQUE,
    content_hash INTEGER NOT NULL
);

-- Table for storing code declarations (classes, modules, methods, etc.)
CREATE TABLE IF NOT EXISTS declarations (
    id INTEGER PRIMARY KEY,  -- Hashed declaration ID
    name TEXT NOT NULL
);

-- Table for storing definitions that make up declarations
CREATE TABLE IF NOT EXISTS definitions (
    id INTEGER PRIMARY KEY,  -- Hashed definition ID
    declaration_id INTEGER NOT NULL,
    document_id INTEGER NOT NULL,
    data BLOB NOT NULL -- Serialized definition data
);

-- Indexes for fast lookups
CREATE INDEX IF NOT EXISTS names_name_index ON declarations (name);
