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
    name_id INTEGER NOT NULL, -- References names.id
    document_id INTEGER NOT NULL, -- References documents.id
    data BLOB NOT NULL -- Serialized definition data
);

-- Indexes for fast lookups
CREATE INDEX IF NOT EXISTS names_name_index ON names (name);
