-- SQLite schema for the index database
-- Contains tables and indexes for storing code declarations and their relationships

-- Table for storing documents
CREATE TABLE IF NOT EXISTS documents (
    id TEXT PRIMARY KEY,  -- Blake3 hash converted to hex
    uri TEXT NOT NULL UNIQUE
);

-- Table for storing code declarations (classes, modules, methods, etc.)
CREATE TABLE IF NOT EXISTS names (
    id TEXT PRIMARY KEY,  -- Blake3 hash converted to hex
    name TEXT NOT NULL
);

-- Table for storing definitions that make up declarations
CREATE TABLE IF NOT EXISTS definitions (
    id TEXT PRIMARY KEY,  -- Blake3 hash converted to hex
    name_id TEXT NOT NULL,  -- References names.id
    definition_type INTEGER NOT NULL CHECK(definition_type IN (1, 2)), -- 1=Class, 2=Module
    document_id TEXT NOT NULL,          -- References documents.id
    start_offset INTEGER NOT NULL,
    end_offset INTEGER NOT NULL,
    FOREIGN KEY (name_id) REFERENCES names (id) ON DELETE CASCADE,
    FOREIGN KEY (document_id) REFERENCES documents (id) ON DELETE CASCADE
);

-- Indexes for fast lookups
CREATE INDEX IF NOT EXISTS names_name_index ON names (name);
CREATE INDEX IF NOT EXISTS definitions_name_id_index ON definitions (name_id);
CREATE INDEX IF NOT EXISTS definitions_document_id_index ON definitions (document_id);
CREATE INDEX IF NOT EXISTS definitions_type_index ON definitions (definition_type);
