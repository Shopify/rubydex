-- SQLite schema for the index database

CREATE TABLE IF NOT EXISTS documents (
    -- Queryable fields
    id INTEGER PRIMARY KEY,  -- Hashed document ID
    content_hash INTEGER NOT NULL,
    -- Serialized struct
    data BLOB NOT NULL
);

CREATE TABLE IF NOT EXISTS declarations (
    -- Queryable fields
    id INTEGER PRIMARY KEY,  -- Hashed declaration ID
    name TEXT NOT NULL
);

CREATE TABLE IF NOT EXISTS definitions (
    -- Queryable fields
    id INTEGER PRIMARY KEY,  -- Hashed definition ID
    declaration_id INTEGER NOT NULL,
    document_id INTEGER NOT NULL,
    -- Serialized struct
    data BLOB NOT NULL
);

CREATE INDEX IF NOT EXISTS names_name_index ON declarations (name);
