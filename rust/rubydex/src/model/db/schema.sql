-- SQLite schema for caching the Rubydex analysis and allowing for partially loading data
--
-- The database keeps the graph with a sparse dependency approach, as opposed to the in-memory fine grained
-- relationships that we maintain for optimal processing. It's challenging to re-trace every piece of global data
-- starting from a document, so we cache all of the data reacheable from a document in a serialized format.
--
-- This means we may load a bit more data than necessary, but managing the database becomes significantly easier. Note
-- that loading a single document may trigger loading other documents if there are dependencies in those. For example,
-- imagine a class `Foo` defined in two different files. Both contribute to the same declaration and so one file depends
-- on the other and cannot be loaded separately.
--
-- The database is also used to minimize the amount of work necessary during boot. We compare the current state of the
-- workspace documents with the ones stored in the database using the content hash. That way, we can skip re-processing
-- documents that haven't changed, saving time in both indexing and resolution.

CREATE TABLE IF NOT EXISTS documents (
    -- The UriId for this document, which is the hashed URI
    id TEXT PRIMARY KEY,
    -- The content hash for the document, used to compare with the current state of the codebase
    content_hash INTEGER NOT NULL,
    -- The serialized vector of UriIds for all documents that must be loaded in conjunction with this one
    dependent_documents BLOB NOT NULL,
    -- The serialized data for the document and all reachable data (declarations, names, strings, definitions)
    data BLOB NOT NULL
);
