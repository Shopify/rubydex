SELECT
    names.id,
    names.name,
    definitions.id,
    definitions.data
FROM documents
JOIN definitions ON documents.id = definitions.document_id
JOIN names ON names.id = definitions.name_id
WHERE documents.id = ?
