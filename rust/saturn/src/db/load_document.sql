SELECT
    declarations.id,
    declarations.name,
    definitions.id,
    definitions.data
FROM documents
JOIN definitions ON documents.id = definitions.document_id
JOIN declarations ON declarations.id = definitions.declaration_id
WHERE documents.id = ?
