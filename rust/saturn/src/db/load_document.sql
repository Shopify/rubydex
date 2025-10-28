SELECT
    documents.data,
    declarations.id,
    declarations.name,
    definitions.id,
    definitions.data
FROM documents
LEFT JOIN definitions ON documents.id = definitions.document_id
LEFT JOIN declarations ON declarations.id = definitions.declaration_id
WHERE documents.id = ?
