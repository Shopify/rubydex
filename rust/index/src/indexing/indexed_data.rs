use std::collections::HashMap;

use crate::model::{
    definitions::Definition,
    ids::{DeclarationId, UriId},
};

// A view of a declaration that may be incomplete since it is indexer specific and not global. This struct contains
// everything we discovered about a given declaration inside a single indexing thread
pub struct PartialDeclaration {
    declaration_id: DeclarationId,
    definitions: Vec<Definition>,
}

impl PartialDeclaration {
    #[must_use]
    pub fn new(declaration_id: DeclarationId) -> Self {
        Self {
            declaration_id,
            definitions: Vec::new(),
        }
    }

    #[must_use]
    pub fn into(self) -> (DeclarationId, Vec<Definition>) {
        (self.declaration_id, self.definitions)
    }

    pub fn add_definition(&mut self, definition: Definition) {
        self.definitions.push(definition);
    }
}

// A collection of declarations specific for this indexer. This is not the global representation of declarations, just a
// list of all definitions discovered that will be merged into the global index. It helps us avoid having to hash fully
// qualified names into DeclarationIds multiple times.
//
// This is similar to a temporary global index instance, but we prioritize performance since we will throw away this
// state once it gets merged globally
pub struct IndexingThreadData {
    declarations: HashMap<String, PartialDeclaration>,
    uris: HashMap<UriId, String>,
}

impl IndexingThreadData {
    #[must_use]
    pub fn new() -> Self {
        Self {
            declarations: HashMap::new(),
            uris: HashMap::new(),
        }
    }

    pub fn add_uri(&mut self, uri: String) -> UriId {
        let uri_id = UriId::new(&uri);
        self.uris.insert(uri_id, uri);
        uri_id
    }

    pub fn add_definition(&mut self, name: String, definition: Definition) {
        self.declarations
            .entry(name)
            .or_insert_with_key(|name| {
                let id = DeclarationId::new(name);
                PartialDeclaration::new(id)
            })
            .add_definition(definition);
    }

    // Returns all the owned data, so that we can merge it into the global index
    #[must_use]
    pub fn into_parts(self) -> (HashMap<String, PartialDeclaration>, HashMap<UriId, String>) {
        (self.declarations, self.uris)
    }
}

impl Default for IndexingThreadData {
    fn default() -> Self {
        Self::new()
    }
}
