use std::collections::HashMap;
use std::collections::hash_map::Entry;

use crate::model::ids::{DeclarationId, UriId};
use crate::{indexing::ruby_indexer::DefinitionCollection, model::declaration::Declaration};

// The `Index` is the global graph representation of the entire Ruby codebase. It contains all declarations and their
// relationships
pub struct Index {
    declarations: HashMap<DeclarationId, Declaration>,
    uri_pool: HashMap<UriId, String>,
}

impl Index {
    #[must_use]
    pub fn new() -> Self {
        Self {
            declarations: HashMap::new(),
            uri_pool: HashMap::new(),
        }
    }

    #[must_use]
    pub fn intern_uri(&mut self, uri: String) -> UriId {
        let uri_id = UriId::new(&uri);
        self.uri_pool.insert(uri_id, uri);
        uri_id
    }

    pub fn merge_definitions(&mut self, definitions: HashMap<String, DefinitionCollection>) {
        for (name, defs) in definitions {
            let incoming = defs.definitions;

            match self.declarations.entry(defs.node_id) {
                Entry::Occupied(mut _occ) => {
                    // todo synchronize modifications to the existing declarations
                }
                Entry::Vacant(vac) => {
                    vac.insert(Declaration::new(name, Some(incoming)));
                }
            }
        }
    }
}

impl Default for Index {
    fn default() -> Self {
        Self::new()
    }
}
