use std::collections::HashMap;
use std::collections::hash_map::Entry;

use crate::indexing::indexed_data::IndexingThreadData;
use crate::model::declaration::Declaration;
use crate::model::ids::{DeclarationId, UriId};

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

    pub fn merge_definitions(&mut self, indexed_data: IndexingThreadData) {
        let (declarations, uris) = indexed_data.into_parts();

        for (name, declarations) in declarations {
            let (id, definitions) = declarations.into();

            match self.declarations.entry(id) {
                Entry::Occupied(mut _occ) => {
                    // todo synchronize modifications to the existing declarations
                }
                Entry::Vacant(vac) => {
                    vac.insert(Declaration::new(name, Some(definitions)));
                }
            }
        }

        self.uri_pool.extend(uris);
    }
}

impl Default for Index {
    fn default() -> Self {
        Self::new()
    }
}
