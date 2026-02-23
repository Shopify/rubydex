use std::fmt;

use crate::model::{
    declaration::{Declaration, Namespace},
    graph::{BASIC_OBJECT_ID, Graph, OBJECT_ID},
    ids::DeclarationId,
};

#[derive(Debug, PartialEq, Eq)]
pub enum IntegrityErrorKind {
    /// A declaration's owner is not a namespace (module, class, or singleton class)
    OwnerIsNotNamespace,
    /// A declaration's owner does not exist in the graph
    OwnerDoesNotExist,
    /// A singleton class chain never resolves to a non-singleton namespace
    SingletonClassChainDoesNotTerminate,
    /// A non-root declaration unexpectedly owns itself
    UnexpectedSelfOwnership,
}

/// An integrity error found during graph validation
#[derive(Debug, PartialEq, Eq)]
pub struct IntegrityError {
    kind: IntegrityErrorKind,
    declaration_name: String,
    uris: Vec<String>,
}

impl fmt::Display for IntegrityError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let message = match self.kind {
            IntegrityErrorKind::OwnerIsNotNamespace => {
                format!("Declaration `{}` is owned by a non-namespace", self.declaration_name)
            }
            IntegrityErrorKind::OwnerDoesNotExist => {
                format!(
                    "Declaration `{}` has an owner that does not exist in the graph",
                    self.declaration_name
                )
            }
            IntegrityErrorKind::SingletonClassChainDoesNotTerminate => {
                format!(
                    "Singleton class `{}` does not eventually attach to a non-singleton namespace",
                    self.declaration_name
                )
            }
            IntegrityErrorKind::UnexpectedSelfOwnership => {
                format!("Declaration `{}` unexpectedly owns itself", self.declaration_name)
            }
        };

        write!(f, "{message}. Defined in: {}", self.uris.join(", "))
    }
}

impl std::error::Error for IntegrityError {}

/// Checks the integrity of the graph data
#[must_use]
pub fn check_integrity(graph: &Graph) -> Vec<IntegrityError> {
    let mut errors = Vec::new();
    let self_owners = [*OBJECT_ID, *BASIC_OBJECT_ID];

    for (id, declaration) in graph.declarations() {
        let owner_id = declaration.owner_id();

        // Check for constants that own themselves. Only `Object` and `BasicObject` own themselves and no other constant
        if *id == *owner_id {
            if self_owners.contains(id) {
                continue;
            }
            errors.push(IntegrityError {
                kind: IntegrityErrorKind::UnexpectedSelfOwnership,
                declaration_name: declaration.name().to_string(),
                uris: collect_uris(graph, declaration),
            });
            continue;
        }

        // Check that the owner exists
        let Some(owner) = graph.declarations().get(owner_id) else {
            errors.push(IntegrityError {
                kind: IntegrityErrorKind::OwnerDoesNotExist,
                declaration_name: declaration.name().to_string(),
                uris: collect_uris(graph, declaration),
            });
            continue;
        };

        // Check that the owner is a namespace
        if owner.as_namespace().is_none() {
            errors.push(IntegrityError {
                kind: IntegrityErrorKind::OwnerIsNotNamespace,
                declaration_name: declaration.name().to_string(),
                uris: collect_uris(graph, declaration),
            });
            continue;
        }

        // Check singleton class chain termination
        if let Declaration::Namespace(Namespace::SingletonClass(_)) = declaration
            && !singleton_chain_terminates(graph, *owner_id)
        {
            errors.push(IntegrityError {
                kind: IntegrityErrorKind::SingletonClassChainDoesNotTerminate,
                declaration_name: declaration.name().to_string(),
                uris: collect_uris(graph, declaration),
            });
        }
    }

    errors
}

/// Collects the URIs where a declaration is defined, sorted and deduplicated
fn collect_uris(graph: &Graph, declaration: &Declaration) -> Vec<String> {
    declaration
        .definitions()
        .iter()
        .map(|def_id| {
            let definition = graph.definitions().get(def_id).unwrap();
            let document = graph.documents().get(definition.uri_id()).unwrap();
            document.uri().to_string()
        })
        .collect()
}

/// Walks the singleton class chain to verify that it eventually finds a module or class as its attached object
fn singleton_chain_terminates(graph: &Graph, start_owner_id: DeclarationId) -> bool {
    const MAX_SINGLETON_DEPTH: usize = 128;
    let mut current_id = start_owner_id;

    for _ in 0..MAX_SINGLETON_DEPTH {
        let Some(current) = graph.declarations().get(&current_id) else {
            return false;
        };

        match current {
            Declaration::Namespace(Namespace::SingletonClass(_)) => {
                current_id = *current.owner_id();
            }
            Declaration::Namespace(_) => return true,
            _ => return false,
        }
    }

    false
}
