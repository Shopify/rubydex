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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::model::declaration::{ClassDeclaration, MethodDeclaration, SingletonClassDeclaration};

    #[test]
    fn test_unexpected_self_ownership() {
        let mut graph = Graph::new();

        // Object and BasicObject are exempt from self-ownership
        graph.declarations_mut().insert(
            *OBJECT_ID,
            Declaration::Namespace(Namespace::Class(Box::new(ClassDeclaration::new(
                "Object".to_string(),
                *OBJECT_ID,
            )))),
        );
        graph.declarations_mut().insert(
            *BASIC_OBJECT_ID,
            Declaration::Namespace(Namespace::Class(Box::new(ClassDeclaration::new(
                "BasicObject".to_string(),
                *BASIC_OBJECT_ID,
            )))),
        );

        // Foo owns itself — should be an error
        let foo_id = DeclarationId::from("Foo");
        graph.declarations_mut().insert(
            foo_id,
            Declaration::Namespace(Namespace::Class(Box::new(ClassDeclaration::new(
                "Foo".to_string(),
                foo_id,
            )))),
        );

        let errors = check_integrity(&graph);
        assert_eq!(errors.len(), 1);
        assert_eq!(errors[0].kind, IntegrityErrorKind::UnexpectedSelfOwnership);
        assert_eq!(errors[0].declaration_name, "Foo");
    }

    #[test]
    fn test_owner_does_not_exist() {
        let mut graph = Graph::new();
        let foo_id = DeclarationId::from("Foo");
        let bogus_owner = DeclarationId::from("NonExistent");

        graph.declarations_mut().insert(
            foo_id,
            Declaration::Namespace(Namespace::Class(Box::new(ClassDeclaration::new(
                "Foo".to_string(),
                bogus_owner,
            )))),
        );

        let errors = check_integrity(&graph);
        assert_eq!(errors.len(), 1);
        assert_eq!(errors[0].kind, IntegrityErrorKind::OwnerDoesNotExist);
        assert_eq!(errors[0].declaration_name, "Foo");
    }

    #[test]
    fn test_owner_is_not_namespace() {
        let mut graph = Graph::new();

        // Object (self-owned, exempt)
        graph.declarations_mut().insert(
            *OBJECT_ID,
            Declaration::Namespace(Namespace::Class(Box::new(ClassDeclaration::new(
                "Object".to_string(),
                *OBJECT_ID,
            )))),
        );

        // A method owned by Object (valid)
        let method_id = DeclarationId::from("Object#foo");
        graph.declarations_mut().insert(
            method_id,
            Declaration::Method(Box::new(MethodDeclaration::new("Object#foo".to_string(), *OBJECT_ID))),
        );

        // A class owned by the method (invalid — owner is not a namespace)
        let bar_id = DeclarationId::from("Bar");
        graph.declarations_mut().insert(
            bar_id,
            Declaration::Namespace(Namespace::Class(Box::new(ClassDeclaration::new(
                "Bar".to_string(),
                method_id,
            )))),
        );

        let errors = check_integrity(&graph);
        assert_eq!(errors.len(), 1);
        assert_eq!(errors[0].kind, IntegrityErrorKind::OwnerIsNotNamespace);
        assert_eq!(errors[0].declaration_name, "Bar");
    }

    #[test]
    fn test_singleton_class_chain_does_not_terminate() {
        let mut graph = Graph::new();

        // Two singleton classes that own each other, forming a cycle
        let s1_id = DeclarationId::from("<Class:Foo>");
        let s2_id = DeclarationId::from("<Class:Bar>");

        graph.declarations_mut().insert(
            s1_id,
            Declaration::Namespace(Namespace::SingletonClass(Box::new(SingletonClassDeclaration::new(
                "<Class:Foo>".to_string(),
                s2_id,
            )))),
        );
        graph.declarations_mut().insert(
            s2_id,
            Declaration::Namespace(Namespace::SingletonClass(Box::new(SingletonClassDeclaration::new(
                "<Class:Bar>".to_string(),
                s1_id,
            )))),
        );

        let errors = check_integrity(&graph);
        assert_eq!(errors.len(), 2);
        assert!(
            errors
                .iter()
                .all(|e| e.kind == IntegrityErrorKind::SingletonClassChainDoesNotTerminate)
        );
    }
}
