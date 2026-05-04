use super::normalize_indentation;
#[cfg(test)]
use crate::diagnostic::Rule;
use crate::indexing::{self, LanguageId};
use crate::model::graph::{Graph, NameDependent};
use crate::model::ids::{NameId, StringId};
use crate::resolution::Resolver;

#[derive(Default)]
pub struct GraphTest {
    graph: Graph,
}

impl GraphTest {
    #[must_use]
    pub fn new() -> Self {
        Self { graph: Graph::new() }
    }

    #[must_use]
    pub fn graph(&self) -> &Graph {
        &self.graph
    }

    #[must_use]
    pub fn into_graph(self) -> Graph {
        self.graph
    }

    /// Indexes a Ruby source
    pub fn index_uri(&mut self, uri: &str, source: &str) {
        let source = normalize_indentation(source);
        indexing::index_source(&mut self.graph, uri, &source, &LanguageId::Ruby);
    }

    /// Indexes an RBS source
    pub fn index_rbs_uri(&mut self, uri: &str, source: &str) {
        let source = normalize_indentation(source);
        indexing::index_source(&mut self.graph, uri, &source, &LanguageId::Rbs);
    }

    pub fn delete_uri(&mut self, uri: &str) {
        self.graph.delete_document(uri);
    }

    pub fn resolve(&mut self) {
        let mut resolver = Resolver::new(&mut self.graph);
        resolver.resolve();
    }

    // Name dependents helpers (shared with LocalGraphTest for assert_dependents! macro)

    /// # Panics
    ///
    /// Panics if no names match the given path.
    #[must_use]
    pub fn find_name_ids(&self, path: &str) -> Vec<NameId> {
        let (parent, name) = match path.rsplit_once("::") {
            Some((p, n)) => (Some(p), n),
            None => (None, path),
        };
        let target_str_id = StringId::from(name);
        let ids: Vec<NameId> = self
            .graph()
            .names()
            .iter()
            .filter(|(_, name_ref)| {
                if *name_ref.str() != target_str_id {
                    return false;
                }
                match parent {
                    None => name_ref.parent_scope().as_ref().is_none(),
                    Some(p) => name_ref.parent_scope().as_ref().is_some_and(|ps_id| {
                        let ps = self.graph().names().get(ps_id).unwrap();
                        *ps.str() == StringId::from(p)
                    }),
                }
            })
            .map(|(id, _)| *id)
            .collect();
        assert!(!ids.is_empty(), "could not find name `{path}`");
        ids
    }

    #[must_use]
    pub fn name_dependents_for(&self, name_id: NameId) -> Vec<NameDependent> {
        self.graph()
            .name_dependents()
            .get(&name_id)
            .cloned()
            .unwrap_or_default()
    }

    /// # Panics
    ///
    /// Panics if the name's string is not in the strings map.
    #[must_use]
    pub fn name_str(&self, name_id: &NameId) -> Option<&str> {
        self.graph()
            .names()
            .get(name_id)
            .map(|n| self.graph().strings().get(n.str()).unwrap().as_str())
    }

    /// Returns the unqualified name string for a `NameDependent`, if available.
    #[must_use]
    pub fn dependent_name_str(&self, dep: &NameDependent) -> Option<&str> {
        match dep {
            NameDependent::ChildName(id) | NameDependent::NestedName(id) => self.name_str(id),
            NameDependent::Definition(id) => self
                .graph()
                .definitions()
                .get(id)
                .and_then(|d| d.name_id())
                .and_then(|name_id| self.name_str(name_id)),
            NameDependent::Reference(id) => self
                .graph()
                .constant_references()
                .get(id)
                .and_then(|r| self.name_str(r.name_id())),
        }
    }

    /// # Panics
    ///
    /// Panics if a diagnostic points to an invalid document
    #[cfg(test)]
    #[must_use]
    pub fn format_diagnostics(&self, ignore_rules: &[Rule]) -> Vec<String> {
        let mut diagnostics: Vec<_> = self
            .graph()
            .all_diagnostics()
            .into_iter()
            .filter(|d| !ignore_rules.contains(d.rule()))
            .collect();

        diagnostics.sort_by_key(|d| {
            let uri = self.graph().documents().get(d.uri_id()).unwrap().uri();
            (uri, d.offset())
        });

        diagnostics
            .iter()
            .map(|d| {
                let document = self.graph().documents().get(d.uri_id()).unwrap();
                d.formatted(document)
            })
            .collect()
    }
}

#[cfg(test)]
#[macro_export]
macro_rules! assert_declaration_exists {
    ($context:expr, $declaration_name:expr) => {
        assert!(
            $context
                .graph()
                .declarations()
                .get(&$crate::model::ids::DeclarationId::from($declaration_name))
                .is_some(),
            "Expected declaration `{}` to exist",
            $declaration_name
        );
    };
}

#[cfg(test)]
#[macro_export]
macro_rules! assert_declaration_kind_eq {
    ($context:expr, $declaration_name:expr, $expected_kind:expr) => {
        let declaration = $context
            .graph()
            .declarations()
            .get(&$crate::model::ids::DeclarationId::from($declaration_name))
            .unwrap();
        assert_eq!(
            declaration.kind(),
            $expected_kind,
            "Expected declaration `{}` to be a {}, got {}",
            $declaration_name,
            $expected_kind,
            declaration.kind()
        );
    };
}

#[cfg(test)]
#[macro_export]
macro_rules! assert_declaration_does_not_exist {
    ($context:expr, $declaration_name:expr) => {
        assert!(
            $context
                .graph()
                .declarations()
                .get(&$crate::model::ids::DeclarationId::from($declaration_name))
                .is_none(),
            "Expected declaration `{}` to not exist",
            $declaration_name
        );
    };
}

#[cfg(test)]
#[macro_export]
macro_rules! assert_declaration_definitions_count_eq {
    ($context:expr, $declaration_name:expr, $expected_definitions:expr) => {
        let declaration = $context
            .graph()
            .declarations()
            .get(&$crate::model::ids::DeclarationId::from($declaration_name))
            .unwrap();

        assert_eq!(
            declaration.definitions().len(),
            $expected_definitions,
            "Expected exactly {} definitions for `{}`, but got {}",
            $expected_definitions,
            $declaration_name,
            declaration.definitions().len()
        );
    };
}

#[cfg(test)]
#[macro_export]
macro_rules! assert_constant_alias_target_eq {
    ($context:expr, $alias_name:expr, $target_name:expr) => {{
        let decl_id = $crate::model::ids::DeclarationId::from($alias_name);
        let target = $context
            .graph()
            .alias_targets(&decl_id)
            .and_then(|t| t.first().copied());
        assert_eq!(
            target,
            Some($crate::model::ids::DeclarationId::from($target_name)),
            "Expected alias '{}' to have primary target '{}'",
            $alias_name,
            $target_name
        );
    }};
}

#[cfg(test)]
#[macro_export]
macro_rules! assert_no_constant_alias_target {
    ($context:expr, $alias_name:expr) => {{
        let decl_id = $crate::model::ids::DeclarationId::from($alias_name);
        let targets = $context.graph().alias_targets(&decl_id).unwrap_or_default();
        assert!(
            targets.is_empty(),
            "Expected no alias target for '{}', but found {:?}",
            $alias_name,
            targets
        );
    }};
}

#[cfg(test)]
#[macro_export]
macro_rules! assert_alias_targets_contain {
    ($context:expr, $alias_name:expr, $($target_name:expr),+ $(,)?) => {{
        let decl_id = $crate::model::ids::DeclarationId::from($alias_name);
        let targets = $context.graph().alias_targets(&decl_id).unwrap_or_default();
        $(
            let expected_id = $crate::model::ids::DeclarationId::from($target_name);
            assert!(
                targets.contains(&expected_id),
                "Expected alias '{}' to contain target '{}', but targets were {:?}",
                $alias_name,
                $target_name,
                targets
            );
        )+
    }};
}

/// Asserts that a declaration has a constant reference at the specified location
///
/// This macro:
/// 1. Parses the location string into `(uri, start_offset, end_offset)`
/// 2. Finds the declaration by name
/// 3. Finds a constant reference to that declaration at the given uri and start offset
/// 4. Asserts the end offset matches
///
/// Location format: "uri:start_line:start_column-end_line:end_column"
/// Example: `<file:///foo.rb:3:0-3:5>`
#[cfg(test)]
#[macro_export]
macro_rules! assert_constant_reference_to {
    ($context:expr, $declaration_name:expr, $location:expr) => {
        let mut all_references = $context
            .graph()
            .constant_references()
            .values()
            .map(|reference| {
                (
                    reference,
                    format!(
                        "{}:{}",
                        $context.graph().documents().get(&reference.uri_id()).unwrap().uri(),
                        reference
                            .offset()
                            .to_display_range($context.graph().documents().get(&reference.uri_id()).unwrap())
                    ),
                )
            })
            .collect::<Vec<_>>();

        all_references.sort_by_key(|(_, reference_location)| reference_location.clone());

        let reference_at_location = all_references
            .iter()
            .find(|(_, reference_location)| reference_location == $location)
            .map(|(reference, _)| reference)
            .expect(&format!(
                "No constant reference at `{}`, found references at {:?}",
                $location,
                all_references
                    .iter()
                    .map(|(_reference, reference_location)| reference_location)
                    .collect::<Vec<_>>()
            ));

        let reference_name = $context.graph().names().get(reference_at_location.name_id()).unwrap();
        let NameRef::Resolved(resolved_name) = reference_name else {
            panic!("Reference to found at `{}` is unresolved", $location);
        };

        let resolved_name_name = $context
            .graph()
            .declarations()
            .get(resolved_name.declaration_id())
            .unwrap()
            .name();
        assert_eq!(
            resolved_name_name, $declaration_name,
            "Expected reference at `{}` to be resolved to `{}`, but got `{}`",
            $location, $declaration_name, resolved_name_name
        );
    };
}

#[cfg(test)]
#[macro_export]
macro_rules! assert_declaration_references_count_eq {
    ($context:expr, $declaration_name:expr, $expected_references:expr) => {
        let declaration = $context
            .graph()
            .declarations()
            .get(&$crate::model::ids::DeclarationId::from($declaration_name))
            .unwrap();

        let count = declaration.reference_count();

        assert_eq!(
            count, $expected_references,
            "Expected exactly {} references for `{}`, but got {}",
            $expected_references, $declaration_name, count,
        );
    };
}

#[cfg(test)]
#[macro_export]
macro_rules! assert_constant_reference_unresolved {
    ($context:expr, $unqualified_name:expr) => {
        let reference_name = $context
            .graph()
            .constant_references()
            .values()
            .find_map(|r| {
                let name = $context.graph().names().get(r.name_id()).unwrap();
                if $context.graph().strings().get(name.str()).unwrap().as_str() == $unqualified_name {
                    Some(name)
                } else {
                    None
                }
            })
            .unwrap_or_else(|| panic!("No constant reference with unqualified name `{}`", $unqualified_name));

        assert!(
            matches!(reference_name, $crate::model::name::NameRef::Unresolved(_)),
            "Expected constant reference `{}` to be unresolved, but it was resolved",
            $unqualified_name
        );
    };
    ($context:expr, $unqualified_name:expr, $location:expr) => {
        let mut all_references = $context
            .graph()
            .constant_references()
            .values()
            .map(|reference| {
                (
                    reference,
                    format!(
                        "{}:{}",
                        $context.graph().documents().get(&reference.uri_id()).unwrap().uri(),
                        reference
                            .offset()
                            .to_display_range($context.graph().documents().get(&reference.uri_id()).unwrap())
                    ),
                )
            })
            .collect::<Vec<_>>();

        all_references.sort_by_key(|(_, reference_location)| reference_location.clone());

        let reference_at_location = all_references
            .iter()
            .find(|(_, reference_location)| reference_location == $location)
            .map(|(reference, _)| reference)
            .expect(&format!(
                "No constant reference at `{}`, found references at {:?}",
                $location,
                all_references
                    .iter()
                    .map(|(_reference, reference_location)| reference_location)
                    .collect::<Vec<_>>()
            ));

        let reference_name = $context.graph().names().get(reference_at_location.name_id()).unwrap();
        assert!(
            matches!(reference_name, $crate::model::name::NameRef::Unresolved(_)),
            "Expected constant reference at `{}` to be unresolved, but it was resolved to `{}`",
            $location,
            if let $crate::model::name::NameRef::Resolved(resolved) = reference_name {
                $context
                    .graph()
                    .declarations()
                    .get(resolved.declaration_id())
                    .unwrap()
                    .name()
                    .to_string()
            } else {
                String::new()
            }
        );
    };
}

#[cfg(test)]
#[macro_export]
macro_rules! assert_ivar_reference_unresolved {
    ($context:expr, $ivar_name:expr) => {
        let ivar_ref = $context
            .graph()
            .instance_variable_references()
            .values()
            .find(|r| $context.graph().strings().get(r.str_id()).unwrap().as_str() == $ivar_name)
            .unwrap_or_else(|| panic!("No instance variable reference with name `{}`", $ivar_name));

        assert!(
            matches!(
                ivar_ref,
                $crate::model::references::InstanceVariableReference::Unresolved(_)
            ),
            "Expected instance variable reference `{}` to be unresolved, but it was resolved",
            $ivar_name
        );
    };
}

#[cfg(test)]
#[macro_export]
macro_rules! assert_ancestors_eq {
    // Arm with mixed Complete/Partial entries: ["Foo", Partial("M1"), "Object"]
    ($context:expr, $name:expr, [$($entry:tt $( ($partial_name:expr) )?),* $(,)?]) => {{
        let declaration = $context
            .graph()
            .declarations()
            .get(&$crate::model::ids::DeclarationId::from($name))
            .unwrap();

        let actual = match declaration.as_namespace().unwrap().ancestors() {
            $crate::model::declaration::Ancestors::Complete(a)
            | $crate::model::declaration::Ancestors::Cyclic(a)
            | $crate::model::declaration::Ancestors::Partial(a) => a,
        };

        let actual_strs: Vec<String> = actual.iter().map(|a| match a {
            $crate::model::declaration::Ancestor::Complete(id) => {
                $context.graph().declarations().get(id).unwrap().name().to_string()
            }
            $crate::model::declaration::Ancestor::Partial(name_id) => {
                let name = $context.graph().names().get(name_id).unwrap();
                format!("Partial({})", $context.graph().strings().get(name.str()).unwrap().as_str())
            }
        }).collect();

        let expected_strs: Vec<String> = vec![
            $($crate::assert_ancestors_eq!(@str $entry $( ($partial_name) )?)),*
        ];

        assert_eq!(
            expected_strs, actual_strs,
            "Incorrect ancestors for {}",
            $name
        );
    }};

    // Arm for variable expressions (e.g., `empty_ancestors`): all entries assumed Complete
    ($context:expr, $name:expr, $expected:expr) => {{
        let declaration = $context
            .graph()
            .declarations()
            .get(&$crate::model::ids::DeclarationId::from($name))
            .unwrap();

        let expected_ancestors: Vec<$crate::model::declaration::Ancestor> = $expected
            .iter()
            .map(|n| {
                $crate::model::declaration::Ancestor::Complete($crate::model::ids::DeclarationId::from(*n))
            })
            .collect();

        let actual = match declaration.as_namespace().unwrap().ancestors() {
            $crate::model::declaration::Ancestors::Complete(a)
            | $crate::model::declaration::Ancestors::Cyclic(a)
            | $crate::model::declaration::Ancestors::Partial(a) => a,
        };

        assert_eq!(
            expected_ancestors, *actual,
            "Incorrect ancestors for {}",
            $name
        );
    }};

    // Internal: Partial("name") → "Partial(name)" string
    (@str Partial ($name:expr)) => {
        format!("Partial({})", $name)
    };

    // Internal: "name" → "name" string
    (@str $name:expr) => {
        $name.to_string()
    };
}

#[cfg(test)]
#[macro_export]
macro_rules! assert_descendants {
    ($context:expr, $parent:expr, $descendants:expr) => {
        let parent = $context
            .graph()
            .declarations()
            .get(&$crate::model::ids::DeclarationId::from($parent))
            .unwrap();
        let actual = match parent {
            $crate::model::declaration::Declaration::Namespace($crate::model::declaration::Namespace::Class(class)) => {
                class.descendants().iter().cloned().collect::<Vec<_>>()
            }
            $crate::model::declaration::Declaration::Namespace($crate::model::declaration::Namespace::Module(
                module,
            )) => module.descendants().iter().cloned().collect::<Vec<_>>(),
            $crate::model::declaration::Declaration::Namespace(
                $crate::model::declaration::Namespace::SingletonClass(singleton),
            ) => singleton.descendants().iter().cloned().collect::<Vec<_>>(),
            _ => panic!("Tried to get descendants for a declaration that isn't a namespace"),
        };

        for descendant in &$descendants {
            let descendant_id = $crate::model::ids::DeclarationId::from(*descendant);

            assert!(
                actual.contains(&descendant_id),
                "Expected '{}' to be a descendant of '{}'",
                $context.graph().declarations().get(&descendant_id).unwrap().name(),
                parent.name()
            );
        }
    };
}

#[cfg(test)]
#[macro_export]
macro_rules! assert_members_eq {
    ($context:expr, $declaration_id:expr, $expected_members:expr) => {
        let mut actual_members = $context
            .graph()
            .declarations()
            .get(&$crate::model::ids::DeclarationId::from($declaration_id))
            .unwrap()
            .as_namespace()
            .unwrap()
            .members()
            .iter()
            .map(|(str_id, _)| $context.graph().strings().get(str_id).unwrap().as_str())
            .collect::<Vec<_>>();

        actual_members.sort();

        assert_eq!($expected_members, actual_members.as_slice());
    };
}

#[cfg(test)]
#[macro_export]
macro_rules! assert_no_members {
    ($context:expr, $declaration_id:expr) => {
        assert_members_eq!($context, $declaration_id, [] as [&str; 0]);
    };
}

#[cfg(test)]
#[macro_export]
macro_rules! assert_owner_eq {
    ($context:expr, $declaration_id:expr, $expected_owner_name:expr) => {
        let actual_owner_id = $context
            .graph()
            .declarations()
            .get(&$crate::model::ids::DeclarationId::from($declaration_id))
            .unwrap()
            .owner_id();

        let actual_owner_name = $context.graph().declarations().get(actual_owner_id).unwrap().name();

        assert_eq!($expected_owner_name, actual_owner_name);
    };
}

#[cfg(test)]
#[macro_export]
macro_rules! assert_singleton_class_eq {
    ($context:expr, $declaration_id:expr, $expected_singleton_class_name:expr) => {
        let declaration = $context
            .graph()
            .declarations()
            .get(&$crate::model::ids::DeclarationId::from($declaration_id))
            .unwrap();

        assert_eq!(
            $expected_singleton_class_name,
            $context
                .graph()
                .declarations()
                .get(declaration.as_namespace().unwrap().singleton_class().unwrap())
                .unwrap()
                .name()
        );
    };
}

#[cfg(test)]
#[macro_export]
macro_rules! assert_instance_variables_eq {
    ($context:expr, $declaration_id:expr, $expected_instance_variables:expr) => {
        let mut actual_instance_variables = $context
            .graph()
            .declarations()
            .get(&$crate::model::ids::DeclarationId::from($declaration_id))
            .unwrap()
            .as_namespace()
            .unwrap()
            .members()
            .iter()
            .filter_map(
                |(str_id, member_id)| match $context.graph().declarations().get(member_id) {
                    Some($crate::model::declaration::Declaration::InstanceVariable(_)) => {
                        Some($context.graph().strings().get(str_id).unwrap().as_str())
                    }
                    _ => None,
                },
            )
            .collect::<Vec<_>>();

        actual_instance_variables.sort();

        assert_eq!($expected_instance_variables, actual_instance_variables.as_slice());
    };
}

#[cfg(test)]
#[macro_export]
macro_rules! assert_diagnostics_eq {
    ($context:expr, $expected_diagnostics:expr) => {{
        assert_eq!($expected_diagnostics, $context.format_diagnostics(&[]).as_slice());
    }};
    ($context:expr, $expected_diagnostics:expr, $ignore_rules:expr) => {{
        assert_eq!($expected_diagnostics, $context.format_diagnostics($ignore_rules));
    }};
}

#[cfg(test)]
#[macro_export]
macro_rules! assert_no_diagnostics {
    ($context:expr) => {{
        let diagnostics = $context.format_diagnostics(&[]);
        assert!(diagnostics.is_empty(), "expected no diagnostics, got {:?}", diagnostics);
    }};
    ($context:expr, $ignore_rules:expr) => {{
        let diagnostics = $context.format_diagnostics($ignore_rules);
        assert!(diagnostics.is_empty(), "expected no diagnostics, got {:?}", diagnostics);
    }};
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_index_uri_with_single_line() {
        let mut context = GraphTest::new();

        context.index_uri("file://method.rb", "class Foo; end");
        context.resolve();

        let foo_defs = context.graph.get("Foo").unwrap();
        assert_eq!(foo_defs.len(), 1);
        assert_eq!(foo_defs[0].offset().start(), 0);
        assert_eq!(foo_defs[0].offset().end(), 14);
    }

    #[test]
    fn test_index_uri_with_multiple_lines() {
        let mut context = GraphTest::new();

        context.index_uri("file://method.rb", {
            "
            class Foo
              class Bar; end
            end
            "
        });

        context.resolve();

        let foo_defs = context.graph.get("Foo").unwrap();
        assert_eq!(foo_defs.len(), 1);
        assert_eq!(foo_defs[0].offset().start(), 0);
        assert_eq!(foo_defs[0].offset().end(), 30);

        let bar_defs = context.graph.get("Foo::Bar").unwrap();
        assert_eq!(bar_defs.len(), 1);
        assert_eq!(bar_defs[0].offset().start(), 12);
        assert_eq!(bar_defs[0].offset().end(), 26);
    }

    #[test]
    fn test_index_uri_with_new_lines() {
        let mut context = GraphTest::new();

        context.index_uri("file://method.rb", "\n\nclass Foo; end");
        context.resolve();

        let foo_defs = context.graph.get("Foo").unwrap();
        assert_eq!(foo_defs.len(), 1);
        assert_eq!(foo_defs[0].offset().start(), 2);
        assert_eq!(foo_defs[0].offset().end(), 16);
    }
}
