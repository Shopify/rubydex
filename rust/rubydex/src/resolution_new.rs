use std::sync::LazyLock;

use crate::model::declaration::ClassDeclaration;
use crate::model::declaration::Declaration;
use crate::model::declaration::DeclarationKind;
use crate::model::declaration::ModuleDeclaration;
use crate::model::declaration::Namespace;
use crate::model::declaration::{Ancestor, Ancestors};
use crate::model::definitions::Definition;
use crate::model::document::Document;
use crate::model::graph::Graph;
use crate::model::ids::DeclarationId;
use crate::model::ids::DefinitionId;
use crate::model::ids::NameId;
use crate::model::ids::UriId;
use crate::model::name::NameRef;

pub static OBJECT_ID: LazyLock<DeclarationId> = LazyLock::new(|| DeclarationId::from("Object"));
pub static MODULE_ID: LazyLock<DeclarationId> = LazyLock::new(|| DeclarationId::from("Module"));
pub static CLASS_ID: LazyLock<DeclarationId> = LazyLock::new(|| DeclarationId::from("Class"));

pub struct ResolverNew<'a> {
    graph: &'a mut Graph,
}

impl<'a> ResolverNew<'a> {
    pub fn new(graph: &'a mut Graph) -> Self {
        Self { graph }
    }

    pub fn resolve_all(&mut self) {
        self.graph.clear_declarations();

        {
            self.graph.declarations_mut().insert(
                *OBJECT_ID,
                Declaration::Namespace(Namespace::Class(Box::new(ClassDeclaration::new(
                    "Object".to_string(),
                    *OBJECT_ID,
                )))),
            );
            self.graph.declarations_mut().insert(
                *MODULE_ID,
                Declaration::Namespace(Namespace::Class(Box::new(ClassDeclaration::new(
                    "Module".to_string(),
                    *OBJECT_ID,
                )))),
            );
            self.graph.declarations_mut().insert(
                *CLASS_ID,
                Declaration::Namespace(Namespace::Class(Box::new(ClassDeclaration::new(
                    "Class".to_string(),
                    *OBJECT_ID,
                )))),
            );
        }

        let uri_ids = self.graph.documents().keys().cloned().collect::<Vec<_>>();
        for uri_id in uri_ids {
            self.resolve_document(uri_id);
        }
    }

    fn resolve_document(&mut self, uri_id: UriId) {
        println!("resolve_document: {:?}", uri_id);

        let document = self.graph.documents().get(&uri_id).unwrap();
        let definition_ids = document.top_level_definition_ids().to_vec();

        for definition_id in definition_ids {
            self.resolve_definition(definition_id);
        }
    }

    fn resolve_definition(&mut self, definition_id: DefinitionId) {
        println!("resolve_definition: {:?}", definition_id);

        let definition = self.graph.definitions().get(&definition_id).unwrap();

        match definition {
            Definition::Class(class) => {
                self.resolve_namespace(*class.name_id(), definition_id);
            }
            _ => {
                // TODO: add diagnostic
            }
        }

        // create the declaration
        // visit the members

        // match definition {
        //     Definition::Class(class) => {
        //         self.resolve_class(class);
        //     }
        //     _ => panic!("Expected a class definition"),
        // }
    }

    fn resolve_namespace(&mut self, name_id: NameId, definition_id: DefinitionId) {
        println!("resolve_namespace: {:?}", name_id);

        let name_ref = self.graph.names().get(&name_id).unwrap();

        match name_ref {
            NameRef::Resolved(resolved) => {
                let declaration_id = resolved.declaration_id();
                self.extend_declaration(*declaration_id, definition_id);
            }
            NameRef::Unresolved(unresolved) => {
                let mut fully_qualified_name = self.graph.strings().get(unresolved.str()).unwrap().to_string();
                let mut owner_id = *OBJECT_ID;

                if let Some(nesting) = unresolved.nesting() {
                    match self.graph.names().get(nesting).unwrap() {
                        NameRef::Resolved(resolved) => {
                            owner_id = *resolved.declaration_id();
                            let owner_name = self.graph.declarations().get(&owner_id).unwrap().name();
                            fully_qualified_name = format!("{owner_name}::{fully_qualified_name}");
                        }
                        NameRef::Unresolved(_) => {
                            // TODO: resolve the nested name
                            return;
                        }
                    }
                }

                if let Some(_parent_scope) = unresolved.parent_scope() {
                    // TODO: resolve the parent scope
                    return;
                }

                let declaration_id = DeclarationId::from(&fully_qualified_name);
                self.create_declaration(fully_qualified_name, declaration_id, owner_id, definition_id);
            }
        }
    }

    fn create_declaration(
        &mut self,
        fully_qualified_name: String,
        declaration_id: DeclarationId,
        owner_id: DeclarationId,
        definition_id: DefinitionId,
    ) {
        let definition = self.graph.definitions().get(&definition_id).unwrap();

        match definition {
            Definition::Class(_class) => {
                let declaration = Declaration::Namespace(Namespace::Class(Box::new(ClassDeclaration::new(
                    fully_qualified_name,
                    owner_id,
                ))));
                self.graph.declarations_mut().insert(declaration_id, declaration);
            }
            Definition::Module(_module) => {
                let declaration = Declaration::Namespace(Namespace::Module(Box::new(ModuleDeclaration::new(
                    fully_qualified_name,
                    owner_id,
                ))));
                self.graph.declarations_mut().insert(declaration_id, declaration);
            }
            _ => {
                // TODO
            }
        }
    }

    fn extend_declaration(&mut self, declaration_id: DeclarationId, definition_id: DefinitionId) {
        let declaration = self.graph.declarations_mut().get_mut(&declaration_id).unwrap();
        declaration.add_definition(definition_id);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::diagnostic::Rule;
    use crate::model::ids::UriId;
    use crate::test_utils::GraphTest;

    macro_rules! assert_constant_alias_target_eq {
        ($context:expr, $alias_name:expr, $target_name:expr) => {{
            let decl_id = DeclarationId::from($alias_name);
            let target = $context
                .graph()
                .alias_targets(&decl_id)
                .and_then(|t| t.first().copied());
            assert_eq!(
                target,
                Some(DeclarationId::from($target_name)),
                "Expected alias '{}' to have primary target '{}'",
                $alias_name,
                $target_name
            );
        }};
    }

    macro_rules! assert_no_constant_alias_target {
        ($context:expr, $alias_name:expr) => {{
            let decl_id = DeclarationId::from($alias_name);
            let targets = $context.graph().alias_targets(&decl_id).unwrap_or_default();
            assert!(
                targets.is_empty(),
                "Expected no alias target for '{}', but found {:?}",
                $alias_name,
                targets
            );
        }};
    }

    macro_rules! assert_alias_targets_contain {
        ($context:expr, $alias_name:expr, $($target_name:expr),+ $(,)?) => {{
            let decl_id = DeclarationId::from($alias_name);
            let targets = $context.graph().alias_targets(&decl_id).unwrap_or_default();
            $(
                let expected_id = DeclarationId::from($target_name);
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
    macro_rules! assert_constant_reference_to {
        ($context:expr, $declaration_name:expr, $location:expr) => {
            let (uri, start, end) = $context.parse_location($location);

            let declaration = $context
                .graph()
                .declarations()
                .get(&DeclarationId::from($declaration_name))
                .expect(&format!("Declaration '{}' not found in graph", $declaration_name));

            let constant = declaration
                .references()
                .iter()
                .filter_map(|r| {
                    let reference = $context
                        .graph()
                        .constant_references()
                        .get(r)
                        .expect("Reference should exist");
                    if let NameRef::Resolved(_) = $context
                        .graph()
                        .names()
                        .get(reference.name_id())
                        .expect("Name should exist")
                    {
                        Some(reference)
                    } else {
                        None
                    }
                })
                .find(|c| c.uri_id() == UriId::from(&uri) && c.offset().start() == start)
                .expect(&format!(
                    "Declaration '{}' does not have a reference at {} starting at offset {}",
                    $declaration_name, $location, start
                ));

            $context.assert_offset_matches(
                &uri,
                constant.offset(),
                start,
                end,
                &format!("reference to '{}'", $declaration_name),
                $location,
            );
        };
    }

    macro_rules! assert_ancestors_eq {
        ($context:expr, $name:expr, $expected:expr) => {
            let declaration = $context
                .graph()
                .declarations()
                .get(&DeclarationId::from($name))
                .unwrap();

            match declaration.as_namespace().unwrap().ancestors() {
                Ancestors::Cyclic(ancestors) | Ancestors::Complete(ancestors) => {
                    assert_eq!(
                        $expected
                            .iter()
                            .map(|n| Ancestor::Complete(DeclarationId::from(*n)))
                            .collect::<Vec<_>>(),
                        ancestors,
                        "Incorrect ancestors {}",
                        ancestors
                            .iter()
                            .filter_map(|id| {
                                if let Ancestor::Complete(id) = id {
                                    let name = {
                                        $context
                                            .graph()
                                            .declarations()
                                            .get(id)
                                            .unwrap()
                                            .name()
                                            .to_string()
                                    };
                                    Some(name)
                                } else {
                                    None
                                }
                            })
                            .collect::<Vec<_>>()
                            .join(", ")
                    );
                }
                Ancestors::Partial(_) => {
                    panic!("Expected ancestors to be resolved for {}", declaration.name());
                }
            }
        };
    }

    macro_rules! assert_descendants {
        ($context:expr, $parent:expr, $descendants:expr) => {
            let parent = $context
                .graph()
                .declarations()
                .get(&DeclarationId::from($parent))
                .unwrap();
            let actual = match parent {
                Declaration::Namespace(Namespace::Class(class)) => {
                    class.descendants().iter().cloned().collect::<Vec<_>>()
                }
                Declaration::Namespace(Namespace::Module(module)) => {
                    module.descendants().iter().cloned().collect::<Vec<_>>()
                }
                Declaration::Namespace(Namespace::SingletonClass(singleton)) => {
                    singleton.descendants().iter().cloned().collect::<Vec<_>>()
                }
                _ => panic!("Tried to get descendants for a declaration that isn't a namespace"),
            };

            for descendant in &$descendants {
                let descendant_id = DeclarationId::from(*descendant);

                assert!(
                    actual.contains(&descendant_id),
                    "Expected '{}' to be a descendant of '{}'",
                    $context
                        .graph()
                        .declarations()
                        .get(&descendant_id)
                        .unwrap()
                        .name(),
                    parent.name()
                );
            }
        };
    }

    macro_rules! assert_members_eq {
        ($context:expr, $declaration_id:expr, $expected_members:expr) => {
            let mut actual_members = $context
                .graph()
                .declarations()
                .get(&DeclarationId::from($declaration_id))
                .expect(&format!("Declaration '{}' not found in graph", $declaration_id))
                .as_namespace()
                .unwrap()
                .members()
                .iter()
                .map(|(str_id, _)| $context.graph().strings().get(str_id).unwrap().as_str())
                .collect::<Vec<_>>();

            actual_members.sort();

            assert_eq!($expected_members, actual_members);
        };
    }

    macro_rules! assert_no_members {
        ($context:expr, $declaration_id:expr) => {
            assert_members_eq!($context, $declaration_id, vec![] as Vec<&str>);
        };
    }

    macro_rules! assert_owner_eq {
        ($context:expr, $declaration_id:expr, $expected_owner_name:expr) => {
            let actual_owner_id = $context
                .graph()
                .declarations()
                .get(&DeclarationId::from($declaration_id))
                .unwrap()
                .owner_id();

            let actual_owner_name = $context
                .graph()
                .declarations()
                .get(actual_owner_id)
                .unwrap()
                .name();

            assert_eq!($expected_owner_name, actual_owner_name);
        };
    }

    macro_rules! assert_singleton_class_eq {
        ($context:expr, $declaration_id:expr, $expected_singleton_class_name:expr) => {
            let declaration = $context
                .graph()
                .declarations()
                .get(&DeclarationId::from($declaration_id))
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

    macro_rules! assert_instance_variables_eq {
        ($context:expr, $declaration_id:expr, $expected_instance_variables:expr) => {
            let mut actual_instance_variables = $context
                .graph()
                .declarations()
                .get(&DeclarationId::from($declaration_id))
                .unwrap()
                .as_namespace()
                .unwrap()
                .members()
                .iter()
                .filter_map(
                    |(str_id, member_id)| match $context.graph().declarations().get(member_id) {
                        Some(Declaration::InstanceVariable(_)) => {
                            Some($context.graph().strings().get(str_id).unwrap().as_str())
                        }
                        _ => None,
                    },
                )
                .collect::<Vec<_>>();

            actual_instance_variables.sort();

            assert_eq!($expected_instance_variables, actual_instance_variables);
        };
    }

    fn format_diagnostics(context: &GraphTest, ignore_rules: &[Rule]) -> Vec<String> {
        let mut diagnostics: Vec<_> = context
            .graph()
            .all_diagnostics()
            .into_iter()
            .filter(|d| !ignore_rules.contains(d.rule()))
            .collect();

        diagnostics.sort_by_key(|d| {
            let uri = context.graph().documents().get(d.uri_id()).unwrap().uri();
            (uri, d.offset())
        });

        diagnostics
            .iter()
            .map(|d| {
                let document = context.graph().documents().get(d.uri_id()).unwrap();
                d.formatted(document)
            })
            .collect()
    }

    macro_rules! assert_diagnostics_eq {
        ($context:expr, $expected_diagnostics:expr) => {{
            assert_eq!($expected_diagnostics, format_diagnostics($context, &[]));
        }};
        ($context:expr, $expected_diagnostics:expr, $ignore_rules:expr) => {{
            assert_eq!($expected_diagnostics, format_diagnostics($context, $ignore_rules));
        }};
    }

    macro_rules! assert_no_diagnostics {
        ($context:expr) => {{
            let diagnostics = format_diagnostics($context, &[]);
            assert!(diagnostics.is_empty(), "expected no diagnostics, got {:?}", diagnostics);
        }};
        ($context:expr, $ignore_rules:expr) => {{
            let diagnostics = format_diagnostics($context, $ignore_rules);
            assert!(diagnostics.is_empty(), "expected no diagnostics, got {:?}", diagnostics);
        }};
    }

    #[test]
    fn resolving_top_level_class_declaration() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            class Foo
              class Bar; end
            end
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context);

        assert_no_members!(context, "Foo");
        assert_owner_eq!(context, "Foo", "Object");

        assert_no_members!(context, "Foo::Bar");
        assert_owner_eq!(context, "Foo::Bar", "Foo");
    }

    #[test]
    fn resolving_top_level_references() {
        let mut context = GraphTest::new();
        context.index_uri("file:///bar.rb", {
            r"
            class Bar; end

            ::Bar
            Bar
            "
        });
        context.index_uri("file:///foo.rb", {
            r"
            module Foo
              ::Bar
            end
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context, &[Rule::ParseWarning]);

        assert_constant_reference_to!(context, "Bar", "file:///bar.rb:2:2-2:5");
        assert_constant_reference_to!(context, "Bar", "file:///bar.rb:3:0-3:3");
        assert_constant_reference_to!(context, "Bar", "file:///foo.rb:1:4-1:7");
    }

    #[test]
    fn resolving_nested_reference() {
        let mut context = GraphTest::new();
        context.index_uri("file:///bar.rb", {
            r"
            module Foo
              CONST = 123

              class Bar
                CONST
                Foo::CONST
              end
            end
            "
        });
        context.resolve();

        assert_constant_reference_to!(context, "Foo::CONST", "file:///bar.rb:4:4-4:9");
        assert_constant_reference_to!(context, "Foo::CONST", "file:///bar.rb:5:9-5:14");
    }

    #[test]
    fn resolving_nested_reference_that_refer_to_top_level_constant() {
        let mut context = GraphTest::new();
        context.index_uri("file:///bar.rb", {
            r"
            class Baz; end

            module Foo
              class Bar
                Baz
              end
            end
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context);

        assert_constant_reference_to!(context, "Baz", "file:///bar.rb:4:4-4:7");
    }

    #[test]
    fn resolving_constant_path_references_at_top_level() {
        let mut context = GraphTest::new();
        context.index_uri("file:///bar.rb", {
            r"
            module Foo
              class Bar; end
            end

            Foo::Bar
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context, &[Rule::ParseWarning]);

        assert_constant_reference_to!(context, "Foo::Bar", "file:///bar.rb:4:5-4:8");
    }

    #[test]
    fn resolving_reference_for_non_existing_declaration() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            Foo
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context, &[Rule::ParseWarning]);

        let reference = context.graph().constant_references().values().next().unwrap();

        match context.graph().names().get(reference.name_id()) {
            Some(NameRef::Unresolved(_)) => {}
            _ => panic!("expected unresolved constant reference"),
        }
    }

    #[test]
    fn resolution_creates_global_declaration() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            module Foo
              class Bar
              end
            end

            class Foo::Baz
            end
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context);

        assert_members_eq!(context, "Foo", vec!["Bar", "Baz"]);
        assert_owner_eq!(context, "Foo", "Object");

        assert_no_members!(context, "Foo::Bar");
        assert_owner_eq!(context, "Foo::Bar", "Foo");

        assert_no_members!(context, "Foo::Baz");
        assert_owner_eq!(context, "Foo::Baz", "Foo");
    }

    #[test]
    fn resolution_for_non_constant_declarations() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            class Foo
              def initialize
                @name = 123
              end
            end
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context);

        assert_members_eq!(context, "Foo", vec!["@name", "initialize()"]);
        assert_owner_eq!(context, "Foo", "Object");
    }

    #[test]
    fn resolution_for_ambiguous_namespace_definitions() {
        // Like many examples of Ruby code that is ambiguous to static analysis, this example is ambiguous due to
        // require order. If `foo.rb` is loaded first, then `Bar` doesn't exist, Ruby crashes and we should emit an
        // error or warning for a non existing constant.
        //
        // If `bar.rb` is loaded first, then `Bar` resolves to top level `Bar` and `Bar::Baz` is defined, completely
        // escaping the `Foo` nesting.
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            module Foo
              class Bar::Baz
              end
            end
            "
        });
        context.index_uri("file:///bar.rb", {
            r"
            module Bar
            end
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context);

        assert_no_members!(context, "Foo");
        assert_owner_eq!(context, "Foo", "Object");

        assert_members_eq!(context, "Bar", vec!["Baz"]);
        assert_owner_eq!(context, "Bar", "Object");
    }

    #[test]
    fn resolution_for_top_level_references() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            module Foo
              class ::Bar
                class Baz
                end
              end
            end
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context);

        assert_no_members!(context, "Foo");
        assert_owner_eq!(context, "Foo", "Object");

        assert_members_eq!(context, "Bar", vec!["Baz"]);
        assert_owner_eq!(context, "Bar", "Object");

        assert_no_members!(context, "Bar::Baz");
        assert_owner_eq!(context, "Bar::Baz", "Bar");
    }

    #[test]
    fn resolution_does_not_loop_infinitely_on_non_existing_constants() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            class Foo::Bar
              class Baz
              end
            end
            "
        });
        context.resolve();
        assert!(
            context
                .graph()
                .declarations()
                .get(&DeclarationId::from("Foo"))
                .is_none()
        );
        assert!(
            context
                .graph()
                .declarations()
                .get(&DeclarationId::from("Foo::Bar"))
                .is_none()
        );
        assert!(
            context
                .graph()
                .declarations()
                .get(&DeclarationId::from("Foo::Bar::Baz"))
                .is_none()
        );

        assert_no_diagnostics!(&context);
    }

    #[test]
    fn resolution_for_singleton_class() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            class Foo
              class << self
                def bar; end
                BAZ = 123
              end
            end
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context);

        assert_no_members!(context, "Foo");
        assert_owner_eq!(context, "Foo", "Object");
        assert_singleton_class_eq!(context, "Foo", "Foo::<Foo>");

        assert_members_eq!(context, "Foo::<Foo>", vec!["BAZ", "bar()"]);
        assert_owner_eq!(context, "Foo::<Foo>", "Foo");
    }

    #[test]
    fn resolution_for_nested_singleton_class() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            class Foo
              class << self
                class << self
                  def baz; end
                end
              end
            end
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context);

        assert_no_members!(context, "Foo");
        assert_singleton_class_eq!(context, "Foo", "Foo::<Foo>");

        assert_no_members!(context, "Foo::<Foo>");
        assert_singleton_class_eq!(context, "Foo::<Foo>", "Foo::<Foo>::<<Foo>>");

        assert_members_eq!(context, "Foo::<Foo>::<<Foo>>", vec!["baz()"]);
        assert_owner_eq!(context, "Foo::<Foo>::<<Foo>>", "Foo::<Foo>");
    }

    #[test]
    fn resolution_for_singleton_class_of_external_constant() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            class Foo; end
            class Bar
              class << Foo
                def baz; end

                class Baz; end
              end
            end
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context);

        assert_no_members!(context, "Foo");
        assert_owner_eq!(context, "Foo", "Object");
        assert_singleton_class_eq!(context, "Foo", "Foo::<Foo>");

        assert_no_members!(context, "Bar");
        assert_owner_eq!(context, "Bar", "Object");

        assert_members_eq!(context, "Foo::<Foo>", vec!["Baz", "baz()"]);
        assert_owner_eq!(context, "Foo::<Foo>", "Foo");
    }

    #[test]
    fn resolution_for_class_variable_in_nested_singleton_class() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            class Foo
              class << self
                @@bar = 123

                class << self
                  @@baz = 456
                end
              end
            end
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context);

        assert_members_eq!(context, "Foo", vec!["@@bar", "@@baz"]);
        assert_owner_eq!(context, "Foo", "Object");
    }

    #[test]
    fn resolution_for_class_variable_in_method() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            class Foo
              def bar
                @@baz = 456
              end
            end
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context);

        assert_members_eq!(context, "Foo", vec!["@@baz", "bar()"]);
    }

    #[test]
    fn resolution_for_class_variable_only_follows_lexical_nesting() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            class Foo; end
            class Bar
              def Foo.demo
                @@cvar1 = 1
              end

              class << Foo
                def demo2
                  @@cvar2 = 1
                end
              end
            end
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context);

        assert_no_members!(context, "Foo");
        assert_members_eq!(context, "Bar", vec!["@@cvar1", "@@cvar2"]);
    }

    #[test]
    fn resolution_for_class_variable_at_top_level() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            "
            @@var = 123
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context);

        // TODO: this should push an error diagnostic
        assert!(
            context
                .graph()
                .declarations()
                .get(&DeclarationId::from("Object::@@var"))
                .is_none()
        );
    }

    #[test]
    fn singleton_class_is_set() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            "
            class Foo
              class << self
              end
            end
            "
        });

        context.resolve();

        assert_no_diagnostics!(&context);

        assert!(
            context
                .graph()
                .declarations()
                .get(&DeclarationId::from("Foo::<Foo>"))
                .is_some()
        );

        assert_singleton_class_eq!(context, "Foo", "Foo::<Foo>");
    }

    #[test]
    fn resolution_for_method_with_receiver() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            class Foo
              def self.bar; end

              class << self
                def self.nested_bar; end
              end
            end

            class Bar
              def Foo.baz; end

              def self.qux; end
            end
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context);

        assert_members_eq!(context, "Foo::<Foo>", vec!["bar()", "baz()"]);
        assert_owner_eq!(context, "Foo::<Foo>", "Foo");

        assert_members_eq!(context, "Foo::<Foo>::<<Foo>>", vec!["nested_bar()"]);
        assert_owner_eq!(context, "Foo::<Foo>::<<Foo>>", "Foo::<Foo>");

        assert_members_eq!(context, "Bar::<Bar>", vec!["qux()"]);
        assert_owner_eq!(context, "Bar::<Bar>", "Bar");
    }

    #[test]
    fn linearizing_super_classes() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            class Foo; end
            class Bar < Foo; end
            class Baz < Bar; end
            class Qux < Baz; end
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context);

        assert_ancestors_eq!(context, "Qux", ["Qux", "Baz", "Bar", "Foo", "Object"]);
    }

    #[test]
    fn descendants_are_tracked_for_parent_classes() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            class Foo
              CONST = 123
            end

            class Bar < Foo; end

            class Baz < Bar
              CONST
            end

            class Qux < Bar
              CONST
            end
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context);

        assert_descendants!(context, "Foo", ["Bar"]);
        assert_descendants!(context, "Bar", ["Baz", "Qux"]);
    }

    #[test]
    fn linearizing_circular_super_classes() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            class Foo < Bar; end
            class Bar < Baz; end
            class Baz < Foo; end
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context);

        assert_ancestors_eq!(context, "Foo", ["Foo", "Bar", "Baz", "Object"]);
    }

    #[test]
    fn resolving_a_constant_inherited_from_the_super_class() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            class Foo
              CONST = 123
            end

            class Bar < Foo
              CONST
            end
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context);

        assert_constant_reference_to!(context, "Foo::CONST", "file:///foo.rb:5:2-5:7");
    }

    #[test]
    fn does_not_loop_forever_on_non_existing_parents() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            class Bar < Foo
              CONST
            end
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context);

        let declaration = context.graph().declarations().get(&DeclarationId::from("Bar")).unwrap();
        assert!(matches!(
            declaration.as_namespace().unwrap().ancestors(),
            Ancestors::Partial(_)
        ));
    }

    #[test]
    fn resolving_inherited_constant_dependent_on_complex_parent() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            module Foo
              module Bar
                class Baz
                  CONST = 123
                end
              end
            end
            class Qux < Foo::Bar::Baz
              CONST
            end
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context);

        assert_constant_reference_to!(context, "Foo::Bar::Baz::CONST", "file:///foo.rb:8:2-8:7");
    }

    #[test]
    fn resolution_for_instance_and_class_instance_variables() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            class Foo
              @foo = 0

              def initialize
                @bar = 1
              end

              def self.baz
                @baz = 2
              end

              class << self
                def qux
                  @qux = 3
                end

                def self.nested
                  @nested = 4
                end
              end
            end
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context);

        assert_instance_variables_eq!(context, "Foo", vec!["@bar"]);
        // @qux in `class << self; def qux` - self is Foo when called, so @qux belongs to Foo's singleton class
        assert_instance_variables_eq!(context, "Foo::<Foo>", vec!["@baz", "@foo", "@qux"]);
        assert_instance_variables_eq!(context, "Foo::<Foo>::<<Foo>>", vec!["@nested"]);
    }

    #[test]
    fn resolution_for_instance_variables_with_dynamic_method_owner() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            class Foo
            end

            class Bar
              def Foo.bar
                @foo = 0
              end

              class << Foo
                def Bar.baz
                  @baz = 1
                end
              end
            end
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context);

        assert_instance_variables_eq!(context, "Foo::<Foo>", vec!["@foo"]);
        assert_instance_variables_eq!(context, "Bar::<Bar>", vec!["@baz"]);
    }

    #[test]
    fn resolution_for_class_instance_variable_in_compact_namespace() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            class Bar; end

            class Foo
              class Bar::Baz
                @baz = 1
              end
            end
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context);

        // The class is `Bar::Baz`, so its singleton class is `Bar::Baz::<Baz>`
        assert_instance_variables_eq!(context, "Bar::Baz::<Baz>", vec!["@baz"]);
    }

    #[test]
    fn resolution_for_instance_variable_in_singleton_class_body() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            class Foo
              class << self
                @bar = 1

                class << self
                  @baz = 2
                end
              end
            end
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context);

        assert_instance_variables_eq!(context, "Foo::<Foo>::<<Foo>>", vec!["@bar"]);
        assert_instance_variables_eq!(context, "Foo::<Foo>::<<Foo>>::<<<Foo>>>", vec!["@baz"]);
    }

    #[test]
    fn resolution_for_top_level_instance_variable() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            @foo = 0
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context);

        // Top-level instance variables belong to `<main>`, not `Object`.
        // We can't represent `<main>` yet, so no declaration is created.
        let foo_decl = context.graph().declarations().get(&DeclarationId::from("Object::@foo"));
        assert!(foo_decl.is_none(), "Object::@foo declaration should not exist");
    }

    #[test]
    fn resolution_for_instance_variable_with_unresolved_receiver() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            class Foo
              def foo.bar
                @baz = 0
              end
            end
            "
        });
        context.resolve();

        assert_diagnostics_eq!(
            &context,
            vec!["dynamic-singleton-definition: Dynamic receiver for singleton method definition (2:3-4:6)",]
        );

        // Instance variable in method with unresolved receiver should not create a declaration
        let baz_decl = context.graph().declarations().get(&DeclarationId::from("Object::@baz"));
        assert!(baz_decl.is_none(), "@baz declaration should not exist");

        let foo_baz_decl = context.graph().declarations().get(&DeclarationId::from("Foo::@baz"));
        assert!(foo_baz_decl.is_none(), "Foo::@baz declaration should not exist");
    }

    #[test]
    fn resolving_method_alias() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            class Foo
              def foo; end

              alias bar foo
            end
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context);

        assert_members_eq!(context, "Foo", vec!["bar()", "foo()"]);
    }

    #[test]
    fn resolving_global_variable_alias() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            $foo = 123
            alias $bar $foo
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context);

        assert_members_eq!(context, "Object", vec!["$bar", "$foo"]);
    }

    #[test]
    fn linearizing_parent_classes_with_parent_scope() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            module Foo
              class Bar
              end
            end
            class Baz < Foo::Bar
            end
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context);

        assert_ancestors_eq!(context, "Baz", ["Baz", "Foo::Bar", "Object"]);
    }

    #[test]
    fn resolving_constant_references_involved_in_prepends() {
        let mut context = GraphTest::new();

        // To linearize the ancestors of `Bar`, we need to resolve `Foo` first. However, during that resolution, we need
        // to check `Bar`'s ancestor chain before checking the top level (which is where we'll find `Foo`). In these
        // scenarios, we need to realize the dependency and skip ancestors
        context.index_uri("file:///foo.rb", {
            r"
            module Foo; end
            module Bar
              prepend Foo
            end
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context);

        assert_ancestors_eq!(context, "Bar", ["Foo", "Bar"]);
    }

    #[test]
    fn resolving_prepend_using_inherited_constant() {
        let mut context = GraphTest::new();
        // Prepending `Foo` makes `Bar` available, which we can then prepend as well. This requires resolving constants
        // with partially linearized ancestors
        context.index_uri("file:///foo.rb", {
            r"
            module Foo
              module Bar; end
            end
            class Baz
              prepend Foo
              prepend Bar
            end
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context);

        assert_ancestors_eq!(context, "Baz", ["Foo::Bar", "Foo", "Baz", "Object"]);
    }

    #[test]
    fn linearizing_prepended_modules() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            module Foo; end
            module Bar
              prepend Foo
            end
            class Baz
              prepend Bar
            end
            class Qux < Baz; end
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context);

        assert_ancestors_eq!(context, "Foo", ["Foo"]);
        assert_ancestors_eq!(context, "Bar", ["Foo", "Bar"]);
        assert_ancestors_eq!(context, "Qux", ["Qux", "Foo", "Bar", "Baz", "Object"]);
    }

    #[test]
    fn prepend_on_dynamic_namespace_definitions() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            module B; end
            A = Struct.new do
              prepend B
            end

            C = Class.new do
              prepend B
            end

            D = Module.new do
              prepend B
            end
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context);

        assert_ancestors_eq!(context, "B", ["B"]);
        // TODO: this is a temporary hack to avoid crashing on `Struct.new`, `Class.new` and `Module.new`
        //assert_ancestors_eq!(context, "A", Vec::<&str>::new());
        assert_ancestors_eq!(context, "C", ["B", "C", "Object"]);
        assert_ancestors_eq!(context, "D", ["B", "D"]);
    }

    #[test]
    fn prepends_track_descendants() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            module Foo; end
            module Bar
              prepend Foo
            end
            class Baz
              prepend Bar
            end
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context);

        assert_descendants!(context, "Foo", ["Bar", "Baz"]);
        assert_descendants!(context, "Bar", ["Baz"]);
    }

    #[test]
    fn cyclic_prepend() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            module Foo
              prepend Foo
            end
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context);

        assert_ancestors_eq!(context, "Foo", ["Foo"]);
    }

    #[test]
    fn duplicate_prepends() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            module Foo
            end

            module Bar
              prepend Foo
              prepend Foo
            end
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context);

        assert_ancestors_eq!(context, "Bar", ["Foo", "Bar"]);
    }

    #[test]
    fn indirect_duplicate_prepends() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            module A; end

            module B
              prepend A
            end

            module C
              prepend A
            end

            module Foo
              prepend B
              prepend C
            end
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context);

        assert_ancestors_eq!(context, "A", ["A"]);
        assert_ancestors_eq!(context, "B", ["A", "B"]);
        assert_ancestors_eq!(context, "C", ["A", "C"]);
        assert_ancestors_eq!(context, "Foo", ["A", "C", "B", "Foo"]);
    }

    #[test]
    fn multiple_mixins_in_same_prepend() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            module A; end
            module B; end

            class Foo
              prepend A, B
            end
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context);

        assert_ancestors_eq!(context, "Foo", ["A", "B", "Foo", "Object"]);
    }

    #[test]
    fn prepends_involving_parent_scopes() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            module A
              module B
                module C; end
              end
            end

            module D
              prepend A::B::C
            end

            module Foo
              prepend D
              prepend A::B::C
            end

            module Bar
              prepend A::B::C
              prepend D
            end
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context);

        assert_ancestors_eq!(context, "Foo", ["A::B::C", "D", "Foo"]);
        assert_ancestors_eq!(context, "Bar", ["A::B::C", "D", "Bar"]);
    }

    #[test]
    fn duplicate_prepends_in_parents() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            module A; end

            module B
              prepend A
            end

            class Parent
              prepend B
            end

            class Child < Parent
              prepend B
            end
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context);

        assert_ancestors_eq!(context, "Child", ["A", "B", "Child", "A", "B", "Parent", "Object"]);
    }

    #[test]
    fn prepended_modules_involved_in_definitions() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            module Foo
              module Bar; end
            end

            module Baz
              prepend Foo

              class Bar::Qux
              end
            end
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context);

        assert_members_eq!(context, "Foo::Bar", vec!["Qux"]);
        assert_owner_eq!(context, "Foo::Bar", "Foo");

        assert_no_members!(context, "Foo::Bar::Qux");
        assert_owner_eq!(context, "Foo::Bar::Qux", "Foo::Bar");
    }

    #[test]
    fn resolving_constant_references_involved_in_includes() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            module Foo; end
            module Bar
              include Foo
            end
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context);

        assert_ancestors_eq!(context, "Bar", ["Bar", "Foo"]);
    }

    #[test]
    fn resolving_include_using_inherited_constant() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            module Foo
              module Bar; end
            end
            class Baz
              include Foo
              include Bar
            end
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context);

        assert_ancestors_eq!(context, "Baz", ["Baz", "Foo::Bar", "Foo", "Object"]);
    }

    #[test]
    fn linearizing_included_modules() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            module Foo; end
            module Bar
              prepend Foo
            end
            class Baz
              prepend Bar
            end
            class Qux < Baz; end
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context);

        assert_ancestors_eq!(context, "Foo", ["Foo"]);
        assert_ancestors_eq!(context, "Bar", ["Foo", "Bar"]);
        assert_ancestors_eq!(context, "Qux", ["Qux", "Foo", "Bar", "Baz", "Object"]);
    }

    #[test]
    fn include_on_dynamic_namespace_definitions() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            module B; end
            A = Struct.new do
              include B
            end

            C = Class.new do
              include B
            end

            D = Module.new do
              include B
            end
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context);

        assert_ancestors_eq!(context, "B", ["B"]);
        // TODO: this is a temporary hack to avoid crashing on `Struct.new`, `Class.new` and `Module.new`
        //assert_ancestors_eq!(context, "A", Vec::<&str>::new());
        assert_ancestors_eq!(context, "C", ["C", "B", "Object"]);
        assert_ancestors_eq!(context, "D", ["D", "B"]);
    }

    #[test]
    fn cyclic_include() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            module Foo
              include Foo
            end
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context);

        assert_ancestors_eq!(context, "Foo", ["Foo"]);
    }

    #[test]
    fn duplicate_includes() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            module Foo
            end

            module Bar
              include Foo
              include Foo
            end
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context);

        assert_ancestors_eq!(context, "Bar", ["Bar", "Foo"]);
    }

    #[test]
    fn indirect_duplicate_includes() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            module A; end

            module B
              include A
            end

            module C
              include A
            end

            module Foo
              include B
              include C
            end
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context);

        assert_ancestors_eq!(context, "A", ["A"]);
        assert_ancestors_eq!(context, "B", ["B", "A"]);
        assert_ancestors_eq!(context, "C", ["C", "A"]);
        assert_ancestors_eq!(context, "Foo", ["Foo", "C", "B", "A"]);
    }

    #[test]
    fn includes_involving_parent_scopes() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            module A
              module B
                module C; end
              end
            end

            module D
              include A::B::C
            end

            module Foo
              include D
              include A::B::C
            end

            module Bar
              include A::B::C
              include D
            end
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context);

        assert_ancestors_eq!(context, "Foo", ["Foo", "D", "A::B::C"]);
        assert_ancestors_eq!(context, "Bar", ["Bar", "D", "A::B::C"]);
    }

    #[test]
    fn duplicate_includes_in_parents() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            module A; end

            module B
              include A
            end

            class Parent
              include B
            end

            class Child < Parent
              include B
            end
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context);

        assert_ancestors_eq!(context, "Child", ["Child", "Parent", "B", "A", "Object"]);
    }

    #[test]
    fn included_modules_involved_in_definitions() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            module Foo
              module Bar; end
            end

            module Baz
              include Foo

              class Bar::Qux
              end
            end
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context);

        assert_members_eq!(context, "Foo::Bar", vec!["Qux"]);
        assert_owner_eq!(context, "Foo::Bar", "Foo");

        assert_no_members!(context, "Foo::Bar::Qux");
        assert_owner_eq!(context, "Foo::Bar::Qux", "Foo::Bar");
    }

    #[test]
    fn references_with_parent_scope_search_inheritance() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            module Foo
              module Bar; end
            end

            class Baz
              include Foo
            end

            Baz::Bar
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context, &[Rule::ParseWarning]);

        assert_constant_reference_to!(context, "Foo::Bar", "file:///foo.rb:8:5-8:8");
    }

    #[test]
    fn duplicate_includes_and_prepends() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            module A; end

            class Foo
              prepend A
              include A
            end

            class Bar
              include A
              prepend A
            end
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context);

        assert_ancestors_eq!(context, "Foo", ["A", "Foo", "Object"]);
        assert_ancestors_eq!(context, "Bar", ["A", "Bar", "A", "Object"]);
    }

    #[test]
    fn duplicate_indirect_includes_and_prepends() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            module A; end
            module B
              include A
            end
            module C
              prepend A
            end

            class Foo
              include C
              prepend B
              include A
            end

            class Bar
              include A
              prepend B
              include C
            end

            class Baz
              prepend B
              include C
              prepend A
            end

            class Qux
              prepend A
              include C
              prepend B
            end
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context);

        assert_ancestors_eq!(context, "Foo", ["B", "A", "Foo", "A", "C", "Object"]);
        assert_ancestors_eq!(context, "Bar", ["B", "A", "Bar", "C", "A", "Object"]);
        assert_ancestors_eq!(context, "Baz", ["B", "A", "Baz", "C", "Object"]);
        assert_ancestors_eq!(context, "Qux", ["B", "A", "Qux", "C", "Object"]);
    }

    #[test]
    fn duplicate_includes_and_prepends_through_parents() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            module A; end

            class Parent
              include A
            end

            class Foo < Parent
              prepend A
            end

            class Bar < Parent
              include A
            end
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context);

        assert_ancestors_eq!(context, "Foo", ["A", "Foo", "Parent", "A", "Object"]);
        assert_ancestors_eq!(context, "Bar", ["Bar", "Parent", "A", "Object"]);
    }

    #[test]
    fn multiple_mixins_in_same_include() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            module A; end
            module B; end

            class Foo
              include A, B
            end
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context);

        assert_ancestors_eq!(context, "Foo", ["Foo", "A", "B", "Object"]);
    }

    #[test]
    fn descendants_are_tracked_for_includes() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            module Foo; end
            module Bar
              include Foo
            end
            module Baz
              include Bar
            end
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context);

        assert_descendants!(context, "Bar", ["Baz"]);
        assert_descendants!(context, "Foo", ["Bar", "Baz"]);
    }

    #[test]
    fn singleton_ancestors_for_classes() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            module Foo; end
            module Qux; end
            module Zip; end
            class Bar; end

            class Baz < Bar
              extend Foo

              class << self
                include Qux

                class << self
                  include Zip
                end
              end
            end
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context);

        // Note: the commented out parts require RBS indexing
        assert_ancestors_eq!(
            context,
            "Baz::<Baz>",
            [
                "Baz::<Baz>",
                "Qux",
                "Foo",
                "Bar::<Bar>",
                "Object::<Object>",
                // "BasicObject::<BasicObject>",
                "Class",
                // "Module",
                "Object",
                // "Kernel",
                // "BasicObject"
            ]
        );

        assert_ancestors_eq!(
            context,
            "Baz::<Baz>::<<Baz>>",
            [
                "Baz::<Baz>::<<Baz>>",
                "Zip",
                "Bar::<Bar>::<<Bar>>",
                "Object::<Object>::<<Object>>",
                // "BasicObject::<BasicObject>::<<BasicObject>>",
                "Class::<Class>",
                // "Module::<Module>",
                "Object::<Object>",
                // "BasicObject::<BasicObject>",
                "Class",
                // "Module",
                "Object",
                // "Kernel",
                // "BasicObject"
            ]
        );
    }

    #[test]
    fn singleton_ancestors_for_modules() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            module Foo; end
            module Qux; end
            module Zip; end
            class Bar; end

            module Baz
              extend Foo

              class << self
                include Qux

                class << self
                  include Zip
                end
              end
            end
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context);

        // Note: the commented out parts require RBS indexing
        assert_ancestors_eq!(
            context,
            "Baz::<Baz>",
            [
                "Baz::<Baz>",
                "Qux",
                "Foo",
                "Module",
                "Object",
                // "Kernel",
                // "BasicObject"
            ]
        );
        assert_ancestors_eq!(
            context,
            "Baz::<Baz>::<<Baz>>",
            [
                "Baz::<Baz>::<<Baz>>",
                "Zip",
                "Module::<Module>",
                "Object::<Object>",
                // "BasicObject::<BasicObject>",
                "Class",
                // "Module",
                "Object",
                // "Kernel",
                // "BasicObject"
            ]
        );
    }

    #[test]
    fn singleton_ancestors_with_inherited_parent_modules() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            module Foo; end
            module Qux; end
            class Bar
              class << self
                include Foo
                prepend Qux
              end
            end

            class Baz < Bar
              class << self
                class << self
                end
              end
            end
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context);

        // TODO: the commented out parts require RBS indexing
        assert_ancestors_eq!(
            context,
            "Bar::<Bar>",
            [
                "Qux",
                "Bar::<Bar>",
                "Foo",
                "Object::<Object>",
                // "BasicObject::<BasicObject>",
                "Class",
                // "Module",
                "Object",
                // "Kernel",
                // "BasicObject"
            ]
        );

        assert_ancestors_eq!(
            context,
            "Baz::<Baz>",
            [
                "Baz::<Baz>",
                "Qux",
                "Bar::<Bar>",
                "Foo",
                "Object::<Object>",
                // "BasicObject::<BasicObject>",
                "Class",
                // "Module",
                "Object",
                // "Kernel",
                // "BasicObject"
            ]
        );
        assert_ancestors_eq!(
            context,
            "Baz::<Baz>::<<Baz>>",
            [
                "Baz::<Baz>::<<Baz>>",
                "Bar::<Bar>::<<Bar>>",
                "Object::<Object>::<<Object>>",
                // "BasicObject::<BasicObject>::<<BasicObject>>",
                "Class::<Class>",
                // "Module::<Module>",
                "Object::<Object>",
                // "BasicObject::<BasicObject>",
                "Class",
                // "Module",
                "Object",
                // "Kernel",
                // "BasicObject"
            ]
        );
    }

    #[test]
    fn resolving_global_variable_alias_inside_method() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            class Foo
              def setup
                alias $bar $baz
              end
            end
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context);

        // Global variable aliases should still be owned by Object, regardless of where defined
        assert_members_eq!(context, "Object", vec!["$bar", "Foo"]);
    }

    #[test]
    fn resolving_method_defined_inside_method() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            class Foo
              def setup
                def inner_method; end
              end
            end
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context);

        // inner_method should be owned by Foo, not by setup
        assert_members_eq!(context, "Foo", vec!["inner_method()", "setup()"]);
    }

    #[test]
    fn resolving_attr_accessors_inside_method() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            class Foo
              def self.setup
                attr_reader :reader_attr
                attr_writer :writer_attr
                attr_accessor :accessor_attr
              end
            end
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context);

        assert_members_eq!(context, "Foo::<Foo>", vec!["setup()"]);

        // All attr_* should be owned by Foo, not by setup
        assert_members_eq!(
            context,
            "Foo",
            vec!["accessor_attr()", "reader_attr()", "writer_attr()"]
        );
    }

    #[test]
    fn resolving_constant_alias_to_module() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            module Foo
              CONST = 123
            end

            ALIAS = Foo
            ALIAS::CONST
            "
        });
        context.resolve();
        assert_no_diagnostics!(&context, &[Rule::ParseWarning]);

        assert_constant_alias_target_eq!(context, "ALIAS", "Foo");
        assert_constant_reference_to!(context, "Foo::CONST", "file:///foo.rb:5:7-5:12");
    }

    #[test]
    fn resolving_constant_alias_to_nested_module() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            module Foo
              module Bar
                CONST = 123
              end
            end

            ALIAS = Foo::Bar
            ALIAS::CONST
            "
        });
        context.resolve();
        assert_no_diagnostics!(&context, &[Rule::ParseWarning]);

        assert_constant_alias_target_eq!(context, "ALIAS", "Foo::Bar");
        assert_constant_reference_to!(context, "Foo::Bar::CONST", "file:///foo.rb:7:7-7:12");
    }

    #[test]
    fn resolving_constant_alias_inside_module() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            module Foo
              CONST = 123
            end

            module Bar
              MyFoo = Foo
              MyFoo::CONST
            end
            "
        });
        context.resolve();
        assert_no_diagnostics!(&context);

        assert_constant_alias_target_eq!(context, "Bar::MyFoo", "Foo");
        assert_constant_reference_to!(context, "Foo::CONST", "file:///foo.rb:6:9-6:14");
    }

    #[test]
    fn resolving_constant_alias_in_superclass() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            class Foo
              CONST = 123
            end

            class Bar < Foo
            end

            ALIAS = Bar
            ALIAS::CONST
            "
        });
        context.resolve();
        assert_no_diagnostics!(&context, &[Rule::ParseWarning]);

        assert_constant_reference_to!(context, "Foo::CONST", "file:///foo.rb:8:7-8:12");
    }

    #[test]
    fn resolving_chained_constant_aliases() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            module Foo
              CONST = 123
            end

            ALIAS1 = Foo
            ALIAS2 = ALIAS1
            ALIAS2::CONST
            "
        });
        context.resolve();
        assert_no_diagnostics!(&context, &[Rule::ParseWarning]);

        assert_constant_alias_target_eq!(context, "ALIAS1", "Foo");
        assert_constant_alias_target_eq!(context, "ALIAS2", "ALIAS1");
        assert_constant_reference_to!(context, "Foo::CONST", "file:///foo.rb:6:8-6:13");
    }

    #[test]
    fn resolving_constant_alias_to_non_existent_target() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            ALIAS_1 = NonExistent
            ALIAS_2 = ALIAS_1
            "
        });
        context.resolve();
        assert_no_diagnostics!(&context);

        assert_constant_alias_target_eq!(context, "ALIAS_2", "ALIAS_1");
        assert_no_constant_alias_target!(context, "ALIAS_1");
    }

    #[test]
    fn resolving_constant_alias_to_value_in_constant_path() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            VALUE = 1
            ALIAS = VALUE
            ALIAS::NOPE
            "
        });
        context.resolve();
        assert_no_diagnostics!(&context, &[Rule::ParseWarning]);

        assert_constant_alias_target_eq!(context, "ALIAS", "VALUE");

        // NOPE can't be created because ALIAS points to a value constant, not a namespace
        assert!(
            !context
                .graph()
                .declarations()
                .contains_key(&DeclarationId::from("VALUE::NOPE"))
        );
    }

    #[test]
    fn resolving_constant_alias_defined_before_target() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            ALIAS = Foo
            module Foo
              CONST = 1
            end
            ALIAS::CONST
            "
        });
        context.resolve();
        assert_no_diagnostics!(&context, &[Rule::ParseWarning]);

        assert_constant_alias_target_eq!(context, "ALIAS", "Foo");
        assert_constant_reference_to!(context, "Foo::CONST", "file:///foo.rb:4:7-4:12");
    }

    #[test]
    fn resolving_constant_alias_to_value() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            class Foo
              CONST = 1
            end
            class Bar
              CONST = Foo::CONST
            end
            BAZ = Bar::CONST
            "
        });
        context.resolve();
        assert_no_diagnostics!(&context);

        assert_constant_alias_target_eq!(context, "BAZ", "Bar::CONST");
        assert_constant_alias_target_eq!(context, "Bar::CONST", "Foo::CONST");
    }

    #[test]
    fn resolving_circular_constant_aliases() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            A = B
            B = C
            C = A
            "
        });
        context.resolve();
        assert_no_diagnostics!(&context);

        assert_constant_alias_target_eq!(context, "A", "B");
        assert_constant_alias_target_eq!(context, "B", "C");
        assert_constant_alias_target_eq!(context, "C", "A");
    }

    #[test]
    fn resolving_circular_constant_aliases_cross_namespace() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            module A
              X = B::Y
            end
            module B
              Y = A::X
            end

            A::X::SOMETHING = 1
            "
        });
        context.resolve();
        assert_no_diagnostics!(&context);

        assert!(
            context
                .graph()
                .declarations()
                .contains_key(&DeclarationId::from("A::X"))
        );
        assert!(
            context
                .graph()
                .declarations()
                .contains_key(&DeclarationId::from("B::Y"))
        );

        // SOMETHING can't be created because the circular alias can't resolve to a namespace
        assert!(
            !context
                .graph()
                .declarations()
                .contains_key(&DeclarationId::from("A::X::SOMETHING"))
        );
    }

    #[test]
    fn resolving_constant_alias_ping_pong() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            module Left
              module Deep
                VALUE = 'left'
              end
            end

            module Right
              module Deep
                VALUE = 'right'
              end
            end

            Left::RIGHT_REF = Right
            Right::LEFT_REF = Left

            Left::RIGHT_REF::Deep::VALUE
            Left::RIGHT_REF::LEFT_REF::Deep::VALUE
            "
        });
        context.resolve();
        assert_no_diagnostics!(&context, &[Rule::ParseWarning]);

        assert_constant_alias_target_eq!(context, "Left::RIGHT_REF", "Right");
        assert_constant_alias_target_eq!(context, "Right::LEFT_REF", "Left");

        // Left::RIGHT_REF::Deep::VALUE
        assert_constant_reference_to!(context, "Right::Deep", "file:///foo.rb:15:17-15:21");
        assert_constant_reference_to!(context, "Right::Deep::VALUE", "file:///foo.rb:15:23-15:28");
        // Left::RIGHT_REF::LEFT_REF::Deep::VALUE
        assert_constant_reference_to!(context, "Left::Deep", "file:///foo.rb:16:27-16:31");
        assert_constant_reference_to!(context, "Left::Deep::VALUE", "file:///foo.rb:16:33-16:38");
    }

    #[test]
    fn resolving_constant_alias_self_referential() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            module M
              SELF_REF = M

              class Thing
                CONST = 1
              end
            end

            M::SELF_REF::Thing::CONST
            M::SELF_REF::SELF_REF::Thing::CONST
            M::SELF_REF::SELF_REF::SELF_REF::Thing::CONST
            "
        });
        context.resolve();
        assert_no_diagnostics!(&context, &[Rule::ParseWarning]);

        assert_constant_alias_target_eq!(context, "M::SELF_REF", "M");

        let m_thing_const = context
            .graph()
            .declarations()
            .get(&DeclarationId::from("M::Thing::CONST"))
            .unwrap();
        let m_thing = context
            .graph()
            .declarations()
            .get(&DeclarationId::from("M::Thing"))
            .unwrap();

        // All 3 paths resolve to M::Thing::CONST
        assert_eq!(m_thing_const.references().len(), 3);
        assert_eq!(m_thing.references().len(), 3);

        // M::SELF_REF::Thing::CONST
        assert_constant_reference_to!(context, "M::Thing", "file:///foo.rb:8:13-8:18");
        assert_constant_reference_to!(context, "M::Thing::CONST", "file:///foo.rb:8:20-8:25");
        // M::SELF_REF::SELF_REF::Thing::CONST
        assert_constant_reference_to!(context, "M::Thing", "file:///foo.rb:9:23-9:28");
        assert_constant_reference_to!(context, "M::Thing::CONST", "file:///foo.rb:9:30-9:35");
        // M::SELF_REF::SELF_REF::SELF_REF::Thing::CONST
        assert_constant_reference_to!(context, "M::Thing", "file:///foo.rb:10:33-10:38");
        assert_constant_reference_to!(context, "M::Thing::CONST", "file:///foo.rb:10:40-10:45");
    }

    #[test]
    fn resolving_class_through_constant_alias() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            module Outer
              class Inner
              end
            end

            ALIAS = Outer
            Outer::NESTED = Outer::Inner

            class ALIAS::NESTED
              ADDED_CONST = 1
            end

            Outer::Inner::ADDED_CONST
            "
        });
        context.resolve();
        assert_no_diagnostics!(&context, &[Rule::ParseWarning]);

        assert_constant_alias_target_eq!(context, "ALIAS", "Outer");
        assert_constant_alias_target_eq!(context, "Outer::NESTED", "Outer::Inner");

        // ADDED_CONST should be in Outer::Inner (the resolved target)
        assert!(
            context
                .graph()
                .declarations()
                .contains_key(&DeclarationId::from("Outer::Inner::ADDED_CONST"))
        );
        assert!(
            context
                .graph()
                .declarations()
                .contains_key(&DeclarationId::from("Outer::Inner::ADDED_CONST"))
        );

        let added_const = context
            .graph()
            .declarations()
            .get(&DeclarationId::from("Outer::Inner::ADDED_CONST"))
            .unwrap();
        assert_eq!(added_const.references().len(), 1);
    }

    #[test]
    fn resolving_class_definition_through_constant_alias() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            module Outer
              CONST = 1
            end

            ALIAS = Outer

            class ALIAS::NewClass
              CLASS_CONST = 2
            end

            Outer::NewClass::CLASS_CONST
            ALIAS::NewClass::CLASS_CONST
            "
        });
        context.resolve();
        assert_no_diagnostics!(&context, &[Rule::ParseWarning]);

        assert_constant_alias_target_eq!(context, "ALIAS", "Outer");

        // NewClass should be declared under Outer, not ALIAS
        assert!(
            context
                .graph()
                .declarations()
                .contains_key(&DeclarationId::from("Outer::NewClass"))
        );
        assert!(
            context
                .graph()
                .declarations()
                .contains_key(&DeclarationId::from("Outer::NewClass::CLASS_CONST"))
        );

        // Outer::NewClass::CLASS_CONST
        assert_constant_reference_to!(context, "Outer::NewClass", "file:///foo.rb:10:7-10:15");
        assert_constant_reference_to!(context, "Outer::NewClass::CLASS_CONST", "file:///foo.rb:10:17-10:28");
        // ALIAS::NewClass::CLASS_CONST
        assert_constant_reference_to!(context, "Outer::NewClass", "file:///foo.rb:11:7-11:15");
        assert_constant_reference_to!(context, "Outer::NewClass::CLASS_CONST", "file:///foo.rb:11:17-11:28");
    }

    #[test]
    fn resolving_constant_alias_with_multiple_definitions() {
        let mut context = GraphTest::new();
        context.index_uri("file:///a.rb", {
            r"
            module A; end
            FOO = A
            "
        });
        context.index_uri("file:///b.rb", {
            r"
            module B; end
            FOO = B
            "
        });
        context.resolve();
        assert_no_diagnostics!(&context);

        // FOO should have 2 definitions pointing to different targets
        assert_eq!(
            context
                .graph()
                .declarations()
                .get(&DeclarationId::from("FOO"))
                .unwrap()
                .definitions()
                .len(),
            2
        );

        assert_alias_targets_contain!(context, "FOO", "A", "B");
    }

    #[test]
    fn resolving_constant_alias_with_multiple_targets() {
        let mut context = GraphTest::new();
        context.index_uri("file:///a.rb", {
            r"
            module A
              CONST_A = 1
            end
            FOO = A
            "
        });
        context.index_uri("file:///b.rb", {
            r"
            module B
              CONST_B = 2
            end
            FOO = B
            "
        });
        context.index_uri("file:///usage.rb", {
            r"
            FOO::CONST_A
            FOO::CONST_B
            "
        });
        context.resolve();
        assert_no_diagnostics!(&context, &[Rule::ParseWarning]);

        // FOO::CONST_A should resolve to A::CONST_A
        assert_constant_reference_to!(context, "A::CONST_A", "file:///usage.rb:0:5-0:12");
        // FOO::CONST_B should resolve to B::CONST_B
        assert_constant_reference_to!(context, "B::CONST_B", "file:///usage.rb:1:5-1:12");
    }

    #[test]
    fn resolving_constant_alias_multi_target_with_circular() {
        let mut context = GraphTest::new();
        context.index_uri("file:///a.rb", {
            r"
            module A
              CONST = 1
            end
            ALIAS = A
            "
        });
        context.index_uri("file:///b.rb", "ALIAS = ALIAS");
        context.index_uri("file:///usage.rb", "ALIAS::CONST");
        context.resolve();
        assert_no_diagnostics!(&context, &[Rule::ParseWarning]);

        // ALIAS should have two targets: A and ALIAS (self-reference)
        assert_alias_targets_contain!(context, "ALIAS", "A", "ALIAS");

        // ALIAS::CONST should still resolve to A::CONST through the valid path
        assert_constant_reference_to!(context, "A::CONST", "file:///usage.rb:0:7-0:12");
    }

    #[test]
    fn resolving_constant_reference_through_chained_aliases() {
        let mut context = GraphTest::new();
        context.index_uri("file:///defs.rb", {
            r"
            module Foo
              CONST = 1
            end
            ALIAS1 = Foo
            ALIAS2 = ALIAS1
            "
        });
        context.index_uri("file:///usage.rb", "ALIAS2::CONST");
        context.resolve();
        assert_no_diagnostics!(&context, &[Rule::ParseWarning]);

        assert_constant_alias_target_eq!(context, "ALIAS1", "Foo");
        assert_constant_alias_target_eq!(context, "ALIAS2", "ALIAS1");

        assert_constant_reference_to!(context, "Foo::CONST", "file:///usage.rb:0:8-0:13");
    }

    #[test]
    fn resolving_constant_reference_through_top_level_alias_target() {
        let mut context = GraphTest::new();
        context.index_uri("file:///defs.rb", {
            r"
            module Foo
              CONST = 1
            end
            ALIAS = ::Foo
            "
        });
        context.index_uri("file:///usage.rb", "ALIAS::CONST");
        context.resolve();
        assert_no_diagnostics!(&context, &[Rule::ParseWarning]);

        assert_constant_reference_to!(context, "Foo::CONST", "file:///usage.rb:0:7-0:12");
    }

    // Regression test: defining singleton method on alias triggers get_or_create_singleton_class
    #[test]
    fn resolving_singleton_method_on_alias_does_not_panic() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            class Foo; end
            ALIAS = Foo
            def ALIAS.singleton_method; end
            "
        });
        context.resolve();
        assert_no_diagnostics!(&context);
    }

    #[test]
    fn multi_target_alias_constant_added_to_primary_owner() {
        let mut context = GraphTest::new();
        context.index_uri("file:///modules.rb", {
            r"
            module Foo; end
            module Bar; end
            "
        });
        context.index_uri("file:///alias1.rb", {
            r"
            ALIAS ||= Foo
            "
        });
        context.index_uri("file:///alias2.rb", {
            r"
            ALIAS ||= Bar
            "
        });
        context.index_uri("file:///const.rb", {
            r"
            ALIAS::CONST = 123
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context);

        assert_members_eq!(context, "Foo", vec!["CONST"]);
        assert_no_members!(context, "Bar");
    }

    #[test]
    fn distinct_declarations_with_conflicting_string_ids() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            class Foo
              def Array(); end
              class Array; end
            end
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context);

        // Both entries exist as unique members
        assert_members_eq!(context, "Foo", vec!["Array", "Array()"]);

        // Both declarations exist with unique IDs
        assert!(
            context
                .graph()
                .declarations()
                .get(&DeclarationId::from("Foo::Array"))
                .is_some()
        );
        assert!(
            context
                .graph()
                .declarations()
                .get(&DeclarationId::from("Foo#Array()"))
                .is_some()
        );
    }

    #[test]
    fn fully_qualified_names_are_unique() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            module Foo
              class Bar
                CONST = 1
                @class_ivar = 2

                attr_reader :baz
                attr_writer :qux
                attr_accessor :zip

                def instance_m
                  @@class_var = 3
                end

                def self.singleton_m
                  $global_var = 4
                end

                def Foo.another_singleton_m; end

                class << self
                  OTHER_CONST = 5
                  @other_class_ivar = 6
                  @@other_class_var = 7

                  def other_instance_m
                    @my_class_var = 8
                  end

                  def self.other_singleton_m
                    $other_global_var = 9
                  end
                end
              end
            end
            "
        });
        context.resolve();

        let declarations = context.graph().declarations();

        // In the same order of appearence
        assert!(declarations.contains_key(&DeclarationId::from("Foo")));
        assert!(declarations.contains_key(&DeclarationId::from("Foo::Bar")));
        assert!(declarations.contains_key(&DeclarationId::from("Foo::Bar::CONST")));
        assert!(declarations.contains_key(&DeclarationId::from("Foo::Bar::<Bar>#@class_ivar")));
        assert!(declarations.contains_key(&DeclarationId::from("Foo::Bar#baz()")));
        // TODO: needs the fix for attributes
        // assert!(declarations.contains_key(&DeclarationId::from("Foo::Bar#qux=()")));
        assert!(declarations.contains_key(&DeclarationId::from("Foo::Bar#zip()")));
        // TODO: needs the fix for attributes
        // assert!(declarations.contains_key(&DeclarationId::from("Foo::Bar#zip=()")));
        assert!(declarations.contains_key(&DeclarationId::from("Foo::Bar#instance_m()")));
        assert!(declarations.contains_key(&DeclarationId::from("Foo::Bar#@@class_var")));
        assert!(declarations.contains_key(&DeclarationId::from("Foo::Bar::<Bar>#singleton_m()")));
        assert!(declarations.contains_key(&DeclarationId::from("$global_var")));
        assert!(declarations.contains_key(&DeclarationId::from("Foo::<Foo>#another_singleton_m()")));
        assert!(declarations.contains_key(&DeclarationId::from("Foo::Bar::<Bar>::OTHER_CONST")));
        assert!(declarations.contains_key(&DeclarationId::from("Foo::Bar::<Bar>::<<Bar>>#@other_class_ivar")));
        assert!(declarations.contains_key(&DeclarationId::from("Foo::Bar#@@other_class_var")));
        assert!(declarations.contains_key(&DeclarationId::from("Foo::Bar::<Bar>#other_instance_m()")));
        assert!(declarations.contains_key(&DeclarationId::from("Foo::Bar::<Bar>#@my_class_var")));
        assert!(declarations.contains_key(&DeclarationId::from("Foo::Bar::<Bar>::<<Bar>>#other_singleton_m()")));
        assert!(declarations.contains_key(&DeclarationId::from("$other_global_var")));
    }

    #[test]
    fn test_nested_same_names() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
              module Foo; end

              module Bar
                Foo

                module Foo
                  FOO = 42
                end
              end
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context, &[Rule::ParseWarning]);

        // FIXME: this is wrong, the reference is not to `Bar::Foo`, but to `Foo`
        assert_constant_reference_to!(context, "Bar::Foo", "file:///foo.rb:3:2-3:5");

        assert_ancestors_eq!(context, "Foo", &["Foo"]);
        assert_ancestors_eq!(context, "Bar::Foo", &["Bar::Foo"]);

        assert_no_members!(context, "Foo");
        assert_members_eq!(context, "Bar::Foo", vec!["FOO"]);
    }

    #[test]
    fn test_minitest() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            class Foo::Bar
              def foo; end
            end
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context);

        println!(
            "{:?}",
            context
                .graph()
                .declarations()
                .values()
                .collect::<Vec<_>>()
                .iter()
                .map(|d| d.name())
                .collect::<Vec<_>>()
        );

        assert_members_eq!(context, "Foo::Bar", vec!["foo()"]);
    }
}
