use crate::model::definitions::Definition;
use crate::model::graph::Graph;

fn fully_qualify_definition_name(graph: &Graph, definition: &Definition) -> String {
    let definition_string_id = graph.definition_string_id(definition);
    let definition_name = graph.strings().get(&definition_string_id).unwrap();

    if let Some(fully_qualified_name) = definition_name.strip_prefix("::") {
        return fully_qualified_name.to_string();
    }

    match definition.owner_id() {
        Some(owner_id) => {
            let owner_definition = graph.definitions().get(owner_id).unwrap();
            let owner_fully_qualified_name = fully_qualify_definition_name(graph, owner_definition);
            format!("{owner_fully_qualified_name}::{definition_name}")
        }
        None => definition_name.clone(),
    }
}

/// # Panics
///
/// This will panic if the graph is not valid
pub fn create_declarations_for_definitions(graph: &mut Graph) {
    let definition_ids = graph.definitions().keys().copied().collect::<Vec<_>>();

    for definition_id in definition_ids {
        let definition = graph.definitions().get(&definition_id).unwrap();
        let definition_name_id = graph.definition_string_id(definition);
        let definition_name = graph.strings().get(&definition_name_id).unwrap();

        if let Some(declaration_name) = definition_name.strip_prefix("::") {
            // Handle top level declarations like these ones:
            // ```ruby
            // module ::Foo; end
            //
            // module Foo
            //   class ::Bar; end
            // end
            // ```
            graph.add_declaration(declaration_name.to_string(), definition_id);
        } else if definition_name.split("::").count() > 1 {
            // TODO: handle complex declarations with path names like `Foo::Bar::Baz`
        } else {
            // Handle simple nested declarations like these ones:
            // ```ruby
            // module Foo; end
            //
            // module Foo
            //   class Bar; end
            // end
            // ```
            let fully_qualified_name = fully_qualify_definition_name(graph, definition);
            graph.add_declaration(fully_qualified_name, definition_id);
        }
    }
}

fn compute_declaration_members_and_ownership(graph: &mut Graph) {
    let declaration_ids = graph.declarations().keys().copied().collect::<Vec<_>>();

    for declaration_id in declaration_ids {
        let definition_ids = graph
            .get_declaration_mut(&declaration_id)
            .unwrap()
            .definitions()
            .to_vec();

        // TODO error if two definitions for the same declaration have different owners

        for definition_id in definition_ids {
            if let Some(definition_owner_id) = graph.definitions().get(&definition_id).unwrap().owner_id() {
                let declaration_owner_id = *graph.definitions_to_declarations().get(definition_owner_id).unwrap();
                let definition = graph.definitions().get(&definition_id).unwrap();
                let member_name_id = graph.definition_string_id(definition);
                let declaration_owner = graph.get_declaration_mut(&declaration_owner_id).unwrap();

                declaration_owner.add_member(member_name_id, declaration_id);
                graph
                    .get_declaration_mut(&declaration_id)
                    .unwrap()
                    .set_owner_id(declaration_owner_id);
            }
        }
    }
}

/// # Panics
///
/// This will panic if the graph is not valid
pub fn resolve(graph: &mut Graph) {
    graph.clear_declarations();

    create_declarations_for_definitions(graph);

    compute_declaration_members_and_ownership(graph);
}

#[cfg(test)]
mod tests {
    use crate::test_utils::GraphTest;

    #[test]
    fn create_declarations_for_top_level_definitions_with_root_prefix() {
        let mut context = GraphTest::new();

        context.index_uri("file:///foo1.rb", "module ::Foo; end");
        context.index_uri("file:///foo2.rb", "module ::Foo; end");

        context.resolve();

        assert_eq!(context.graph.get("Foo").unwrap().len(), 2);
    }

    #[test]
    fn create_declarations_for_top_level_definitions() {
        let mut context = GraphTest::new();

        context.index_uri("file:///foo1.rb", "module Foo; end");
        context.index_uri("file:///foo2.rb", "module Foo; end");

        context.resolve();

        assert_eq!(context.graph.get("Foo").unwrap().len(), 2);
    }

    #[test]
    fn create_declarations_for_nested_definitions() {
        let mut context = GraphTest::new();

        context.index_uri("file:///foo1.rb", {
            "
            module Foo
              class Bar; end
            end
            "
        });
        context.index_uri("file:///foo2.rb", {
            "
            module Foo
              class Bar; end
            end
            "
        });

        context.resolve();

        assert_eq!(context.graph.get("Foo").unwrap().len(), 2);
        assert_eq!(context.graph.get("Foo::Bar").unwrap().len(), 2);
    }

    #[test]
    fn delete_all_declarations_when_resolving() {
        let mut context = GraphTest::new();

        context.index_uri("file:///foo1.rb", "class Foo; end");
        context.index_uri("file:///foo2.rb", "class Bar; end");

        context.resolve();

        assert_eq!(context.graph.get("Foo").unwrap().len(), 1);
        assert_eq!(context.graph.get("Bar").unwrap().len(), 1);

        context.index_uri("file:///foo2.rb", "");

        context.resolve();

        assert_eq!(context.graph.get("Foo").unwrap().len(), 1);
        assert!(context.graph.get("Bar").is_none());

        context.delete_uri("file:///foo1.rb");
        context.delete_uri("file:///foo2.rb");

        context.resolve();

        assert!(context.graph.get("Foo").is_none());
        assert!(context.graph.get("Bar").is_none());
    }

    #[test]
    fn create_declarations_for_qualified_nested_definitions() {
        let mut context = GraphTest::new();

        context.index_uri("file:///foo1.rb", {
            "
            module Foo
              class Bar::Baz; end
            end
            "
        });

        context.resolve();

        assert_eq!(context.graph.get("Foo").unwrap().len(), 1);
        assert!(context.graph.get("Foo::Bar").is_none());
        assert!(context.graph.get("Foo::Bar::Baz").is_none());

        context.index_uri("file:///foo2.rb", {
            "
            module Bar; end
            "
        });

        context.resolve();

        assert_eq!(context.graph.get("Foo").unwrap().len(), 1);
        assert_eq!(context.graph.get("Bar").unwrap().len(), 1);

        // TODO: this case requires constant resolution to be implemented
        // assert_eq!(context.graph.get("Bar::Baz").unwrap().len(), 1);
        assert!(context.graph.get("Bar::Baz").is_none());
        assert!(context.graph.get("Foo::Bar::Baz").is_none());

        context.delete_uri("file:///foo2.rb");

        context.resolve();

        assert_eq!(context.graph.get("Foo").unwrap().len(), 1);
        assert!(context.graph.get("Foo::Bar").is_none());
        assert!(context.graph.get("Foo::Bar::Baz").is_none());
        assert!(context.graph.get("Bar").is_none());
        assert!(context.graph.get("Bar::Baz").is_none());
    }
}
