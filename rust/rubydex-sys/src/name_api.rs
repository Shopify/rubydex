use rubydex::model::{graph::Graph, ids::NameId, name::ParentScope};

/// Takes a constant name and a nesting stack (e.g.: `["Foo", "Bar::Baz", "Qux"]`) and transforms it into a `NameId`,
/// registering each required part in the graph. Returns the `NameId` and a list of name ids that need to be untracked
/// afterwards. Returns `None` if the constant name contains no valid identifier parts (e.g.: `""`, `"::"`, `"Foo::"`).
pub fn nesting_stack_to_name_id(
    graph: &mut Graph,
    const_name: &str,
    nesting: Vec<String>,
) -> Option<(NameId, Vec<NameId>)> {
    let mut current_nesting = None;
    let mut current_name = ParentScope::None;
    let mut names_to_untrack = Vec::new();

    for entry in nesting {
        process_qualified_name(graph, &entry, current_nesting, &mut current_name, &mut names_to_untrack);
        current_nesting = current_name.as_ref().copied();
        current_name = ParentScope::None;
    }

    process_qualified_name(
        graph,
        const_name,
        current_nesting,
        &mut current_name,
        &mut names_to_untrack,
    );

    let (ParentScope::Some(name_id) | ParentScope::Attached(name_id)) = current_name else {
        // Invalid names may leave temporary parts in the graph (e.g. `Foo` from `Foo::`).
        for name_id in names_to_untrack {
            graph.untrack_name(name_id);
        }
        return None;
    };

    Some((name_id, names_to_untrack))
}

/// Takes a constant name and an existing lexical nesting and transforms it into a `NameId`, registering each required
/// part in the graph. Returns the `NameId` and a list of name ids that need to be untracked afterwards.
pub fn name_in_nesting_to_name_id(
    graph: &mut Graph,
    const_name: &str,
    nesting: Option<NameId>,
) -> Option<(NameId, Vec<NameId>)> {
    let mut current_name = ParentScope::None;
    let mut names_to_untrack = Vec::new();

    process_qualified_name(graph, const_name, nesting, &mut current_name, &mut names_to_untrack);

    let (ParentScope::Some(name_id) | ParentScope::Attached(name_id)) = current_name else {
        // Invalid names may leave temporary parts in the graph (e.g. `Foo` from `Foo::`).
        for name_id in names_to_untrack {
            graph.untrack_name(name_id);
        }
        return None;
    };

    Some((name_id, names_to_untrack))
}

/// Processes a qualified name (e.g., `"Foo::Bar"` or `"<Foo>"`) by splitting on `"::"` and registering each part in the
/// graph. Singleton class names (starting with `<`) use `ParentScope::Attached` and a `nesting` equal to the attached
/// target, matching how the indexer creates them (`class << self` always sits lexically inside its attached class).
/// When a singleton is the first part (i.e., `current_name` has no parent), `current_nesting` is used as the attachment
/// point.
fn process_qualified_name(
    graph: &mut Graph,
    qualified_name: &str,
    current_nesting: Option<NameId>,
    current_name: &mut ParentScope,
    names_to_untrack: &mut Vec<NameId>,
) {
    for part in qualified_name.split("::") {
        if part.is_empty() {
            *current_name = ParentScope::TopLevel;
            continue;
        }

        let (parent_scope, nesting_for_part) = if part.starts_with('<') {
            let attached_id = match *current_name {
                ParentScope::Some(id) | ParentScope::Attached(id) => Some(id),
                _ => current_nesting,
            };

            let attached = attached_id.map_or(ParentScope::None, ParentScope::Attached);
            (attached, attached_id)
        } else {
            (*current_name, current_nesting)
        };

        let str_id = graph.intern_string(part.to_owned());
        let name_id = graph.add_name(str_id, parent_scope, nesting_for_part);
        names_to_untrack.push(name_id);
        *current_name = ParentScope::Some(name_id);
    }
}

#[cfg(test)]
mod tests {
    use rubydex::model::ids::StringId;

    use super::*;

    #[test]
    fn nesting_is_converted_to_name_id() {
        let mut graph = Graph::new();

        let (name_id, _) = nesting_stack_to_name_id(
            &mut graph,
            "Some::CONST",
            vec!["Foo".into(), "Bar::Zip".into(), "Qux".into()],
        )
        .unwrap();

        let const_name = graph.names().get(&name_id).unwrap();
        assert_eq!(StringId::from("CONST"), *const_name.str());

        let some_name = graph
            .names()
            .get(&const_name.parent_scope().expect("Parent scope should exist"))
            .unwrap();
        assert_eq!(StringId::from("Some"), *some_name.str());
        assert_eq!(const_name.nesting(), some_name.nesting());

        let qux_name = graph.names().get(&some_name.nesting().unwrap()).unwrap();
        assert_eq!(StringId::from("Qux"), *qux_name.str());
        assert!(qux_name.parent_scope().is_none());

        let zip_name = graph.names().get(&qux_name.nesting().unwrap()).unwrap();
        assert_eq!(StringId::from("Zip"), *zip_name.str());

        let bar_name = graph
            .names()
            .get(&zip_name.parent_scope().expect("Parent scope should exist"))
            .unwrap();
        assert_eq!(StringId::from("Bar"), *bar_name.str());
        assert_eq!(zip_name.nesting(), bar_name.nesting());

        let foo_name = graph.names().get(&bar_name.nesting().unwrap()).unwrap();
        assert_eq!(StringId::from("Foo"), *foo_name.str());
        assert!(foo_name.parent_scope().is_none());
        assert!(foo_name.nesting().is_none());
    }

    #[test]
    fn top_level_reference_is_converted_to_name_id() {
        let mut graph = Graph::new();

        let (name_id, _) = nesting_stack_to_name_id(&mut graph, "::CONST", vec!["Foo".into()]).unwrap();

        let const_name = graph.names().get(&name_id).unwrap();
        assert_eq!(StringId::from("CONST"), *const_name.str());
        assert!(const_name.parent_scope().is_top_level());

        let foo_name = graph.names().get(&const_name.nesting().unwrap()).unwrap();
        assert_eq!(StringId::from("Foo"), *foo_name.str());
        assert!(foo_name.nesting().is_none());
        assert!(foo_name.parent_scope().is_none());
    }

    #[test]
    fn singleton_class_names_use_attached_parent_scope() {
        let mut graph = Graph::new();

        let (name_id, _) = nesting_stack_to_name_id(&mut graph, "CONST", vec!["Foo".into(), "<Foo>".into()]).unwrap();

        let const_name = graph.names().get(&name_id).unwrap();
        assert_eq!(StringId::from("CONST"), *const_name.str());

        // The nesting should be <Foo> with an Attached parent scope
        let singleton_name = graph.names().get(&const_name.nesting().unwrap()).unwrap();
        assert_eq!(StringId::from("<Foo>"), *singleton_name.str());
        assert!(
            matches!(singleton_name.parent_scope(), ParentScope::Attached(_)),
            "Expected ParentScope::Attached, got {}",
            singleton_name.parent_scope()
        );

        // The attached parent should be Foo
        let foo_id = singleton_name.parent_scope().expect("Attached should have an id");
        let foo_name = graph.names().get(&foo_id).unwrap();
        assert_eq!(StringId::from("Foo"), *foo_name.str());
    }

    #[test]
    fn top_level_nesting_is_converted_to_name_id() {
        let mut graph = Graph::new();

        let (name_id, _) = nesting_stack_to_name_id(&mut graph, "CONST", vec!["Foo".into(), "::Bar".into()]).unwrap();

        let const_name = graph.names().get(&name_id).unwrap();
        assert_eq!(StringId::from("CONST"), *const_name.str());
        assert!(const_name.parent_scope().is_none());

        let bar_name = graph.names().get(&const_name.nesting().unwrap()).unwrap();
        assert_eq!(StringId::from("Bar"), *bar_name.str());
        assert!(bar_name.parent_scope().is_top_level());

        let foo_name = graph.names().get(&bar_name.nesting().unwrap()).unwrap();
        assert_eq!(StringId::from("Foo"), *foo_name.str());
        assert!(foo_name.parent_scope().is_none());
        assert!(foo_name.nesting().is_none());
    }

    #[test]
    fn invalid_names_are_untracked() {
        let mut graph = Graph::new();
        let name_count = graph.names().len();
        let string_count = graph.strings().len();

        assert!(nesting_stack_to_name_id(&mut graph, "Foo::", vec!["Bar".into()]).is_none());
        assert_eq!(name_count, graph.names().len());
        assert_eq!(string_count, graph.strings().len());

        assert!(name_in_nesting_to_name_id(&mut graph, "Foo::", None).is_none());
        assert_eq!(name_count, graph.names().len());
        assert_eq!(string_count, graph.strings().len());
    }
}
