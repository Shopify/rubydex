use rubydex::model::{graph::Graph, ids::NameId, name::Name};

/// Takes a constant name and a nesting stack (e.g.: `["Foo", "Bar::Baz", "Qux"]`) and transforms it into a `NameId`,
/// registering each required part in the graph. Returns the `NameId` and a list of name ids that need to be untracked
/// afterwards
///
/// # Panics
///
/// Should not panic because `const_name` will always be turned into a name
pub fn nesting_stack_to_name_id(graph: &mut Graph, const_name: &str, nesting: Vec<String>) -> (NameId, Vec<NameId>) {
    let mut current_nesting = None;
    let mut current_name = None;
    let mut names_to_untrack = Vec::new();

    for entry in nesting {
        for part in entry.split("::").map(String::from) {
            let str_id = graph.intern_string(part);
            let name_id = graph.add_name(Name::new(str_id, current_name, current_nesting));
            names_to_untrack.push(name_id);
            let new_name = Some(name_id);
            current_name = new_name;
        }

        current_nesting = current_name;
        current_name = None;
    }

    for part in const_name.split("::").map(String::from) {
        let str_id = graph.intern_string(part);
        let name_id = graph.add_name(Name::new(str_id, current_name, current_nesting));
        names_to_untrack.push(name_id);
        let new_name = Some(name_id);
        current_name = new_name;
    }

    (
        current_name.expect("The NameId cannot be None since it contains at least `const_name`"),
        names_to_untrack,
    )
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
        );

        let const_name = graph.names().get(&name_id).unwrap();
        assert_eq!(StringId::from("CONST"), *const_name.str());

        let some_name = graph.names().get(&const_name.parent_scope().unwrap()).unwrap();
        assert_eq!(StringId::from("Some"), *some_name.str());
        assert_eq!(const_name.nesting(), some_name.nesting());

        let qux_name = graph.names().get(&some_name.nesting().unwrap()).unwrap();
        assert_eq!(StringId::from("Qux"), *qux_name.str());
        assert!(qux_name.parent_scope().is_none());

        let zip_name = graph.names().get(&qux_name.nesting().unwrap()).unwrap();
        assert_eq!(StringId::from("Zip"), *zip_name.str());

        let bar_name = graph.names().get(&zip_name.parent_scope().unwrap()).unwrap();
        assert_eq!(StringId::from("Bar"), *bar_name.str());
        assert_eq!(zip_name.nesting(), bar_name.nesting());

        let foo_name = graph.names().get(&bar_name.nesting().unwrap()).unwrap();
        assert_eq!(StringId::from("Foo"), *foo_name.str());
        assert!(foo_name.parent_scope().is_none());
        assert!(foo_name.nesting().is_none());
    }
}
