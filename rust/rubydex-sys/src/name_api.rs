use rubydex::model::{graph::Graph, ids::NameId, name::ParentScope};

struct NameTracker<'graph> {
    graph: &'graph mut Graph,
    tracked_name_ids: Vec<NameId>,
}

impl<'graph> NameTracker<'graph> {
    fn new(graph: &'graph mut Graph) -> Self {
        Self {
            graph,
            tracked_name_ids: Vec::new(),
        }
    }

    fn track_name(&mut self, string: &str, parent_scope: ParentScope, nesting: Option<NameId>) -> NameId {
        let str_id = self.graph.intern_string(string.to_owned());
        let name_id = self.graph.add_name(str_id, parent_scope, nesting);
        self.tracked_name_ids.push(name_id);
        name_id
    }
}

impl Drop for NameTracker<'_> {
    fn drop(&mut self) {
        for name_id in self.tracked_name_ids.drain(..).rev() {
            self.graph.untrack_name(name_id);
        }
    }
}

/// A name temporarily tracked in a graph. Dropping this guard undoes every name and string reference-count increment
/// made while building the name.
pub struct ScopedName<'graph> {
    tracker: NameTracker<'graph>,
    name_id: NameId,
}

impl<'graph> ScopedName<'graph> {
    fn new(tracker: NameTracker<'graph>, name_id: NameId) -> Self {
        ScopedName { tracker, name_id }
    }

    #[must_use]
    pub fn name_id(&self) -> NameId {
        self.name_id
    }

    #[must_use]
    pub fn graph(&self) -> &Graph {
        &*self.tracker.graph
    }

    pub fn graph_mut(&mut self) -> &mut Graph {
        &mut *self.tracker.graph
    }
}

/// Takes a constant name and a nesting stack (e.g.: `["Foo", "Bar::Baz", "Qux"]`) and transforms it into a scoped
/// `NameId`, registering each required part in the graph. The names are untracked when the returned guard is dropped.
/// Returns `None` if the constant name contains no valid identifier parts (e.g.: `""`, `"::"`, `"Foo::"`).
pub fn nesting_stack_to_scoped_name<'graph>(
    graph: &'graph mut Graph,
    const_name: &str,
    nesting: Vec<String>,
) -> Option<ScopedName<'graph>> {
    let mut current_nesting = None;
    let mut current_name = ParentScope::None;
    let mut tracker = NameTracker::new(graph);

    for entry in nesting {
        process_qualified_name(&mut tracker, &entry, current_nesting, &mut current_name);
        current_nesting = current_name.as_ref().copied();
        current_name = ParentScope::None;
    }

    process_qualified_name(&mut tracker, const_name, current_nesting, &mut current_name);

    let (ParentScope::Some(name_id) | ParentScope::Attached(name_id)) = current_name else {
        return None;
    };

    Some(ScopedName::new(tracker, name_id))
}

/// Takes a constant name and an existing lexical nesting and transforms it into a scoped `NameId`, registering each
/// required part in the graph. The names are untracked when the returned guard is dropped.
pub fn name_in_nesting_to_scoped_name<'graph>(
    graph: &'graph mut Graph,
    const_name: &str,
    nesting: Option<NameId>,
) -> Option<ScopedName<'graph>> {
    let mut current_name = ParentScope::None;
    let mut tracker = NameTracker::new(graph);

    process_qualified_name(&mut tracker, const_name, nesting, &mut current_name);

    let (ParentScope::Some(name_id) | ParentScope::Attached(name_id)) = current_name else {
        return None;
    };

    Some(ScopedName::new(tracker, name_id))
}

/// Processes a qualified name (e.g., `"Foo::Bar"` or `"<Foo>"`) by splitting on `"::"` and registering each part in the
/// graph. Singleton class names (starting with `<`) use `ParentScope::Attached` and a `nesting` equal to the attached
/// target, matching how the indexer creates them (`class << self` always sits lexically inside its attached class).
/// When a singleton is the first part (i.e., `current_name` has no parent), `current_nesting` is used as the attachment
/// point.
fn process_qualified_name(
    tracker: &mut NameTracker<'_>,
    qualified_name: &str,
    current_nesting: Option<NameId>,
    current_name: &mut ParentScope,
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

        let name_id = tracker.track_name(part, parent_scope, nesting_for_part);
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

        let scoped_name = nesting_stack_to_scoped_name(
            &mut graph,
            "Some::CONST",
            vec!["Foo".into(), "Bar::Zip".into(), "Qux".into()],
        )
        .unwrap();
        let name_id = scoped_name.name_id();

        let const_name = scoped_name.graph().names().get(&name_id).unwrap();
        assert_eq!(StringId::from("CONST"), *const_name.str());

        let some_name = scoped_name
            .graph()
            .names()
            .get(&const_name.parent_scope().expect("Parent scope should exist"))
            .unwrap();
        assert_eq!(StringId::from("Some"), *some_name.str());
        assert_eq!(const_name.nesting(), some_name.nesting());

        let qux_name = scoped_name.graph().names().get(&some_name.nesting().unwrap()).unwrap();
        assert_eq!(StringId::from("Qux"), *qux_name.str());
        assert!(qux_name.parent_scope().is_none());

        let zip_name = scoped_name.graph().names().get(&qux_name.nesting().unwrap()).unwrap();
        assert_eq!(StringId::from("Zip"), *zip_name.str());

        let bar_name = scoped_name
            .graph()
            .names()
            .get(&zip_name.parent_scope().expect("Parent scope should exist"))
            .unwrap();
        assert_eq!(StringId::from("Bar"), *bar_name.str());
        assert_eq!(zip_name.nesting(), bar_name.nesting());

        let foo_name = scoped_name.graph().names().get(&bar_name.nesting().unwrap()).unwrap();
        assert_eq!(StringId::from("Foo"), *foo_name.str());
        assert!(foo_name.parent_scope().is_none());
        assert!(foo_name.nesting().is_none());
    }

    #[test]
    fn top_level_reference_is_converted_to_name_id() {
        let mut graph = Graph::new();

        let scoped_name = nesting_stack_to_scoped_name(&mut graph, "::CONST", vec!["Foo".into()]).unwrap();
        let name_id = scoped_name.name_id();

        let const_name = scoped_name.graph().names().get(&name_id).unwrap();
        assert_eq!(StringId::from("CONST"), *const_name.str());
        assert!(const_name.parent_scope().is_top_level());

        let foo_name = scoped_name.graph().names().get(&const_name.nesting().unwrap()).unwrap();
        assert_eq!(StringId::from("Foo"), *foo_name.str());
        assert!(foo_name.nesting().is_none());
        assert!(foo_name.parent_scope().is_none());
    }

    #[test]
    fn singleton_class_names_use_attached_parent_scope() {
        let mut graph = Graph::new();

        let scoped_name =
            nesting_stack_to_scoped_name(&mut graph, "CONST", vec!["Foo".into(), "<Foo>".into()]).unwrap();
        let name_id = scoped_name.name_id();

        let const_name = scoped_name.graph().names().get(&name_id).unwrap();
        assert_eq!(StringId::from("CONST"), *const_name.str());

        // The nesting should be <Foo> with an Attached parent scope
        let singleton_name = scoped_name.graph().names().get(&const_name.nesting().unwrap()).unwrap();
        assert_eq!(StringId::from("<Foo>"), *singleton_name.str());
        assert!(
            matches!(singleton_name.parent_scope(), ParentScope::Attached(_)),
            "Expected ParentScope::Attached, got {}",
            singleton_name.parent_scope()
        );

        // The attached parent should be Foo
        let foo_id = singleton_name.parent_scope().expect("Attached should have an id");
        let foo_name = scoped_name.graph().names().get(&foo_id).unwrap();
        assert_eq!(StringId::from("Foo"), *foo_name.str());
    }

    #[test]
    fn top_level_nesting_is_converted_to_name_id() {
        let mut graph = Graph::new();

        let scoped_name =
            nesting_stack_to_scoped_name(&mut graph, "CONST", vec!["Foo".into(), "::Bar".into()]).unwrap();
        let name_id = scoped_name.name_id();

        let const_name = scoped_name.graph().names().get(&name_id).unwrap();
        assert_eq!(StringId::from("CONST"), *const_name.str());
        assert!(const_name.parent_scope().is_none());

        let bar_name = scoped_name.graph().names().get(&const_name.nesting().unwrap()).unwrap();
        assert_eq!(StringId::from("Bar"), *bar_name.str());
        assert!(bar_name.parent_scope().is_top_level());

        let foo_name = scoped_name.graph().names().get(&bar_name.nesting().unwrap()).unwrap();
        assert_eq!(StringId::from("Foo"), *foo_name.str());
        assert!(foo_name.parent_scope().is_none());
        assert!(foo_name.nesting().is_none());
    }

    #[test]
    fn invalid_names_restore_name_and_string_counts() {
        let mut graph = Graph::new();
        let name_count = graph.names().len();
        let string_count = graph.strings().len();

        assert!(nesting_stack_to_scoped_name(&mut graph, "", vec!["Foo".into()]).is_none());
        assert_eq!(name_count, graph.names().len());
        assert_eq!(string_count, graph.strings().len());

        assert!(nesting_stack_to_scoped_name(&mut graph, "Foo::", vec!["Bar".into()]).is_none());
        assert_eq!(name_count, graph.names().len());
        assert_eq!(string_count, graph.strings().len());

        assert!(name_in_nesting_to_scoped_name(&mut graph, "Foo::", None).is_none());
        assert_eq!(name_count, graph.names().len());
        assert_eq!(string_count, graph.strings().len());
    }

    #[test]
    fn invalid_name_restores_existing_name_reference_counts() {
        let mut graph = Graph::new();
        let string_id = graph.intern_string("Foo".to_owned());
        let name_id = graph.add_name(string_id, ParentScope::None, None);
        let name_ref_count = graph.names().get(&name_id).unwrap().ref_count();
        let string_ref_count = graph.strings().get(&string_id).unwrap().ref_count();

        assert!(nesting_stack_to_scoped_name(&mut graph, "", vec!["Foo".into()]).is_none());

        assert_eq!(name_ref_count, graph.names().get(&name_id).unwrap().ref_count());
        assert_eq!(string_ref_count, graph.strings().get(&string_id).unwrap().ref_count());
    }

    #[test]
    fn tracker_preserves_duplicate_name_ids() {
        let mut graph = Graph::new();
        let name_count = graph.names().len();
        let string_count = graph.strings().len();

        {
            let mut tracker = NameTracker::new(&mut graph);
            let first_id = tracker.track_name("Foo", ParentScope::None, None);
            let second_id = tracker.track_name("Foo", ParentScope::None, None);

            assert_eq!(first_id, second_id);
            assert_eq!(2, tracker.graph.names().get(&first_id).unwrap().ref_count());
        }

        assert_eq!(name_count, graph.names().len());
        assert_eq!(string_count, graph.strings().len());
    }

    #[test]
    fn scoped_name_is_untracked_when_scope_ends() {
        let mut graph = Graph::new();
        let name_count = graph.names().len();
        let string_count = graph.strings().len();

        {
            let scoped_name = nesting_stack_to_scoped_name(&mut graph, "Bar::CONST", vec!["Foo".into()]).unwrap();
            let name_id = scoped_name.name_id();
            let const_name = scoped_name.graph().names().get(&name_id).unwrap();

            assert_eq!(
                "CONST",
                scoped_name.graph().strings().get(const_name.str()).unwrap().as_str()
            );
            assert_eq!(name_count + 3, scoped_name.graph().names().len());
            assert_eq!(string_count + 3, scoped_name.graph().strings().len());
        }

        assert_eq!(name_count, graph.names().len());
        assert_eq!(string_count, graph.strings().len());
    }

    #[test]
    fn scoped_name_is_untracked_during_unwind() {
        let mut graph = Graph::new();
        let name_count = graph.names().len();
        let string_count = graph.strings().len();

        let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
            let scoped_name = nesting_stack_to_scoped_name(&mut graph, "Bar::CONST", vec!["Foo".into()]).unwrap();

            assert_eq!(name_count + 3, scoped_name.graph().names().len());
            panic!("unwind scoped name");
        }));

        assert!(result.is_err());
        assert_eq!(name_count, graph.names().len());
        assert_eq!(string_count, graph.strings().len());
    }
}
