use crate::{
    assert_mem_size,
    model::{
        definitions::DefinitionFlags,
        ids::{DeferredCallId, NameId, StringId, UriId, deferred_call_id},
    },
    offset::Offset,
};

#[derive(Debug)]
pub enum DeferredArgument {
    LiteralName(StringId),
    Unsupported,
}
assert_mem_size!(DeferredArgument, 8);

#[derive(Debug)]
pub struct DeferredConstantAssignment {
    name_id: NameId,
    offset: Offset,
    name_offset: Offset,
    flags: DefinitionFlags,
}
assert_mem_size!(DeferredConstantAssignment, 32);

impl DeferredConstantAssignment {
    #[must_use]
    pub fn new(name_id: NameId, offset: Offset, name_offset: Offset, flags: DefinitionFlags) -> Self {
        Self {
            name_id,
            offset,
            name_offset,
            flags,
        }
    }

    #[must_use]
    pub fn name_id(&self) -> NameId {
        self.name_id
    }

    #[must_use]
    pub fn offset(&self) -> &Offset {
        &self.offset
    }

    #[must_use]
    pub fn name_offset(&self) -> &Offset {
        &self.name_offset
    }

    #[must_use]
    pub fn flags(&self) -> DefinitionFlags {
        self.flags.clone()
    }
}

#[derive(Debug)]
pub struct DeferredCall {
    id: DeferredCallId,
    uri_id: UriId,
    offset: Offset,
    receiver_name_id: NameId,
    method_name: StringId,
    assignment: DeferredConstantAssignment,
    arguments: Box<[DeferredArgument]>,
}
assert_mem_size!(DeferredCall, 88);

impl DeferredCall {
    #[must_use]
    pub fn new(
        uri_id: UriId,
        offset: Offset,
        receiver_name_id: NameId,
        method_name: StringId,
        assignment: DeferredConstantAssignment,
        arguments: Box<[DeferredArgument]>,
    ) -> Self {
        let id = deferred_call_id(uri_id, &offset);
        Self {
            id,
            uri_id,
            offset,
            receiver_name_id,
            method_name,
            assignment,
            arguments,
        }
    }

    #[must_use]
    pub fn id(&self) -> DeferredCallId {
        self.id
    }

    #[must_use]
    pub fn uri_id(&self) -> UriId {
        self.uri_id
    }

    #[must_use]
    pub fn offset(&self) -> &Offset {
        &self.offset
    }

    #[must_use]
    pub fn receiver_name_id(&self) -> NameId {
        self.receiver_name_id
    }

    #[must_use]
    pub fn method_name(&self) -> StringId {
        self.method_name
    }

    #[must_use]
    pub fn assignment(&self) -> &DeferredConstantAssignment {
        &self.assignment
    }

    #[must_use]
    pub fn arguments(&self) -> &[DeferredArgument] {
        &self.arguments
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CompilerKind {
    StructNew,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DeferredCallResolution {
    Pending,
    Fallback,
    Compiler(CompilerKind),
}

#[derive(Debug)]
pub(crate) struct DeferredCallResult {
    resolution: DeferredCallResolution,
    expansion: Box<[crate::operation::Operation]>,
    /// Extra `name_dependents` edges beyond the receiver along the alias path.
    dependency_names: Box<[NameId]>,
}
assert_mem_size!(DeferredCallResult, 40);

impl DeferredCallResult {
    #[must_use]
    pub(crate) fn new(
        resolution: DeferredCallResolution,
        expansion: Box<[crate::operation::Operation]>,
        dependency_names: Box<[NameId]>,
    ) -> Self {
        Self {
            resolution,
            expansion,
            dependency_names,
        }
    }

    #[must_use]
    pub(crate) fn resolution(&self) -> DeferredCallResolution {
        self.resolution
    }

    #[must_use]
    #[cfg_attr(not(test), allow(dead_code))]
    pub(crate) fn expansion(&self) -> &[crate::operation::Operation] {
        &self.expansion
    }

    #[must_use]
    pub(crate) fn dependency_names(&self) -> &[NameId] {
        &self.dependency_names
    }

    #[cfg_attr(not(test), allow(dead_code))]
    pub(crate) fn take_expansion(&mut self) -> Box<[crate::operation::Operation]> {
        std::mem::replace(&mut self.expansion, Box::from([]))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        indexing::{IndexerBackend, LanguageId, build_local_graph},
        integrity,
        model::{graph::Graph, ids::DeferredCallId},
        operation::Operation,
        resolution::Resolver,
    };

    fn index(graph: &mut Graph, uri: &str, source: &str) {
        let local_graph = build_local_graph(
            uri.to_string(),
            source,
            &LanguageId::Ruby,
            IndexerBackend::OperationBuilder,
        );
        graph.consume_document_changes(local_graph);
    }

    fn resolve(graph: &mut Graph) {
        Resolver::new(graph).resolve();
    }

    fn only_call_id(graph: &Graph) -> DeferredCallId {
        assert_eq!(graph.deferred_calls().len(), 1);
        *graph.deferred_calls().keys().next().unwrap()
    }

    fn deferred_dependency_edge_count(graph: &Graph, call_id: DeferredCallId) -> usize {
        graph
            .name_dependents()
            .values()
            .filter(|dependents| dependents.contains(&crate::model::graph::NameDependent::DeferredCall(call_id)))
            .count()
    }

    fn apply_expansion_in_stream(
        uri: &str,
        source: &str,
        call_id: DeferredCallId,
        assignment_name_id: NameId,
        expansion: Box<[Operation]>,
    ) -> Graph {
        let built = crate::operation::ruby_builder::RubyOperationBuilder::new(uri.to_string(), source).build();
        let crate::operation::ruby_builder::OperationBuilderResult {
            uri_id,
            document,
            items,
            strings,
            names,
        } = built;
        let mut expansion = Some(expansion);
        let mut substituted = Vec::with_capacity(items.len());

        for item in items {
            match item {
                crate::operation::CompiledItem::Operation(Operation::DefineConstant(op))
                    if op.name_id == assignment_name_id => {}
                crate::operation::CompiledItem::DeferredCall(call) if call.id() == call_id => {
                    substituted.extend(
                        expansion
                            .take()
                            .expect("deferred call must occur once")
                            .into_vec()
                            .into_iter()
                            .map(crate::operation::CompiledItem::Operation),
                    );
                }
                item => substituted.push(item),
            }
        }

        assert!(expansion.is_none(), "deferred call was not found in compiled stream");
        let local =
            crate::operation::applier::apply_operations(crate::operation::ruby_builder::OperationBuilderResult {
                uri_id,
                document,
                items: substituted,
                strings,
                names,
            });
        let mut graph = Graph::new();
        graph.consume_document_changes(local);
        resolve(&mut graph);
        graph
    }

    fn apply_expansion_out_of_band(
        uri: &str,
        source: &str,
        assignment_name_id: NameId,
        expansion: Box<[Operation]>,
    ) -> Graph {
        let built = crate::operation::ruby_builder::RubyOperationBuilder::new(uri.to_string(), source).build();
        let crate::operation::ruby_builder::OperationBuilderResult {
            uri_id,
            document,
            items,
            strings,
            names,
        } = built;
        let items = items
            .into_iter()
            .filter(|item| match item {
                crate::operation::CompiledItem::DeferredCall(_) => false,
                crate::operation::CompiledItem::Operation(Operation::DefineConstant(op)) => {
                    op.name_id != assignment_name_id
                }
                crate::operation::CompiledItem::Operation(_) => true,
            })
            .collect();
        let local =
            crate::operation::applier::apply_operations(crate::operation::ruby_builder::OperationBuilderResult {
                uri_id,
                document,
                items,
                strings,
                names,
            });
        let local = crate::operation::applier::apply_additional_operations(local, expansion);
        let mut graph = Graph::new();
        graph.consume_document_changes(local);
        resolve(&mut graph);
        graph
    }

    #[test]
    fn matches_struct_new_by_resolved_owner() {
        let mut graph = Graph::new();
        index(
            &mut graph,
            "file:///test.rb",
            "class Struct; end\nFoo = Struct.new(:name)",
        );
        let call_id = only_call_id(&graph);

        resolve(&mut graph);

        assert_eq!(
            graph.deferred_call_resolution(call_id),
            Some(DeferredCallResolution::Compiler(CompilerKind::StructNew))
        );
    }

    #[test]
    fn matches_alias_to_struct() {
        let mut graph = Graph::new();
        index(
            &mut graph,
            "file:///test.rb",
            "class Struct; end\nAlias = Struct\nFoo = Alias.new(:name)",
        );
        let call_id = only_call_id(&graph);

        resolve(&mut graph);

        assert_eq!(
            graph.deferred_call_resolution(call_id),
            Some(DeferredCallResolution::Compiler(CompilerKind::StructNew))
        );
    }

    #[test]
    fn shadowed_struct_falls_back() {
        let mut graph = Graph::new();
        index(
            &mut graph,
            "file:///test.rb",
            r"
            class SomethingElse; end
            module MyNamespace
              Struct = SomethingElse
              Foo = Struct.new(:name)
            end
            ",
        );
        let call_id = only_call_id(&graph);

        resolve(&mut graph);

        assert_eq!(
            graph.deferred_call_resolution(call_id),
            Some(DeferredCallResolution::Fallback)
        );
    }

    #[test]
    fn root_qualified_struct_matches_top_level_owner() {
        let mut graph = Graph::new();
        index(
            &mut graph,
            "file:///test.rb",
            "class Struct; end\nFoo = ::Struct.new(:name)",
        );
        let call_id = only_call_id(&graph);

        resolve(&mut graph);

        assert_eq!(
            graph.deferred_call_resolution(call_id),
            Some(DeferredCallResolution::Compiler(CompilerKind::StructNew))
        );
    }

    #[test]
    fn nested_class_named_struct_does_not_match_top_level_owner() {
        let mut graph = Graph::new();
        index(
            &mut graph,
            "file:///test.rb",
            r"
            class Struct; end
            module X
              class Struct; end
              Foo = Struct.new(:name)
            end
            ",
        );
        let call_id = only_call_id(&graph);

        resolve(&mut graph);

        assert_eq!(
            graph.deferred_call_resolution(call_id),
            Some(DeferredCallResolution::Fallback)
        );
    }

    #[test]
    fn reopened_top_level_struct_keeps_one_matching_owner() {
        let mut graph = Graph::new();
        index(
            &mut graph,
            "file:///test.rb",
            "class Struct; end\nclass Struct; end\nFoo = Struct.new(:name)",
        );
        let call_id = only_call_id(&graph);

        resolve(&mut graph);

        assert_eq!(
            graph.deferred_call_resolution(call_id),
            Some(DeferredCallResolution::Compiler(CompilerKind::StructNew))
        );
    }

    #[test]
    fn unsupported_arguments_fall_back() {
        let mut graph = Graph::new();
        index(
            &mut graph,
            "file:///test.rb",
            "class Struct; end\nFoo = Struct.new(dynamic_name)",
        );
        let call_id = only_call_id(&graph);

        resolve(&mut graph);

        assert_eq!(
            graph.deferred_call_resolution(call_id),
            Some(DeferredCallResolution::Fallback)
        );
    }

    #[test]
    fn unresolved_receiver_stays_pending_for_retry() {
        let mut graph = Graph::new();
        index(&mut graph, "file:///use.rb", "Foo = Missing.new(:name)");
        let call_id = only_call_id(&graph);

        resolve(&mut graph);

        assert_eq!(
            graph.deferred_call_resolution(call_id),
            Some(DeferredCallResolution::Pending)
        );
        assert!(
            graph
                .take_pending_work()
                .iter()
                .any(|unit| matches!(unit, crate::model::graph::Unit::DeferredCall(id) if *id == call_id))
        );
    }

    #[test]
    fn expansion_uses_only_existing_semantic_operations() {
        let mut graph = Graph::new();
        index(
            &mut graph,
            "file:///test.rb",
            "class Struct; end\nFoo = Struct.new(:name, :email)",
        );
        let call_id = only_call_id(&graph);
        let receiver_name_id = graph.deferred_calls()[&call_id].receiver_name_id();

        resolve(&mut graph);

        let expansion = graph.deferred_call_expansion(call_id).unwrap();
        assert!(matches!(expansion[0], Operation::EnterClass(_)));
        assert!(matches!(expansion[1], Operation::DefineAttribute(_)));
        assert!(matches!(expansion[2], Operation::DefineAttribute(_)));
        assert!(matches!(expansion[3], Operation::ExitScope));
        let Operation::EnterClass(enter) = &expansion[0] else {
            panic!("expected EnterClass");
        };
        assert_eq!(enter.superclass_name, Some(receiver_name_id));
    }

    #[test]
    fn alias_expansion_keeps_source_receiver_name() {
        let mut graph = Graph::new();
        index(
            &mut graph,
            "file:///test.rb",
            "class Struct; end\nAlias = Struct\nFoo = Alias.new(:name)",
        );
        let call_id = only_call_id(&graph);
        let receiver_name_id = graph.deferred_calls()[&call_id].receiver_name_id();

        resolve(&mut graph);

        let expansion = graph.deferred_call_expansion(call_id).unwrap();
        let Operation::EnterClass(enter) = &expansion[0] else {
            panic!("expected EnterClass expansion");
        };
        // Semantic match is Struct, but IR keeps source Alias for applier ReferenceConstant contract.
        assert_eq!(enter.superclass_name, Some(receiver_name_id));
        assert_eq!(
            graph.names().get(&receiver_name_id).and_then(|name| match name {
                crate::model::name::NameRef::Resolved(resolved) => Some(*resolved.declaration_id()),
                crate::model::name::NameRef::Unresolved(_) => None,
            }),
            Some(crate::model::ids::declaration_id_from_lookup_name("Alias"))
        );
    }

    #[test]
    fn matches_multi_hop_alias_to_struct() {
        let mut graph = Graph::new();
        index(
            &mut graph,
            "file:///test.rb",
            "class Struct; end\nAliasB = Struct\nAliasA = AliasB\nFoo = AliasA.new(:name)",
        );
        let call_id = only_call_id(&graph);

        resolve(&mut graph);

        assert_eq!(
            graph.deferred_call_resolution(call_id),
            Some(DeferredCallResolution::Compiler(CompilerKind::StructNew))
        );
    }

    #[test]
    fn dependency_edges_track_only_the_resolved_alias_path() {
        for (source, expected_edges) in [
            ("class Struct; end\nFoo = Struct.new(:name)", 1),
            ("class Struct; end\nAlias = Struct\nFoo = Alias.new(:name)", 2),
            (
                "class Struct; end\nAliasB = Struct\nAliasA = AliasB\nFoo = AliasA.new(:name)",
                3,
            ),
        ] {
            let mut graph = Graph::new();
            index(&mut graph, "file:///test.rb", source);
            let call_id = only_call_id(&graph);
            resolve(&mut graph);

            assert_eq!(deferred_dependency_edge_count(&graph, call_id), expected_edges);
        }
    }

    #[test]
    fn ambiguous_alias_matches_after_other_target_is_removed() {
        let mut graph = Graph::new();
        index(&mut graph, "file:///types.rb", "class Struct; end\nclass Other; end");
        index(&mut graph, "file:///a.rb", "Alias = Struct");
        index(&mut graph, "file:///b.rb", "Alias = Other");
        index(&mut graph, "file:///use.rb", "Foo = Alias.new(:name)");
        let call_id = only_call_id(&graph);

        resolve(&mut graph);
        assert_eq!(
            graph.deferred_call_resolution(call_id),
            Some(DeferredCallResolution::Fallback)
        );

        index(&mut graph, "file:///b.rb", "");
        assert_eq!(
            graph.deferred_call_resolution(call_id),
            Some(DeferredCallResolution::Pending)
        );
        resolve(&mut graph);
        assert_eq!(
            graph.deferred_call_resolution(call_id),
            Some(DeferredCallResolution::Compiler(CompilerKind::StructNew))
        );
    }

    #[test]
    fn duplicate_aliases_to_same_target_are_not_ambiguous() {
        let mut graph = Graph::new();
        index(&mut graph, "file:///types.rb", "class Struct; end");
        index(&mut graph, "file:///a.rb", "Alias = Struct");
        index(&mut graph, "file:///b.rb", "Alias = Struct");
        index(&mut graph, "file:///use.rb", "Foo = Alias.new(:name)");
        let call_id = only_call_id(&graph);

        resolve(&mut graph);

        assert_eq!(
            graph.deferred_call_resolution(call_id),
            Some(DeferredCallResolution::Compiler(CompilerKind::StructNew))
        );
    }

    #[test]
    fn constant_struct_identity_falls_back() {
        let mut graph = Graph::new();
        index(&mut graph, "file:///test.rb", "Struct = 123\nFoo = Struct.new(:name)");
        let call_id = only_call_id(&graph);

        resolve(&mut graph);

        assert_eq!(
            graph.deferred_call_resolution(call_id),
            Some(DeferredCallResolution::Fallback)
        );
    }

    #[test]
    fn module_struct_identity_falls_back() {
        let mut graph = Graph::new();
        index(
            &mut graph,
            "file:///test.rb",
            "module Struct; end\nFoo = Struct.new(:name)",
        );
        let call_id = only_call_id(&graph);

        resolve(&mut graph);

        assert_eq!(
            graph.deferred_call_resolution(call_id),
            Some(DeferredCallResolution::Fallback)
        );
    }

    #[test]
    fn owner_kind_transition_re_evaluates_without_reindexing_use_site() {
        let mut graph = Graph::new();
        index(&mut graph, "file:///owner.rb", "class Struct; end");
        index(&mut graph, "file:///use.rb", "Foo = Struct.new(:name)");
        let call_id = only_call_id(&graph);

        resolve(&mut graph);
        assert_eq!(
            graph.deferred_call_resolution(call_id),
            Some(DeferredCallResolution::Compiler(CompilerKind::StructNew))
        );

        index(&mut graph, "file:///owner.rb", "module Struct; end");
        resolve(&mut graph);
        assert_eq!(
            graph.deferred_call_resolution(call_id),
            Some(DeferredCallResolution::Fallback)
        );

        index(&mut graph, "file:///owner.rb", "class Struct; end");
        resolve(&mut graph);
        assert_eq!(
            graph.deferred_call_resolution(call_id),
            Some(DeferredCallResolution::Compiler(CompilerKind::StructNew))
        );
    }

    #[test]
    fn alias_cycle_terminates_with_deterministic_fallback() {
        let mut graph = Graph::new();
        index(&mut graph, "file:///test.rb", "A = B\nB = A\nFoo = A.new(:name)");
        let call_id = only_call_id(&graph);

        resolve(&mut graph);
        assert_eq!(
            graph.deferred_call_resolution(call_id),
            Some(DeferredCallResolution::Fallback)
        );

        resolve(&mut graph);
        assert_eq!(
            graph.deferred_call_resolution(call_id),
            Some(DeferredCallResolution::Fallback)
        );
        assert!(integrity::check_integrity(&graph).is_empty());
    }

    #[test]
    fn later_constant_value_does_not_replace_class_owner_identity() {
        let mut graph = Graph::new();
        index(
            &mut graph,
            "file:///test.rb",
            "class Struct; end\nStruct = 123\nFoo = Struct.new(:name)",
        );
        let call_id = only_call_id(&graph);

        resolve(&mut graph);

        assert_eq!(
            graph.deferred_call_resolution(call_id),
            Some(DeferredCallResolution::Compiler(CompilerKind::StructNew))
        );
    }

    #[test]
    fn multi_hop_alias_path_change_re_evaluates() {
        let mut graph = Graph::new();
        index(&mut graph, "file:///types.rb", "class Struct; end\nclass Other; end");
        index(&mut graph, "file:///mid.rb", "AliasB = Struct");
        index(&mut graph, "file:///binding.rb", "AliasA = AliasB");
        index(&mut graph, "file:///use.rb", "Foo = AliasA.new(:name)");
        let call_id = only_call_id(&graph);
        resolve(&mut graph);
        assert_eq!(
            graph.deferred_call_resolution(call_id),
            Some(DeferredCallResolution::Compiler(CompilerKind::StructNew))
        );

        index(&mut graph, "file:///mid.rb", "AliasB = Other");
        resolve(&mut graph);
        assert_eq!(
            graph.deferred_call_resolution(call_id),
            Some(DeferredCallResolution::Fallback)
        );
    }

    #[test]
    fn unrelated_struct_reference_does_not_wake_deferred_call() {
        let mut graph = Graph::new();
        index(&mut graph, "file:///types.rb", "class Struct; end\nclass Other; end");
        index(&mut graph, "file:///binding.rb", "Alias = Struct");
        index(&mut graph, "file:///use.rb", "Foo = Alias.new(:name)");
        index(&mut graph, "file:///unrelated.rb", "Unrelated = Struct");
        let call_id = only_call_id(&graph);
        resolve(&mut graph);
        assert_eq!(
            graph.deferred_call_resolution(call_id),
            Some(DeferredCallResolution::Compiler(CompilerKind::StructNew))
        );

        // Changing an unrelated alias to Struct must not invalidate Foo's candidate.
        index(&mut graph, "file:///unrelated.rb", "Unrelated = Other");
        // Do not resolve — if the candidate was incorrectly woken it would be Pending.
        assert_eq!(
            graph.deferred_call_resolution(call_id),
            Some(DeferredCallResolution::Compiler(CompilerKind::StructNew))
        );
        assert!(
            !graph
                .take_pending_work()
                .iter()
                .any(|unit| matches!(unit, crate::model::graph::Unit::DeferredCall(id) if *id == call_id))
        );
    }

    #[test]
    fn unrelated_alias_does_not_wake_deferred_call() {
        let mut graph = Graph::new();
        index(&mut graph, "file:///types.rb", "class Struct; end\nclass Other; end");
        index(&mut graph, "file:///binding.rb", "AliasA = Struct");
        index(&mut graph, "file:///other_alias.rb", "OtherAlias = Struct");
        index(&mut graph, "file:///use.rb", "Foo = AliasA.new(:name)");
        let call_id = only_call_id(&graph);
        resolve(&mut graph);
        assert_eq!(
            graph.deferred_call_resolution(call_id),
            Some(DeferredCallResolution::Compiler(CompilerKind::StructNew))
        );

        index(&mut graph, "file:///other_alias.rb", "OtherAlias = Other");
        assert_eq!(
            graph.deferred_call_resolution(call_id),
            Some(DeferredCallResolution::Compiler(CompilerKind::StructNew))
        );
        assert!(
            !graph
                .take_pending_work()
                .iter()
                .any(|unit| matches!(unit, crate::model::graph::Unit::DeferredCall(id) if *id == call_id))
        );
    }

    #[test]
    fn alias_expansion_is_replay_valid_through_applier() {
        let source = "class Struct; end\nAlias = Struct\nFoo = Alias.new(:name)";
        let mut graph = Graph::new();
        index(&mut graph, "file:///test.rb", source);
        let call_id = only_call_id(&graph);
        let receiver_name_id = graph.deferred_calls()[&call_id].receiver_name_id();
        let assignment_name_id = graph.deferred_calls()[&call_id].assignment().name_id();
        resolve(&mut graph);
        assert_eq!(
            graph.deferred_call_resolution(call_id),
            Some(DeferredCallResolution::Compiler(CompilerKind::StructNew))
        );

        let expansion = graph.take_deferred_call_expansion(call_id).unwrap();
        let Operation::EnterClass(enter) = &expansion[0] else {
            panic!("expected EnterClass");
        };
        assert_eq!(enter.superclass_name, Some(receiver_name_id));

        // Replay without the shadow DefineConstant(Foo): authoritative contribution would
        // replace that constant, not coexist with EnterClass at the same DefinitionId.
        let built =
            crate::operation::ruby_builder::RubyOperationBuilder::new("file:///test.rb".to_string(), source).build();
        let crate::operation::ruby_builder::OperationBuilderResult {
            uri_id,
            document,
            items,
            strings,
            names,
        } = built;
        let items = items
            .into_iter()
            .filter(|item| match item {
                crate::operation::CompiledItem::DeferredCall(_) => false,
                crate::operation::CompiledItem::Operation(crate::operation::Operation::DefineConstant(op)) => {
                    op.name_id != assignment_name_id
                }
                crate::operation::CompiledItem::Operation(_) => true,
            })
            .collect();
        let local =
            crate::operation::applier::apply_operations(crate::operation::ruby_builder::OperationBuilderResult {
                uri_id,
                document,
                items,
                strings,
                names,
            });
        let local = crate::operation::applier::apply_additional_operations(local, expansion);
        let class_def = local
            .definitions()
            .values()
            .find_map(|definition| match definition {
                crate::model::definitions::Definition::Class(class_def)
                    if *class_def.name_id() == assignment_name_id =>
                {
                    Some(class_def.as_ref())
                }
                _ => None,
            })
            .expect("expected ClassDefinition from expansion replay");
        let superclass_ref = class_def.superclass_ref().expect("superclass ref must bind");
        let superclass_name = *local.constant_references().get(superclass_ref).unwrap().name_id();
        assert_eq!(superclass_name, receiver_name_id);

        // Source Alias still resolves to Struct through ordinary alias machinery.
        assert_eq!(
            graph.resolve_alias(&crate::model::ids::declaration_id_from_lookup_name("Alias")),
            Some(crate::model::ids::declaration_id_from_lookup_name("Struct"))
        );
    }

    #[test]
    fn in_stream_expansion_affects_downstream_resolution() {
        let uri = "file:///test.rb";
        let source = r"
            class Struct; end
            Foo = Struct.new(:name)
            class Bar < Foo; end
        ";
        let mut observation_graph = Graph::new();
        index(&mut observation_graph, uri, source);
        let call_id = only_call_id(&observation_graph);
        let assignment_name_id = observation_graph.deferred_calls()[&call_id].assignment().name_id();
        resolve(&mut observation_graph);
        let expansion = observation_graph.take_deferred_call_expansion(call_id).unwrap();

        let graph = apply_expansion_in_stream(uri, source, call_id, assignment_name_id, expansion);
        let struct_id = crate::model::ids::declaration_id_from_lookup_name("Struct");
        let foo_id = crate::model::ids::declaration_id_from_lookup_name("Foo");
        let bar_id = crate::model::ids::declaration_id_from_lookup_name("Bar");

        let foo = graph.declarations().get(&foo_id).unwrap();
        assert!(matches!(
            foo,
            crate::model::declaration::Declaration::Namespace(crate::model::declaration::Namespace::Class(_))
        ));
        let foo_namespace = foo.as_namespace().unwrap();
        assert!(
            foo_namespace.ancestors().iter().any(
                |ancestor| matches!(ancestor, crate::model::declaration::Ancestor::Complete(id) if *id == struct_id)
            )
        );
        assert!(foo_namespace.members().contains_key(&StringId::from("name")));

        let bar = graph.declarations().get(&bar_id).unwrap().as_namespace().unwrap();
        assert!(
            bar.ancestors()
                .iter()
                .any(|ancestor| matches!(ancestor, crate::model::declaration::Ancestor::Complete(id) if *id == foo_id))
        );
        assert!(integrity::check_integrity(&graph).is_empty());
    }

    #[test]
    fn in_stream_expansion_preserves_nested_execution_context() {
        let uri = "file:///test.rb";
        let source = r"
            class Struct; end
            module Outer
              Foo = Struct.new(:name)
              class Bar < Foo; end
            end
        ";
        let mut observation_graph = Graph::new();
        index(&mut observation_graph, uri, source);
        let call_id = only_call_id(&observation_graph);
        let assignment_name_id = observation_graph.deferred_calls()[&call_id].assignment().name_id();
        resolve(&mut observation_graph);
        let expansion = observation_graph.take_deferred_call_expansion(call_id).unwrap();

        let graph = apply_expansion_in_stream(uri, source, call_id, assignment_name_id, expansion);
        let struct_id = crate::model::ids::declaration_id_from_lookup_name("Struct");
        let foo_id = crate::model::ids::declaration_id_from_lookup_name("Outer::Foo");
        let bar_id = crate::model::ids::declaration_id_from_lookup_name("Outer::Bar");

        let foo = graph.declarations().get(&foo_id).unwrap().as_namespace().unwrap();
        assert!(
            foo.ancestors().iter().any(
                |ancestor| matches!(ancestor, crate::model::declaration::Ancestor::Complete(id) if *id == struct_id)
            )
        );
        assert!(foo.members().contains_key(&StringId::from("name")));

        let bar = graph.declarations().get(&bar_id).unwrap().as_namespace().unwrap();
        assert!(
            bar.ancestors()
                .iter()
                .any(|ancestor| matches!(ancestor, crate::model::declaration::Ancestor::Complete(id) if *id == foo_id))
        );
        assert!(integrity::check_integrity(&graph).is_empty());
    }

    #[test]
    fn out_of_band_replay_supports_the_self_contained_nested_struct_expansion() {
        let uri = "file:///test.rb";
        let source = r"
            class Struct; end
            module Outer
              Foo = Struct.new(:name)
              class Bar < Foo; end
            end
        ";
        let mut observation_graph = Graph::new();
        index(&mut observation_graph, uri, source);
        let call_id = only_call_id(&observation_graph);
        let assignment_name_id = observation_graph.deferred_calls()[&call_id].assignment().name_id();
        resolve(&mut observation_graph);
        let expansion = observation_graph.take_deferred_call_expansion(call_id).unwrap();

        let graph = apply_expansion_out_of_band(uri, source, assignment_name_id, expansion);
        let foo_id = crate::model::ids::declaration_id_from_lookup_name("Outer::Foo");
        let bar_id = crate::model::ids::declaration_id_from_lookup_name("Outer::Bar");

        let foo = graph.declarations().get(&foo_id).unwrap().as_namespace().unwrap();
        assert!(foo.members().contains_key(&StringId::from("name")));
        let bar = graph.declarations().get(&bar_id).unwrap().as_namespace().unwrap();
        assert!(
            bar.ancestors()
                .iter()
                .any(|ancestor| matches!(ancestor, crate::model::declaration::Ancestor::Complete(id) if *id == foo_id))
        );
        assert!(integrity::check_integrity(&graph).is_empty());
    }

    #[test]
    fn document_delete_removes_candidate_and_reverse_dependency() {
        let mut graph = Graph::new();
        index(
            &mut graph,
            "file:///test.rb",
            "class Struct; end\nFoo = Struct.new(:name)",
        );
        let call_id = only_call_id(&graph);
        let receiver_name_id = graph.deferred_calls()[&call_id].receiver_name_id();

        graph.delete_document("file:///test.rb");

        assert!(!graph.deferred_calls().contains_key(&call_id));
        assert!(
            graph.name_dependents().get(&receiver_name_id).is_none_or(
                |dependents| !dependents.contains(&crate::model::graph::NameDependent::DeferredCall(call_id))
            )
        );
    }

    #[test]
    fn document_reindex_removes_stale_candidate() {
        let mut graph = Graph::new();
        index(
            &mut graph,
            "file:///test.rb",
            "class Struct; end\nFoo = Struct.new(:name)",
        );
        let call_id = only_call_id(&graph);

        index(&mut graph, "file:///test.rb", "class Struct; end\nFoo = 1");

        assert!(!graph.deferred_calls().contains_key(&call_id));
        assert_eq!(graph.deferred_call_resolution(call_id), None);
        assert!(graph.deferred_call_expansion(call_id).is_none());
    }

    #[test]
    fn deferred_call_cleanup_does_not_untrack_shared_assignment_name() {
        for reindex in [false, true] {
            let mut graph = Graph::new();
            index(&mut graph, "file:///candidate.rb", "Foo = Bar.new(:candidate_only)");
            index(&mut graph, "file:///survivor.rb", "Foo = 123");
            resolve(&mut graph);

            let call_id = only_call_id(&graph);
            let assignment_name_id = graph.deferred_calls()[&call_id].assignment().name_id();
            let argument_string_id = match graph.deferred_calls()[&call_id].arguments()[0] {
                DeferredArgument::LiteralName(str_id) => str_id,
                DeferredArgument::Unsupported => panic!("expected literal argument"),
            };

            if reindex {
                index(&mut graph, "file:///candidate.rb", "Candidate = 456");
            } else {
                graph.delete_document("file:///candidate.rb");
            }

            assert!(graph.names().contains_key(&assignment_name_id));
            assert!(!graph.strings().contains_key(&argument_string_id));
            assert!(graph.deferred_calls().is_empty());
            assert!(integrity::check_integrity(&graph).is_empty());
        }
    }

    #[test]
    fn alias_target_deletion_re_evaluates_deferred_call() {
        let mut graph = Graph::new();
        index(&mut graph, "file:///types.rb", "class Struct; end\nclass Other; end");
        index(&mut graph, "file:///binding.rb", "Alias = Struct");
        index(&mut graph, "file:///use.rb", "Foo = Alias.new(:name)");
        let call_id = only_call_id(&graph);
        resolve(&mut graph);
        assert_eq!(
            graph.deferred_call_resolution(call_id),
            Some(DeferredCallResolution::Compiler(CompilerKind::StructNew))
        );

        // Delete Struct without touching the Alias binding or use site. Matching depended on
        // the normalized owner, so the candidate must re-evaluate.
        index(&mut graph, "file:///types.rb", "class Other; end");
        resolve(&mut graph);
        assert_ne!(
            graph.deferred_call_resolution(call_id),
            Some(DeferredCallResolution::Compiler(CompilerKind::StructNew))
        );
    }

    #[test]
    fn pending_receiver_matches_after_owner_appears() {
        let mut graph = Graph::new();
        index(&mut graph, "file:///use.rb", "Foo = Struct.new(:name)");
        let call_id = only_call_id(&graph);
        resolve(&mut graph);
        assert_eq!(
            graph.deferred_call_resolution(call_id),
            Some(DeferredCallResolution::Pending)
        );

        index(&mut graph, "file:///types.rb", "class Struct; end");
        resolve(&mut graph);
        assert_eq!(
            graph.deferred_call_resolution(call_id),
            Some(DeferredCallResolution::Compiler(CompilerKind::StructNew))
        );
    }

    #[test]
    fn receiver_change_re_evaluates_without_reindexing_use_site() {
        let mut graph = Graph::new();
        index(&mut graph, "file:///types.rb", "class Struct; end\nclass Other; end");
        index(&mut graph, "file:///binding.rb", "Alias = Struct");
        index(&mut graph, "file:///use.rb", "Foo = Alias.new(:name)");
        let call_id = only_call_id(&graph);
        resolve(&mut graph);
        assert_eq!(
            graph.deferred_call_resolution(call_id),
            Some(DeferredCallResolution::Compiler(CompilerKind::StructNew))
        );

        index(&mut graph, "file:///binding.rb", "Alias = Other");
        resolve(&mut graph);
        assert_eq!(
            graph.deferred_call_resolution(call_id),
            Some(DeferredCallResolution::Fallback)
        );

        index(&mut graph, "file:///binding.rb", "Alias = Struct");
        resolve(&mut graph);
        assert_eq!(
            graph.deferred_call_resolution(call_id),
            Some(DeferredCallResolution::Compiler(CompilerKind::StructNew))
        );
    }
}
