//! Converts an `OperationBuilderResult` into a `LocalGraph` by walking operations and creating definitions.
//!
//! This is the second phase of the two-phase operation pipeline:
//! 1. `RubyOperationBuilder` parses source → produces ordered operations
//! 2. `apply_operations` walks operations → creates definitions in a `LocalGraph`
//!
//! The applier maintains its own scope stack to derive `lexical_nesting_id` for definitions.
//! Operations carry only their own data; scope context comes from Enter/Exit scope operations.

use std::collections::HashMap;

use crate::indexing::local_graph::LocalGraph;
use crate::model::definitions::{
    AttrAccessorDefinition, AttrReaderDefinition, AttrWriterDefinition, ClassDefinition, ClassVariableDefinition,
    ConstantAliasDefinition, ConstantDefinition, ConstantVisibilityDefinition, Definition, ExtendDefinition,
    GlobalVariableAliasDefinition, GlobalVariableDefinition, IncludeDefinition, InstanceVariableDefinition,
    MethodAliasDefinition, MethodDefinition, MethodVisibilityDefinition, Mixin, ModuleDefinition, PrependDefinition,
    Receiver, SingletonClassDefinition,
};
use crate::model::ids::{ConstantReferenceId, DefinitionId, NameId};
use crate::model::references::{ConstantReference, InstanceVariableRef, MethodRef};
use crate::model::visibility::Visibility;
use crate::operation::ruby_builder::OperationBuilderResult;
use crate::operation::{
    AliasConstant, AliasGlobalVariable, AliasMethod, AttrKind, DefineAttribute, DefineClassVariable, DefineConstant,
    DefineGlobalVariable, DefineInstanceVariable, EnterClass, EnterMethod, EnterModule, EnterSingletonClass, MixinKind,
    Operation, ReferenceConstant, ReferenceInstanceVariable, ReferenceMethod, SetConstantVisibility,
    SetMethodVisibility, Target,
};

enum ApplierScope {
    Namespace {
        definition_id: DefinitionId,
        is_lexical_scope: bool,
    },
    Method {
        definition_id: DefinitionId,
    },
}

struct OperationApplier {
    local_graph: LocalGraph,
    scope_stack: Vec<ApplierScope>,
    scope_visibility: HashMap<Option<DefinitionId>, Visibility>,
    // Maps the most recently emitted ReferenceConstant per name. The builder emits
    // ReferenceConstant immediately before the operation that consumes it (Mixin,
    // EnterClass superclass, SetConstantVisibility), so the last entry always wins.
    constant_ref_ids: HashMap<NameId, ConstantReferenceId>,
}

impl OperationApplier {
    fn current_owner_id(&self) -> Option<DefinitionId> {
        self.scope_stack.iter().rev().find_map(|scope| match scope {
            ApplierScope::Namespace { definition_id, .. } => Some(*definition_id),
            ApplierScope::Method { .. } => None,
        })
    }

    fn current_lexical_scope_id(&self) -> Option<DefinitionId> {
        self.scope_stack.iter().rev().find_map(|scope| match scope {
            ApplierScope::Namespace {
                definition_id,
                is_lexical_scope: true,
            } => Some(*definition_id),
            _ => None,
        })
    }

    fn current_scope_id(&self) -> Option<DefinitionId> {
        self.scope_stack.last().map(|scope| match scope {
            ApplierScope::Namespace { definition_id, .. } | ApplierScope::Method { definition_id } => *definition_id,
        })
    }

    fn resolve_receiver(&self, receiver: Option<&Target>) -> Option<Receiver> {
        let current_owner_id = self.current_owner_id();
        match receiver {
            Some(Target::ExplicitSelf) => current_owner_id.map(Receiver::SelfReceiver),
            Some(Target::Constant(name_id)) => Some(Receiver::ConstantReceiver(*name_id)),
            Some(Target::Other) | None => None,
        }
    }

    fn resolve_visibility(&self, has_receiver: bool) -> Visibility {
        if has_receiver {
            return Visibility::Public;
        }
        let scope = self.current_owner_id();
        let default = self
            .scope_visibility
            .get(&scope)
            .copied()
            .unwrap_or(if scope.is_none() {
                Visibility::Private
            } else {
                Visibility::Public
            });
        match default {
            Visibility::ModuleFunction => Visibility::Private,
            v => v,
        }
    }

    fn add_member(&mut self, owner_id: Option<DefinitionId>, member_id: DefinitionId) {
        let Some(owner_id) = owner_id else {
            return;
        };

        let Some(owner) = self.local_graph.get_definition_mut(owner_id) else {
            return;
        };

        match owner {
            Definition::Class(class) => class.add_member(member_id),
            Definition::Module(module) => module.add_member(member_id),
            Definition::SingletonClass(singleton) => singleton.add_member(member_id),
            _ => {}
        }
    }
}

impl OperationApplier {
    fn apply_operation(&mut self, op: Operation) {
        match op {
            Operation::EnterClass(op) => self.apply_enter_class(op),
            Operation::EnterModule(op) => self.apply_enter_module(op),
            Operation::EnterSingletonClass(op) => self.apply_enter_singleton_class(op),
            Operation::EnterMethod(op) => self.apply_enter_method(op),
            Operation::ExitScope => {
                debug_assert!(!self.scope_stack.is_empty(), "ExitScope with empty scope stack");
                self.scope_stack.pop();
            }
            Operation::AliasMethod(op) => self.apply_alias_method(op),
            Operation::SetMethodVisibility(op) => self.apply_set_method_visibility(op),
            Operation::SetDefaultVisibility(op) => {
                let scope = self.current_owner_id();
                self.scope_visibility.insert(scope, op.visibility);
            }
            Operation::DefineConstant(op) => self.apply_define_constant(op),
            Operation::AliasConstant(op) => self.apply_alias_constant(op),
            Operation::SetConstantVisibility(op) => self.apply_set_constant_visibility(op),
            Operation::Mixin(ref op) => self.apply_mixin(op),
            Operation::DefineAttribute(op) => self.apply_define_attribute(op),
            Operation::DefineGlobalVariable(op) => self.apply_define_global_variable(op),
            Operation::DefineInstanceVariable(op) => self.apply_define_instance_variable(op),
            Operation::DefineClassVariable(op) => self.apply_define_class_variable(op),
            Operation::AliasGlobalVariable(op) => self.apply_alias_global_variable(op),
            Operation::ReferenceConstant(op) => self.apply_reference_constant(op),
            Operation::ReferenceMethod(op) => self.apply_reference_method(op),
            Operation::ReferenceInstanceVariable(op) => self.apply_reference_instance_variable(op),
        }
    }

    fn apply_enter_class(&mut self, op: EnterClass) {
        let lexical_nesting_id = self.current_lexical_scope_id();
        let superclass_ref = op.superclass_name.and_then(|n| self.constant_ref_ids.get(&n).copied());
        let def = ClassDefinition::new(
            op.name_id,
            op.uri_id,
            op.offset,
            op.name_offset,
            op.comments,
            op.flags,
            lexical_nesting_id,
            superclass_ref,
        );
        let def_id = self.local_graph.add_definition(Definition::Class(Box::new(def)));
        self.add_member(lexical_nesting_id, def_id);
        self.scope_stack.push(ApplierScope::Namespace {
            definition_id: def_id,
            is_lexical_scope: op.is_lexical_scope,
        });
    }

    fn apply_enter_module(&mut self, op: EnterModule) {
        let lexical_nesting_id = self.current_lexical_scope_id();
        let def = ModuleDefinition::new(
            op.name_id,
            op.uri_id,
            op.offset,
            op.name_offset,
            op.comments,
            op.flags,
            lexical_nesting_id,
        );
        let def_id = self.local_graph.add_definition(Definition::Module(Box::new(def)));
        self.add_member(lexical_nesting_id, def_id);
        self.scope_stack.push(ApplierScope::Namespace {
            definition_id: def_id,
            is_lexical_scope: op.is_lexical_scope,
        });
    }

    fn apply_enter_singleton_class(&mut self, op: EnterSingletonClass) {
        let lexical_nesting_id = self.current_lexical_scope_id();
        let def = SingletonClassDefinition::new(
            op.name_id,
            op.uri_id,
            op.offset,
            op.name_offset,
            op.comments,
            op.flags,
            lexical_nesting_id,
        );
        let def_id = self
            .local_graph
            .add_definition(Definition::SingletonClass(Box::new(def)));
        self.add_member(lexical_nesting_id, def_id);
        self.scope_stack.push(ApplierScope::Namespace {
            definition_id: def_id,
            is_lexical_scope: true,
        });
    }

    fn apply_enter_method(&mut self, op: EnterMethod) {
        let lexical_nesting_id = self.current_owner_id();
        let has_receiver = op.receiver.is_some();
        let receiver = self.resolve_receiver(op.receiver.as_ref());
        let visibility = self.resolve_visibility(has_receiver);
        let def = MethodDefinition::new(
            op.str_id,
            op.uri_id,
            op.offset,
            op.name_offset,
            op.comments,
            op.flags,
            lexical_nesting_id,
            op.signatures,
            visibility,
            receiver,
        );
        let def_id = self.local_graph.add_definition(Definition::Method(Box::new(def)));
        self.add_member(lexical_nesting_id, def_id);
        self.scope_stack.push(ApplierScope::Method { definition_id: def_id });
    }

    fn apply_alias_method(&mut self, op: AliasMethod) {
        let lexical_nesting_id = self.current_owner_id();
        let receiver = self.resolve_receiver(op.receiver.as_ref());
        let def = MethodAliasDefinition::new(
            op.new_name_str_id,
            op.old_name_str_id,
            op.uri_id,
            op.offset,
            op.comments,
            op.flags,
            lexical_nesting_id,
            receiver,
        );
        let def_id = self.local_graph.add_definition(Definition::MethodAlias(Box::new(def)));
        self.add_member(lexical_nesting_id, def_id);
    }

    fn apply_set_method_visibility(&mut self, op: SetMethodVisibility) {
        let lexical_nesting_id = self.current_owner_id();
        let def = MethodVisibilityDefinition::new(
            op.str_id,
            op.visibility,
            op.uri_id,
            op.offset,
            Box::default(),
            op.flags,
            lexical_nesting_id,
        );
        let def_id = self
            .local_graph
            .add_definition(Definition::MethodVisibility(Box::new(def)));
        self.add_member(lexical_nesting_id, def_id);
    }

    fn apply_define_constant(&mut self, op: DefineConstant) {
        let lexical_nesting_id = self.current_lexical_scope_id();
        let def = ConstantDefinition::new(
            op.name_id,
            op.uri_id,
            op.offset,
            op.comments,
            op.flags,
            lexical_nesting_id,
        );
        let def_id = self.local_graph.add_definition(Definition::Constant(Box::new(def)));
        self.add_member(lexical_nesting_id, def_id);
    }

    fn apply_alias_constant(&mut self, op: AliasConstant) {
        let lexical_nesting_id = self.current_lexical_scope_id();
        let constant = ConstantDefinition::new(
            op.name_id,
            op.uri_id,
            op.offset,
            op.comments,
            op.flags,
            lexical_nesting_id,
        );
        let def = ConstantAliasDefinition::new(op.target_name_id, constant);
        let def_id = self
            .local_graph
            .add_definition(Definition::ConstantAlias(Box::new(def)));
        self.add_member(lexical_nesting_id, def_id);
    }

    fn apply_set_constant_visibility(&mut self, op: SetConstantVisibility) {
        let lexical_nesting_id = self.current_owner_id();
        let receiver = match op.receiver {
            Some(Target::Constant(name_id)) => Some(name_id),
            Some(Target::ExplicitSelf | Target::Other) | None => None,
        };
        let def = ConstantVisibilityDefinition::new(
            receiver,
            op.target,
            op.visibility,
            op.uri_id,
            op.offset,
            op.comments,
            op.flags,
            lexical_nesting_id,
        );
        let def_id = self
            .local_graph
            .add_definition(Definition::ConstantVisibility(Box::new(def)));
        self.add_member(lexical_nesting_id, def_id);
    }

    fn apply_mixin(&mut self, op: &crate::operation::Mixin) {
        let Some(owner_id) = self.current_owner_id() else {
            return;
        };

        let constant_reference_id = match op.target {
            Target::Constant(name_id) => self.constant_ref_ids.get(&name_id).copied(),
            Target::ExplicitSelf | Target::Other => None,
        };

        let Some(constant_reference_id) = constant_reference_id else {
            return;
        };

        let mixin = match op.kind {
            MixinKind::Include => Mixin::Include(IncludeDefinition::new(constant_reference_id)),
            MixinKind::Prepend => Mixin::Prepend(PrependDefinition::new(constant_reference_id)),
            MixinKind::Extend => Mixin::Extend(ExtendDefinition::new(constant_reference_id)),
        };

        if let Some(owner) = self.local_graph.get_definition_mut(owner_id) {
            match owner {
                Definition::Class(class) => class.add_mixin(mixin),
                Definition::Module(module) => module.add_mixin(mixin),
                Definition::SingletonClass(singleton) => singleton.add_mixin(mixin),
                _ => {}
            }
        }
    }

    fn apply_define_attribute(&mut self, op: DefineAttribute) {
        let lexical_nesting_id = self.current_scope_id();
        let visibility = self.resolve_visibility(false);
        let def_id = match op.kind {
            AttrKind::Accessor => {
                let def = AttrAccessorDefinition::new(
                    op.str_id,
                    op.uri_id,
                    op.offset,
                    op.comments,
                    op.flags,
                    lexical_nesting_id,
                    visibility,
                );
                self.local_graph.add_definition(Definition::AttrAccessor(Box::new(def)))
            }
            AttrKind::Reader => {
                let def = AttrReaderDefinition::new(
                    op.str_id,
                    op.uri_id,
                    op.offset,
                    op.comments,
                    op.flags,
                    lexical_nesting_id,
                    visibility,
                );
                self.local_graph.add_definition(Definition::AttrReader(Box::new(def)))
            }
            AttrKind::Writer => {
                let def = AttrWriterDefinition::new(
                    op.str_id,
                    op.uri_id,
                    op.offset,
                    op.comments,
                    op.flags,
                    lexical_nesting_id,
                    visibility,
                );
                self.local_graph.add_definition(Definition::AttrWriter(Box::new(def)))
            }
        };
        self.add_member(lexical_nesting_id, def_id);
    }

    fn apply_define_global_variable(&mut self, op: DefineGlobalVariable) {
        let lexical_nesting_id = self.current_scope_id();
        let member_owner_id = self.current_owner_id();
        let def = GlobalVariableDefinition::new(
            op.str_id,
            op.uri_id,
            op.offset,
            op.comments,
            op.flags,
            lexical_nesting_id,
        );
        let def_id = self
            .local_graph
            .add_definition(Definition::GlobalVariable(Box::new(def)));
        self.add_member(member_owner_id, def_id);
    }

    fn apply_define_instance_variable(&mut self, op: DefineInstanceVariable) {
        let lexical_nesting_id = self.current_scope_id();
        let member_owner_id = self.current_owner_id();
        let def = InstanceVariableDefinition::new(
            op.str_id,
            op.uri_id,
            op.offset,
            op.comments,
            op.flags,
            lexical_nesting_id,
        );
        let def_id = self
            .local_graph
            .add_definition(Definition::InstanceVariable(Box::new(def)));
        self.add_member(member_owner_id, def_id);
    }

    fn apply_define_class_variable(&mut self, op: DefineClassVariable) {
        let lexical_nesting_id = self.current_lexical_scope_id();
        let member_owner_id = self.current_owner_id();
        let def = ClassVariableDefinition::new(
            op.str_id,
            op.uri_id,
            op.offset,
            op.comments,
            op.flags,
            lexical_nesting_id,
        );
        let def_id = self
            .local_graph
            .add_definition(Definition::ClassVariable(Box::new(def)));
        self.add_member(member_owner_id, def_id);
    }

    fn apply_alias_global_variable(&mut self, op: AliasGlobalVariable) {
        let lexical_nesting_id = self.current_scope_id();
        let def = GlobalVariableAliasDefinition::new(
            op.new_name_str_id,
            op.old_name_str_id,
            op.uri_id,
            op.offset,
            op.comments,
            op.flags,
            lexical_nesting_id,
        );
        self.local_graph
            .add_definition(Definition::GlobalVariableAlias(Box::new(def)));
    }

    fn apply_reference_constant(&mut self, op: ReferenceConstant) {
        let ref_id = self
            .local_graph
            .add_constant_reference(ConstantReference::new(op.name_id, op.uri_id, op.offset));
        self.constant_ref_ids.insert(op.name_id, ref_id);
    }

    fn apply_reference_method(&mut self, op: ReferenceMethod) {
        let receiver = match op.receiver {
            Some(Target::Constant(name_id)) => Some(name_id),
            Some(Target::ExplicitSelf | Target::Other) | None => None,
        };
        self.local_graph
            .add_method_reference(MethodRef::new(op.str_id, op.uri_id, op.offset, receiver));
    }

    fn apply_reference_instance_variable(&mut self, op: ReferenceInstanceVariable) {
        // The lexical nesting at the point of the read mirrors the owner used for instance
        // variable definitions, so resolution can locate the declaration the read targets.
        let lexical_nesting_id = self.current_scope_id();

        self.local_graph
            .add_instance_variable_reference(InstanceVariableRef::new(
                op.str_id,
                op.uri_id,
                op.offset,
                lexical_nesting_id,
            ));
    }
}

/// Converts an `OperationBuilderResult` into a `LocalGraph`.
///
/// Walks the operations in order, creating `Definition` objects and registering members/mixins.
/// Scope context is derived from the scope stack maintained by Enter/Exit operations.
#[must_use]
pub fn apply_operations(result: OperationBuilderResult) -> LocalGraph {
    let OperationBuilderResult {
        uri_id,
        document,
        operations,
        strings,
        names,
    } = result;

    let mut applier = OperationApplier {
        local_graph: LocalGraph::from_parts(uri_id, document, strings, names),
        scope_stack: Vec::new(),
        scope_visibility: HashMap::new(),
        constant_ref_ids: HashMap::new(),
    };

    for op in operations {
        applier.apply_operation(op);
    }

    applier.local_graph
}

#[cfg(test)]
fn backend() -> crate::indexing::IndexerBackend {
    crate::indexing::IndexerBackend::OperationBuilder
}

#[cfg(test)]
#[allow(clippy::duplicate_mod)]
#[path = "../indexing/ruby_indexer_tests.rs"]
mod applier_tests;

#[cfg(test)]
#[allow(clippy::duplicate_mod)]
#[path = "../resolution_tests.rs"]
mod resolution_tests;
