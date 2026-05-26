use super::{DefinitionOrderCache, Resolver};
use crate::model::{
    comment::Comment,
    declaration::{Declaration, MethodDeclaration},
    definitions::{Definition, DefinitionFlags, MethodDefinition, Parameter, ParameterStruct, Receiver, Signatures},
    identity_maps::IdentityHashSet,
    ids::{DeclarationId, DefinitionId, StringId, UriId},
    visibility::Visibility,
};
use crate::offset::Offset;

pub(in crate::resolution::method_visibility) struct ModuleFunctionCopySource {
    definition_id: DefinitionId,
    target_visibility: Visibility,
    source_kind: ModuleFunctionSourceKind,
    alias_definition_ids: Vec<DefinitionId>,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum ModuleFunctionSourceKind {
    DirectMethod,
    DirectAlias,
    InheritedMethod,
    InheritedAlias,
}

impl ModuleFunctionSourceKind {
    fn new(target_is_direct: bool, uses_alias: bool) -> Self {
        match (target_is_direct, uses_alias) {
            (true, false) => Self::DirectMethod,
            (true, true) => Self::DirectAlias,
            (false, false) => Self::InheritedMethod,
            (false, true) => Self::InheritedAlias,
        }
    }

    fn should_create_instance_copy(self, target_visibility: Visibility) -> bool {
        match self {
            Self::DirectMethod => false,
            Self::DirectAlias => true,
            Self::InheritedMethod | Self::InheritedAlias => target_visibility != Visibility::Private,
        }
    }

    fn is_direct_method(self) -> bool {
        self == Self::DirectMethod
    }

    fn should_track_ancestor_dependency(self, source_declared_on_owner: bool) -> bool {
        !source_declared_on_owner && !self.is_direct_method()
    }
}

struct ModuleFunctionCopyRequest {
    str_id: StringId,
    visibility_id: DefinitionId,
    source_definition_id: DefinitionId,
    owner_id: DeclarationId,
    copy_declaration_id: DeclarationId,
    fully_qualified_name: String,
}

struct MethodBodySnapshot {
    uri_id: UriId,
    offset: Offset,
    comments: Box<[Comment]>,
    flags: DefinitionFlags,
    signatures: Signatures,
}

struct ModuleFunctionCopyDefinitionRequest {
    str_id: StringId,
    owner_definition_id: DefinitionId,
    visibility: Visibility,
    receiver: Option<Receiver>,
    generated: bool,
}

impl Resolver<'_> {
    pub(in crate::resolution::method_visibility) fn apply_module_function(
        &mut self,
        str_id: StringId,
        visibility_id: DefinitionId,
        owner_id: DeclarationId,
        source: ModuleFunctionCopySource,
    ) -> bool {
        let Some(owner_definition_id) = self
            .graph
            .definitions()
            .get(&visibility_id)
            .and_then(|definition| *definition.lexical_nesting_id())
        else {
            return false;
        };

        let source_definition_id = source.definition_id;
        let source_declared_on_owner = self.source_definition_declared_on_owner(owner_id, source_definition_id);
        // Ruby only installs a direct instance method copy for inherited module_function
        // sources when the inherited method was public/protected at the call site. Direct
        // methods are handled by the MethodVisibilityDefinition, and direct aliases need
        // their own private copy because the alias body can come from another declaration.
        // Inherited private sources keep using the ancestor method while still getting a
        // singleton copy.
        if source.source_kind.should_create_instance_copy(source.target_visibility)
            && !self.create_module_function_instance_copy(
                str_id,
                visibility_id,
                owner_id,
                source_definition_id,
                owner_definition_id,
            )
        {
            return false;
        }

        if !self.create_module_function_singleton_copy(
            str_id,
            visibility_id,
            owner_id,
            source_definition_id,
            owner_definition_id,
        ) {
            return false;
        }

        if source.source_kind.is_direct_method() {
            self.create_method_visibility_declaration_for_owner(str_id, visibility_id, owner_id);
        } else if source
            .source_kind
            .should_track_ancestor_dependency(source_declared_on_owner)
        {
            self.graph
                .track_module_function_ancestor_dependency(owner_id, str_id, visibility_id);
        }

        for alias_id in source.alias_definition_ids {
            self.graph
                .track_module_function_alias_dependency(alias_id, visibility_id);
        }

        true
    }

    fn source_definition_declared_on_owner(&self, owner_id: DeclarationId, source_definition_id: DefinitionId) -> bool {
        self.graph
            .definition_id_to_declaration_id(source_definition_id)
            .and_then(|declaration_id| self.graph.declarations().get(declaration_id))
            .is_some_and(|declaration| *declaration.owner_id() == owner_id)
    }

    fn create_module_function_instance_copy(
        &mut self,
        str_id: StringId,
        visibility_id: DefinitionId,
        owner_id: DeclarationId,
        source_definition_id: DefinitionId,
        owner_definition_id: DefinitionId,
    ) -> bool {
        let fully_qualified_name = self.member_fully_qualified_name(owner_id, str_id);
        let copy_declaration_id = DeclarationId::from(&fully_qualified_name);
        self.create_module_function_copy(
            ModuleFunctionCopyRequest {
                str_id,
                visibility_id,
                source_definition_id,
                owner_id,
                copy_declaration_id,
                fully_qualified_name,
            },
            |resolver| {
                resolver.build_module_function_copy_definition(
                    str_id,
                    source_definition_id,
                    owner_definition_id,
                    Visibility::Private,
                    None,
                    true,
                )
            },
        )
    }

    fn create_module_function_singleton_copy(
        &mut self,
        str_id: StringId,
        visibility_id: DefinitionId,
        owner_id: DeclarationId,
        source_definition_id: DefinitionId,
        owner_definition_id: DefinitionId,
    ) -> bool {
        let Some(singleton_id) = self.get_or_create_singleton_class(owner_id, true) else {
            return false;
        };

        let fully_qualified_name = self.member_fully_qualified_name(singleton_id, str_id);
        let copy_declaration_id = DeclarationId::from(&fully_qualified_name);

        self.create_module_function_copy(
            ModuleFunctionCopyRequest {
                str_id,
                visibility_id,
                source_definition_id,
                owner_id: singleton_id,
                copy_declaration_id,
                fully_qualified_name,
            },
            |resolver| {
                resolver.build_module_function_copy_definition(
                    str_id,
                    source_definition_id,
                    owner_definition_id,
                    Visibility::Public,
                    Some(Receiver::SelfReceiver(owner_definition_id)),
                    false,
                )
            },
        )
    }

    fn create_module_function_copy<F>(&mut self, request: ModuleFunctionCopyRequest, build_copy_definition: F) -> bool
    where
        F: FnOnce(&mut Self) -> Option<Definition>,
    {
        let ModuleFunctionCopyRequest {
            str_id,
            visibility_id,
            source_definition_id,
            owner_id,
            copy_declaration_id,
            fully_qualified_name,
        } = request;

        let copy_definition_id = if let Some(copy_definition_id) = self
            .graph
            .find_module_function_copy(source_definition_id, copy_declaration_id)
        {
            self.graph
                .attach_module_function_copy_visibility(copy_definition_id, visibility_id);
            copy_definition_id
        } else {
            let Some(copy_definition) = build_copy_definition(self) else {
                return false;
            };

            self.graph.add_module_function_copy(
                source_definition_id,
                visibility_id,
                copy_declaration_id,
                copy_definition,
            )
        };

        if self
            .graph
            .declarations()
            .get(&copy_declaration_id)
            .is_some_and(|declaration| declaration.definitions().contains(&copy_definition_id))
        {
            self.graph.add_member(&owner_id, copy_declaration_id, str_id);
            return true;
        }

        let copy_declaration_id = self.graph.add_module_function_copy_declaration(
            copy_definition_id,
            visibility_id,
            fully_qualified_name,
            |name| Declaration::Method(Box::new(MethodDeclaration::new(name, owner_id))),
        );
        self.graph.add_member(&owner_id, copy_declaration_id, str_id);

        true
    }

    pub(in crate::resolution::method_visibility) fn module_function_copy_source(
        &self,
        source_method_id: DeclarationId,
        visibility_id: DefinitionId,
        owner_id: DeclarationId,
        target_visibility: Visibility,
        definition_order_cache: &mut DefinitionOrderCache,
    ) -> Option<ModuleFunctionCopySource> {
        let visibility_definition = self.graph.definitions().get(&visibility_id)?;
        let visibility_uri_id = *visibility_definition.uri_id();
        let visibility_offset = visibility_definition.offset();

        let mut seen_method_ids = IdentityHashSet::default();
        let mut source = self.module_function_copy_source_before(
            source_method_id,
            visibility_uri_id,
            visibility_offset,
            target_visibility,
            &mut seen_method_ids,
            definition_order_cache,
        )?;
        let target_is_direct = self.method_declaration_declared_on_owner(source_method_id, owner_id);
        source.source_kind = ModuleFunctionSourceKind::new(target_is_direct, !source.alias_definition_ids.is_empty());
        Some(source)
    }

    fn module_function_copy_source_before(
        &self,
        source_method_id: DeclarationId,
        uri_id: UriId,
        offset: &Offset,
        target_visibility: Visibility,
        seen_method_ids: &mut IdentityHashSet<DeclarationId>,
        definition_order_cache: &mut DefinitionOrderCache,
    ) -> Option<ModuleFunctionCopySource> {
        if !seen_method_ids.insert(source_method_id) {
            return None;
        }

        self.definition_ids_before_visibility(source_method_id, uri_id, offset, definition_order_cache)?
            .iter()
            .copied()
            .rev()
            .find_map(|definition_id| {
                let definition = self.graph.definitions().get(&definition_id)?;

                if definition.is_copyable_method_body() {
                    return Some(ModuleFunctionCopySource {
                        definition_id,
                        target_visibility,
                        source_kind: ModuleFunctionSourceKind::InheritedMethod,
                        alias_definition_ids: Vec::new(),
                    });
                }

                let Definition::MethodAlias(alias) = definition else {
                    return None;
                };

                let owner_id = self
                    .graph
                    .definition_id_to_declaration_id(definition_id)
                    .and_then(|decl_id| self.graph.declarations().get(decl_id))
                    .map(|declaration| *declaration.owner_id())?;
                for target_id in self.method_member_ids_in_ancestors_before(
                    owner_id,
                    *alias.old_name_str_id(),
                    *definition.uri_id(),
                    definition.offset(),
                    definition_order_cache,
                ) {
                    let Some(mut source) = self.module_function_copy_source_before(
                        target_id,
                        *definition.uri_id(),
                        definition.offset(),
                        target_visibility,
                        seen_method_ids,
                        definition_order_cache,
                    ) else {
                        continue;
                    };
                    source.alias_definition_ids.push(definition_id);
                    return Some(source);
                }

                None
            })
    }

    fn method_declaration_declared_on_owner(&self, method_id: DeclarationId, owner_id: DeclarationId) -> bool {
        self.graph
            .declarations()
            .get(&method_id)
            .is_some_and(|declaration| *declaration.owner_id() == owner_id)
    }

    fn build_module_function_copy_definition(
        &mut self,
        copy_str_id: StringId,
        source_definition_id: DefinitionId,
        owner_definition_id: DefinitionId,
        visibility: Visibility,
        receiver: Option<Receiver>,
        generated: bool,
    ) -> Option<Definition> {
        let snapshot = self.module_function_body_snapshot(source_definition_id)?;

        Some(Self::module_function_copy_from_snapshot(
            ModuleFunctionCopyDefinitionRequest {
                str_id: copy_str_id,
                owner_definition_id,
                visibility,
                receiver,
                generated,
            },
            snapshot,
        ))
    }

    fn module_function_body_snapshot(&mut self, source_definition_id: DefinitionId) -> Option<MethodBodySnapshot> {
        let source_definition = self.graph.definitions().get(&source_definition_id)?;

        match source_definition {
            Definition::Method(method) => Some(MethodBodySnapshot {
                uri_id: *method.uri_id(),
                offset: method.offset().clone(),
                comments: method.comments().to_vec().into_boxed_slice(),
                flags: method.flags().clone(),
                signatures: method.signatures().clone(),
            }),
            Definition::AttrAccessor(attr) => Some(Self::module_function_attr_body_snapshot(
                *attr.uri_id(),
                attr.offset(),
                attr.comments(),
                attr.flags(),
            )),
            Definition::AttrReader(attr) => Some(Self::module_function_attr_body_snapshot(
                *attr.uri_id(),
                attr.offset(),
                attr.comments(),
                attr.flags(),
            )),
            Definition::AttrWriter(attr) => {
                let uri_id = *attr.uri_id();
                let offset = attr.offset().clone();
                let comments = attr.comments().to_vec().into_boxed_slice();
                let flags = attr.flags().clone();
                let arg_str_id = self.graph.prepare_generated_string("__rubydex_arg0".to_string());
                Some(Self::module_function_attr_body_snapshot_from_parts(
                    uri_id,
                    offset,
                    comments,
                    flags,
                    Some(arg_str_id),
                ))
            }
            _ => None,
        }
    }

    fn module_function_attr_body_snapshot(
        uri_id: UriId,
        offset: &Offset,
        comments: &[Comment],
        flags: &DefinitionFlags,
    ) -> MethodBodySnapshot {
        Self::module_function_attr_body_snapshot_from_parts(
            uri_id,
            offset.clone(),
            comments.to_vec().into_boxed_slice(),
            flags.clone(),
            None,
        )
    }

    fn module_function_attr_body_snapshot_from_parts(
        uri_id: UriId,
        offset: Offset,
        comments: Box<[Comment]>,
        flags: DefinitionFlags,
        writer_arg_str_id: Option<StringId>,
    ) -> MethodBodySnapshot {
        let signatures = if let Some(arg_str_id) = writer_arg_str_id {
            Signatures::Simple(
                vec![Parameter::RequiredPositional(ParameterStruct::new(
                    offset.clone(),
                    arg_str_id,
                ))]
                .into_boxed_slice(),
            )
        } else {
            Signatures::Simple(Box::default())
        };

        MethodBodySnapshot {
            uri_id,
            offset,
            comments,
            flags,
            signatures,
        }
    }

    fn module_function_copy_from_snapshot(
        request: ModuleFunctionCopyDefinitionRequest,
        snapshot: MethodBodySnapshot,
    ) -> Definition {
        let ModuleFunctionCopyDefinitionRequest {
            str_id,
            owner_definition_id,
            visibility,
            receiver,
            generated,
        } = request;
        let MethodBodySnapshot {
            uri_id,
            offset,
            comments,
            mut flags,
            signatures,
        } = snapshot;

        // A generated source copy may be copied again. Recompute GENERATED from
        // this copy's role so singleton copies can still collide with inline
        // `module_function def` definitions while instance copies stay distinct.
        flags.remove(DefinitionFlags::GENERATED);

        let method = if generated {
            MethodDefinition::new_generated(
                str_id,
                uri_id,
                offset,
                comments,
                flags,
                Some(owner_definition_id),
                signatures,
                visibility,
                receiver,
            )
        } else {
            MethodDefinition::new(
                str_id,
                uri_id,
                offset,
                comments,
                flags,
                Some(owner_definition_id),
                signatures,
                visibility,
                receiver,
            )
        };

        Definition::Method(Box::new(method))
    }
}
