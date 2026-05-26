//! Resolver-side semantic effects for Ruby method visibility calls.
//!
//! The indexer records source constructs first. This pass then applies Ruby's
//! retroactive visibility operations (`private :foo`, `module_function :foo`,
//! and class-method variants) to declarations, including the generated method
//! copies required by `module_function`.

mod module_function;
mod order;

use module_function::ModuleFunctionCopySource;
use order::DefinitionOrderCache;

use super::Resolver;
use crate::diagnostic::{Diagnostic, Rule};
use crate::model::{
    declaration::{Ancestor, Declaration, MethodDeclaration},
    definitions::Definition,
    graph::{DefinitionProgramOrderKey, Unit},
    identity_maps::IdentityHashSet,
    ids::{DeclarationId, DefinitionId, StringId},
    visibility::Visibility,
};

enum MethodVisibilityResolution {
    ApplyExisting,
    ApplyModuleFunctionCopy(ModuleFunctionCopySource),
    RetryPartial,
    Missing,
    Skip,
}

impl Resolver<'_> {
    /// Resolves retroactive method visibility changes (`private :foo`, `protected :foo`, `public :foo`,
    /// `private_class_method :foo`, `public_class_method :foo`).
    ///
    /// Runs as a second pass after all methods/attrs are declared. Visibility calls with method-name arguments require
    /// a method declaration that already exists at the call site. `module_function :bar` also copies that method
    /// snapshot into a public singleton method.
    pub(super) fn resolve_method_visibilities(&mut self, mut visibility_ids: Vec<DefinitionId>) {
        let mut pending_work = Vec::new();
        // This cache is intentionally scoped to one visibility-resolution pass.
        // Visibility definitions are processed in static program order, and each
        // lookup asks for the current visibility site or an earlier alias site.
        // That means a successful application cannot invalidate a previously
        // cached future-position lookup during this pass.
        let mut definition_order_cache = DefinitionOrderCache::default();
        let ordered_visibility_ids = {
            let mut seen_visibility_ids = IdentityHashSet::default();
            let mut ordered_visibility_ids = visibility_ids
                .drain(..)
                .filter(|id| seen_visibility_ids.insert(*id))
                .filter_map(|id| {
                    let (uri, offset) = self.graph.definition_program_order_key(id)?;
                    Some((id, uri, offset))
                })
                .collect::<Vec<_>>();

            ordered_visibility_ids.sort_unstable_by(
                |(left_id, left_uri, left_offset), (right_id, right_uri, right_offset)| {
                    DefinitionProgramOrderKey::new(*left_id, left_uri, left_offset)
                        .cmp(&DefinitionProgramOrderKey::new(*right_id, right_uri, right_offset))
                },
            );

            ordered_visibility_ids
                .into_iter()
                .map(|(id, _, _)| id)
                .collect::<Vec<_>>()
        };

        for id in ordered_visibility_ids {
            let Definition::MethodVisibility(method_visibility) = self.graph.definitions().get(&id).unwrap() else {
                unreachable!()
            };

            let str_id = *method_visibility.str_id();
            let uri_id = *method_visibility.uri_id();
            let offset = method_visibility.offset().clone();
            let lexical_nesting_id = *method_visibility.lexical_nesting_id();
            let is_singleton = method_visibility.flags().is_singleton_method_visibility();
            let visibility = *method_visibility.visibility();

            let Some(lexical_owner_id) = self.resolve_lexical_owner(lexical_nesting_id, id) else {
                continue;
            };

            let owner_id = if is_singleton {
                let Some(singleton_id) = self.get_or_create_singleton_class(lexical_owner_id, true) else {
                    continue;
                };
                singleton_id
            } else {
                lexical_owner_id
            };

            let resolution =
                self.method_visibility_resolution(owner_id, str_id, visibility, id, &mut definition_order_cache);
            match resolution {
                MethodVisibilityResolution::ApplyExisting => {
                    self.graph.mark_method_visibility_resolved(id);
                    self.create_method_visibility_declaration_for_owner(str_id, id, owner_id);
                    continue;
                }
                MethodVisibilityResolution::ApplyModuleFunctionCopy(source) => {
                    if self.apply_module_function(str_id, id, owner_id, source) {
                        self.graph.mark_method_visibility_resolved(id);
                        continue;
                    }
                }
                MethodVisibilityResolution::RetryPartial => {
                    self.graph.detach_method_visibility_declaration(id);
                    // Method might exist on an unresolved ancestor — requeue for retry.
                    pending_work.push(Unit::Definition(id));
                    continue;
                }
                MethodVisibilityResolution::Missing => {}
                MethodVisibilityResolution::Skip => continue,
            }

            // Ancestors are fully resolved — method definitively doesn't exist.
            self.graph.detach_method_visibility_declaration(id);
            self.graph.track_unresolved_method_visibility(owner_id, str_id, id);

            let method_name = self.graph.strings().get(&str_id).unwrap().as_str().to_string();
            let owner_name = self.graph.declarations().get(&owner_id).unwrap().name().to_string();
            let diagnostic = Diagnostic::new(
                Rule::UndefinedMethodVisibilityTarget,
                uri_id,
                offset,
                format!("undefined method `{owner_name}#{method_name}` for visibility change"),
            );
            self.graph
                .add_unresolved_method_visibility_diagnostic(owner_id, id, diagnostic);
        }

        // Must extend work here so incremental resolution can resolve previously unresolved visibility operations
        self.graph.extend_work(pending_work);
    }

    pub(in crate::resolution::method_visibility) fn create_method_visibility_declaration_for_owner(
        &mut self,
        str_id: StringId,
        visibility_id: DefinitionId,
        owner_id: DeclarationId,
    ) {
        // Direct member: `create_declaration`'s fully qualified name dedup attaches
        // this visibility definition to the existing method declaration.
        // Inherited: a new child-owned declaration is created.
        self.create_method_visibility_declaration(str_id, visibility_id, owner_id, |name| {
            Declaration::Method(Box::new(MethodDeclaration::new(name, owner_id)))
        });
    }

    fn method_visibility_resolution(
        &self,
        owner_id: DeclarationId,
        str_id: StringId,
        visibility: Visibility,
        visibility_id: DefinitionId,
        definition_order_cache: &mut DefinitionOrderCache,
    ) -> MethodVisibilityResolution {
        let Some(Declaration::Namespace(namespace)) = self.graph.declarations().get(&owner_id) else {
            return MethodVisibilityResolution::Skip;
        };

        let mut has_partial = false;

        for ancestor in namespace.ancestors() {
            match ancestor {
                Ancestor::Complete(ancestor_id) => {
                    let member_id = self
                        .graph
                        .declarations()
                        .get(ancestor_id)
                        .and_then(|decl| decl.as_namespace())
                        .and_then(|ns| ns.member(&str_id))
                        .copied();

                    if let Some(member_id) = member_id {
                        if visibility == Visibility::ModuleFunction {
                            let Some(target_visibility) = self.method_visibility_before_visibility_definition(
                                member_id,
                                visibility_id,
                                definition_order_cache,
                            ) else {
                                continue;
                            };
                            let Some(source) = self.module_function_copy_source(
                                member_id,
                                visibility_id,
                                owner_id,
                                target_visibility,
                                definition_order_cache,
                            ) else {
                                continue;
                            };

                            return MethodVisibilityResolution::ApplyModuleFunctionCopy(source);
                        }

                        if self.method_has_definition_before_visibility(
                            member_id,
                            visibility_id,
                            definition_order_cache,
                        ) {
                            return MethodVisibilityResolution::ApplyExisting;
                        }
                    }
                }
                Ancestor::Partial(_) => has_partial = true,
            }
        }

        if has_partial {
            MethodVisibilityResolution::RetryPartial
        } else {
            MethodVisibilityResolution::Missing
        }
    }

    fn create_method_visibility_declaration<F>(
        &mut self,
        str_id: StringId,
        definition_id: DefinitionId,
        owner_id: DeclarationId,
        declaration_builder: F,
    ) where
        F: FnOnce(String) -> Declaration,
    {
        let fully_qualified_name = self.member_fully_qualified_name(owner_id, str_id);

        let declaration_id = self.graph.add_method_visibility_declaration(
            definition_id,
            owner_id,
            str_id,
            fully_qualified_name,
            declaration_builder,
        );
        self.graph.add_member(&owner_id, declaration_id, str_id);
    }
}
