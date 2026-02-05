//! DSL processor types for matching and handling specific DSL patterns.

use crate::model::comment::Comment;
use crate::model::definitions::{Definition, DefinitionFlags, DynamicClassDefinition, DynamicModuleDefinition, Mixin};
use crate::model::graph::{CLASS_ID, Graph, MODULE_ID};
use crate::model::ids::{DeclarationId, DefinitionId, NameId, UriId};
use crate::model::name::{Name, ParentScope};
use crate::offset::Offset;

/// Type alias for DSL processor handle function.
/// Processes the DSL and mutates the graph accordingly.
///
/// Parameters:
/// - `graph`: The graph to mutate
/// - `dsl_def_id`: The DSL definition ID being processed
///
/// Returns the `DefinitionId` of any newly created definition.
pub type DslHandleFn = fn(graph: &mut Graph, dsl_def_id: DefinitionId) -> Option<DefinitionId>;

/// A DSL processor that can match and handle specific DSL patterns.
///
/// The `method_name` field is used for pre-filtering during indexing.
/// The `matches` function performs full matching after resolution, when the receiver has been
/// resolved to a `DeclarationId`.
#[derive(Clone, Copy, Debug)]
pub struct DslProcessor {
    /// The method name this processor is interested in (e.g., "new")
    pub method_name: &'static str,
    /// Returns true if this processor handles the given receiver/method combination
    pub matches: fn(receiver_decl_id: Option<DeclarationId>) -> bool,
    /// The handler function for this DSL pattern
    pub handle: DslHandleFn,
}

/// Matches `Class.new` DSL calls.
#[must_use]
pub fn class_new_matches(receiver_decl_id: Option<DeclarationId>) -> bool {
    receiver_decl_id == Some(*CLASS_ID)
}

/// Matches `Module.new` DSL calls.
#[must_use]
pub fn module_new_matches(receiver_decl_id: Option<DeclarationId>) -> bool {
    receiver_decl_id == Some(*MODULE_ID)
}

/// Result of attempting to match a DSL against processors.
pub enum DslMatchResult {
    /// A processor matched - use it to handle the DSL
    Matched(DslProcessor),
    /// No processor matched, but one might match after more resolution
    /// (e.g., receiver exists but not resolved yet)
    Retry,
    /// No processor will ever match this DSL
    /// (e.g., no receiver exists, or receiver resolved but doesn't match any processor)
    WillNotMatch,
}

/// Finds a matching processor for the given DSL, with detailed match result.
///
/// Parameters:
/// - `processors`: The available DSL processors
/// - `method_name`: The method name being called
/// - `receiver_name`: Whether a receiver exists in source (Some = exists, None = no receiver)
/// - `receiver_decl_id`: The resolved receiver declaration (if resolved)
///
/// Returns:
/// - `Matched(processor)` if a processor matches
/// - `Retry` if receiver exists but not resolved, and method could match a processor
/// - `WillNotMatch` if no processor will ever match
#[must_use]
pub fn find_matching_processor(
    processors: &[DslProcessor],
    method_name: &str,
    receiver_name: Option<NameId>,
    receiver_decl_id: Option<DeclarationId>,
) -> DslMatchResult {
    let mut could_match_later = false;

    for processor in processors {
        // Method name must match
        if processor.method_name != method_name {
            continue;
        }

        // Method matches - check receiver
        match (receiver_name, receiver_decl_id) {
            // Receiver resolved - check if it matches
            (Some(_), Some(decl_id)) => {
                if (processor.matches)(Some(decl_id)) {
                    return DslMatchResult::Matched(*processor);
                }
                // Receiver resolved but doesn't match this processor - try next
            }
            // Receiver exists but not resolved yet - could match later
            (Some(_), None) => {
                could_match_later = true;
            }
            // No receiver in source - can never match processors that need a receiver
            (None, _) => {
                // Check if this processor matches with no receiver
                if (processor.matches)(None) {
                    return DslMatchResult::Matched(*processor);
                }
                // This processor requires a receiver - try next
            }
        }
    }

    if could_match_later {
        DslMatchResult::Retry
    } else {
        DslMatchResult::WillNotMatch
    }
}

/// Common data extracted from a DSL definition and its associated constant.
struct DslContext {
    uri_id: UriId,
    offset: Offset,
    lexical_nesting_id: Option<DefinitionId>,
    mixins: Vec<Mixin>,
    constant_def_id: Option<DefinitionId>,
    name_id: Option<NameId>,
    comments: Vec<Comment>,
    flags: DefinitionFlags,
}

/// Gets the reparented name from a processed parent DSL (`DynamicClass` or `DynamicModule`).
/// Uses the deterministic ID format to look up directly without iteration.
/// Returns None if the parent hasn't been processed yet or has no name.
fn get_processed_parent_name(graph: &Graph, parent_dsl_id: DefinitionId) -> Option<NameId> {
    // Try DynamicClass first (more common)
    let dynamic_class_id = DynamicClassDefinition::id_for_dsl(parent_dsl_id);
    if let Some(Definition::DynamicClass(dc)) = graph.definitions().get(&dynamic_class_id) {
        return dc.name_id().copied();
    }

    // Try DynamicModule
    let dynamic_module_id = DynamicModuleDefinition::id_for_dsl(parent_dsl_id);
    if let Some(Definition::DynamicModule(dm)) = graph.definitions().get(&dynamic_module_id) {
        return dm.name_id().copied();
    }

    None
}

/// Returns true if the parent DSL has been processed (converted to DynamicClass/Module).
/// Uses the deterministic ID format to check directly without iteration.
#[must_use]
pub fn is_parent_dsl_processed(graph: &Graph, parent_dsl_id: DefinitionId) -> bool {
    let dynamic_class_id = DynamicClassDefinition::id_for_dsl(parent_dsl_id);
    if graph.definitions().contains_key(&dynamic_class_id) {
        return true;
    }

    let dynamic_module_id = DynamicModuleDefinition::id_for_dsl(parent_dsl_id);
    graph.definitions().contains_key(&dynamic_module_id)
}

/// Extracts common data from a DSL definition and computes the reparented name.
fn extract_dsl_context(graph: &mut Graph, dsl_def_id: DefinitionId) -> Option<DslContext> {
    let (uri_id, offset, lexical_nesting_id, mixins, constant_def_id) = {
        let Definition::Dsl(dsl_def) = graph.definitions().get(&dsl_def_id)? else {
            return None;
        };
        (
            *dsl_def.uri_id(),
            dsl_def.offset().clone(),
            *dsl_def.lexical_nesting_id(),
            dsl_def.mixins().to_vec(),
            dsl_def.assigned_to(),
        )
    };

    let (original_name_id, comments, flags, parent_dsl_id) = constant_def_id
        .and_then(|id| {
            let Definition::Constant(c) = graph.definitions().get(&id)? else {
                return None;
            };
            Some((
                Some(*c.name_id()),
                c.comments().to_vec(),
                c.flags().clone(),
                *c.lexical_nesting_id(),
            ))
        })
        .unwrap_or_else(|| (None, Vec::new(), DefinitionFlags::empty(), None));

    // Eagerly reparent the name if nested inside another DynamicClass/Module
    // Parent must already be processed (depth-first ordering guarantees this)
    let name_id = match (original_name_id, parent_dsl_id) {
        (Some(orig_name), Some(parent_id)) => {
            if let Some(parent_name_id) = get_processed_parent_name(graph, parent_id) {
                Some(create_reparented_name(graph, orig_name, parent_name_id))
            } else {
                Some(orig_name)
            }
        }
        (Some(orig_name), None) => Some(orig_name),
        (None, _) => None,
    };

    Some(DslContext {
        uri_id,
        offset,
        lexical_nesting_id,
        mixins,
        constant_def_id,
        name_id,
        comments,
        flags,
    })
}

/// Creates a reparented name by setting the parent name.
fn create_reparented_name(graph: &mut Graph, original_name_id: NameId, parent_name_id: NameId) -> NameId {
    let name_ref = graph.names().get(&original_name_id).expect("name must exist");
    let new_name = Name::new(*name_ref.str(), ParentScope::Some(parent_name_id), *name_ref.nesting());
    let new_name_id = new_name.id();
    graph.add_name(new_name);
    new_name_id
}

/// Handles `Class.new` DSL calls by creating a `DynamicClassDefinition`.
///
/// Creates a dynamic class definition from the DSL, reparenting names if nested.
/// The original `ConstantDefinition` is removed after creating the dynamic definition.
pub fn handle_class_new(graph: &mut Graph, dsl_def_id: DefinitionId) -> Option<DefinitionId> {
    // Get superclass reference from DSL arguments before extracting context
    let superclass_ref = {
        let Definition::Dsl(dsl_def) = graph.definitions().get(&dsl_def_id)? else {
            return None;
        };
        dsl_def.arguments().first_positional_reference()
    };

    let ctx = extract_dsl_context(graph, dsl_def_id)?;

    let dynamic_class = DynamicClassDefinition::new(
        dsl_def_id,
        ctx.name_id,
        superclass_ref,
        ctx.comments,
        ctx.flags,
        ctx.uri_id,
        ctx.offset,
        ctx.lexical_nesting_id,
        ctx.mixins,
    );

    let definition_id = dynamic_class.id();
    graph
        .definitions_mut()
        .insert(definition_id, Definition::DynamicClass(Box::new(dynamic_class)));

    if let Some(id) = ctx.constant_def_id {
        graph.definitions_mut().remove(&id);
        Some(definition_id)
    } else {
        None
    }
}

/// Handles `Module.new` DSL calls by creating a `DynamicModuleDefinition`.
///
/// Creates a dynamic module definition from the DSL, reparenting names if nested.
/// The original `ConstantDefinition` is removed after creating the dynamic definition.
pub fn handle_module_new(graph: &mut Graph, dsl_def_id: DefinitionId) -> Option<DefinitionId> {
    let ctx = extract_dsl_context(graph, dsl_def_id)?;

    let dynamic_module = DynamicModuleDefinition::new(
        dsl_def_id,
        ctx.name_id,
        ctx.comments,
        ctx.flags,
        ctx.uri_id,
        ctx.offset,
        ctx.lexical_nesting_id,
        ctx.mixins,
    );

    let definition_id = dynamic_module.id();
    graph
        .definitions_mut()
        .insert(definition_id, Definition::DynamicModule(Box::new(dynamic_module)));

    if let Some(id) = ctx.constant_def_id {
        graph.definitions_mut().remove(&id);
        Some(definition_id)
    } else {
        None
    }
}
