use std::collections::HashSet;
use std::error::Error;
use std::path::PathBuf;
use std::thread;

use url::Url;

use crate::model::built_in::OBJECT_ID;
use crate::model::declaration::{Ancestor, Declaration, Namespace};
use crate::model::definitions::{Definition, Parameter};
use crate::model::graph::Graph;
use crate::model::identity_maps::IdentityHashSet;
use crate::model::ids::{DeclarationId, DefinitionId, NameId, StringId, UriId};
use crate::model::keywords::{self, Keyword};
use crate::model::name::NameRef;

/// Controls how declaration names are matched against the search query.
#[derive(Default)]
pub enum MatchMode {
    /// Fuzzy matching: query characters must appear in order in the target (case-insensitive). Used for LSP workspace
    /// symbol.
    #[default]
    Fuzzy,
    /// Exact partial matching: query must appear as a contiguous substring of the target. Used for precise filtering
    /// (e.g., finding all declarations containing `#is_a?()`).
    Exact,
}

/// # Panics
///
/// Will panic if any of the threads panic
pub fn declaration_search(graph: &Graph, query: &str, match_mode: &MatchMode) -> Vec<DeclarationId> {
    let num_threads = thread::available_parallelism().map_or(4, std::num::NonZero::get);
    let declarations = graph.declarations();
    let ids: Vec<DeclarationId> = declarations.keys().copied().collect();
    let chunk_size = ids.len().div_ceil(num_threads);

    if chunk_size == 0 {
        return Vec::new();
    }

    thread::scope(|s| {
        let handles: Vec<_> = ids
            .chunks(chunk_size)
            .map(|chunk| {
                s.spawn(|| {
                    chunk
                        .iter()
                        .filter(|id| {
                            let declaration = declarations.get(id).unwrap();
                            let name = declaration.name();
                            match match_mode {
                                MatchMode::Fuzzy => {
                                    // When the query is empty, we return everything as per the LSP specification.
                                    // Otherwise, we compute the match score and return anything with a score greater than zero
                                    query.is_empty() || match_score(query, name) > 0
                                }
                                MatchMode::Exact => name.contains(query),
                            }
                        })
                        .copied()
                        .collect::<Vec<_>>()
                })
            })
            .collect();

        handles.into_iter().flat_map(|h| h.join().unwrap()).collect()
    })
}

#[must_use]
fn match_score(query: &str, target: &str) -> usize {
    let mut query_chars = query.chars().peekable();
    let mut score = 0;

    // Count the number of matches in the order of the query, so that character ordering is taken into account
    for t_char in target.chars() {
        if let Some(&q_char) = query_chars.peek()
            && q_char.eq_ignore_ascii_case(&t_char)
        {
            score += 1;
            query_chars.next();
        }
    }

    // If after going through the target, there are still query characters left, then some of the query can't be found
    // in this target and we return zero to indicate a non-match
    if query_chars.peek().is_some() { 0 } else { score }
}

/// Resolves a require path to its URI ID. Used for go-to-definition.
///
/// Searches the `load_path` in order and returns the first match, mirroring how Ruby's `require`
/// walks `$LOAD_PATH`.
#[must_use]
pub fn resolve_require_path(graph: &Graph, require_path: &str, load_path: &[PathBuf]) -> Option<UriId> {
    let normalized = require_path.trim_end_matches(".rb");

    for path in load_path {
        let file_path = path.join(format!("{normalized}.rb"));
        let Ok(url) = Url::from_file_path(&file_path) else {
            continue;
        };
        let uri_id = UriId::from(url.as_str());
        if graph.documents().contains_key(&uri_id) {
            return Some(uri_id);
        }
    }

    None
}

/// Returns all require paths. Used for completion.
///
/// When multiple files resolve to the same require path (e.g., `foo.rb` exists in multiple
/// load paths), the one from the earliest load path wins. This matches Ruby's `require` behavior.
///
/// # Panics
///
/// Panics if one of the search threads panics
#[must_use]
pub fn require_paths(graph: &Graph, load_paths: &[PathBuf]) -> Vec<String> {
    let num_threads = thread::available_parallelism().map_or(4, std::num::NonZero::get);
    let documents = graph.documents().iter().collect::<Vec<_>>();
    let chunk_size = documents.len().div_ceil(num_threads);

    if chunk_size == 0 {
        return Vec::new();
    }

    let mut all_results = thread::scope(|scope| {
        let handles: Vec<_> = documents
            .chunks(chunk_size)
            .map(|chunk| {
                scope.spawn(move || {
                    chunk
                        .iter()
                        .filter_map(|(_, document)| document.require_path(load_paths))
                        .collect::<Vec<_>>()
                })
            })
            .collect();

        handles
            .into_iter()
            .flat_map(|handle| handle.join().unwrap())
            .collect::<Vec<_>>()
    });

    // Sort by load path index so earlier load paths win during deduplication
    all_results.sort_by_key(|(_, index)| *index);

    let mut seen = HashSet::new();
    all_results
        .into_iter()
        .filter(|(require_path, _)| seen.insert(require_path.clone()))
        .map(|(require_path, _)| require_path)
        .collect()
}

/// A completion candidate
pub enum CompletionCandidate {
    Declaration(DeclarationId),
    KeywordArgument(StringId),
    Keyword(&'static Keyword),
}

/// The context in which completion is being requested
pub enum CompletionReceiver {
    /// Completion requested for an expression with no previous token (e.g.: at the start of a line with nothing before)
    /// Includes: all keywords, all global variables and reacheable instance variables, class variables, constants and methods
    ///
    /// `nesting_name_id` represents the lexical scope. It is walked for constants and drives class-variable
    /// lookup (cvars follow lexical scope in Ruby, not self).
    /// `self_decl_id` overrides the self-type used for methods and instance variables when the runtime `self`
    /// diverges from the innermost lexical scope — for example `def Foo.bar` (where self is `Foo` but the
    /// lexical scope is the outer namespace) or `def self.bar`. Callers may pass a `ConstantAlias` id; it is
    /// unwrapped to the target namespace. When `None`, self is derived from the innermost lexical scope.
    Expression {
        self_decl_id: Option<DeclarationId>,
        nesting_name_id: NameId,
    },
    /// Completion requested after a namespace access operator (e.g.: `Foo::`)
    /// Includes: all constants and singleton methods for the namespace and its ancestors
    NamespaceAccess(DeclarationId),
    /// Completion requested after a method call operator (e.g.: `foo.`, `@bar.`, `@@baz.`, `Qux.`).
    /// In the case of singleton completion (e.g.: `Foo.`), the declaration ID should be for the singleton class (i.e.: `Foo::<Foo>`)
    /// Includes: all methods that exist on the type of the receiver and its ancestors
    MethodCall(DeclarationId),
    /// Completion requested inside a method call's argument list (e.g.: `foo.bar(|)`)
    /// Includes: everything expressions do plus keyword parameter names of the method being called
    ///
    /// Same `self_decl_id` / `nesting_name_id` split as `Expression`.
    MethodArgument {
        self_decl_id: Option<DeclarationId>,
        nesting_name_id: NameId,
        method_decl_id: DeclarationId,
    },
}

pub struct CompletionContext<'a> {
    seen_members: IdentityHashSet<&'a StringId>,
    completion_receiver: CompletionReceiver,
}

impl<'a> CompletionContext<'a> {
    #[must_use]
    pub fn new(completion_receiver: CompletionReceiver) -> Self {
        Self {
            seen_members: IdentityHashSet::default(),
            completion_receiver,
        }
    }

    pub fn dedup(&mut self, member_str_id: &'a StringId) -> bool {
        self.seen_members.insert(member_str_id)
    }
}

/// Collects completion candidate members
macro_rules! collect_candidates {
    // Collect all members with no filtering
    ($declaration:expr, $context:expr, $candidates:expr) => {
        for (member_str_id, member_declaration_id) in $declaration.members() {
            if $context.dedup(member_str_id) {
                $candidates.push(CompletionCandidate::Declaration(*member_declaration_id));
            }
        }
    };
    // Collect only members matching certain kinds
    ($graph:expr, $declaration:expr, $context:expr, $candidates:expr, $kinds:pat) => {
        for (member_str_id, member_declaration_id) in $declaration.members() {
            let member = $graph.declarations().get(member_declaration_id).unwrap();

            if matches!(member, $kinds) && $context.dedup(member_str_id) {
                $candidates.push(CompletionCandidate::Declaration(*member_declaration_id));
            }
        }
    };
}

/// Determines all possible completion candidates based on the current context of the cursor. There are multiple cases
/// that change what has to be collected for completion:
///
/// - Expressions collect all keywords, constants, methods, instance variables, class variables, local variables and
///   global variables that are reacheable from the current lexical scope and self type
/// - Expression in method arguments collects everything that expressions do and all keyword parameter names that are
///   applicable to the method being called
///   everything else
/// - Namespace access (e.g.: `Foo::`) collects all constants and singleton methods for the namespace that `Foo`
///   resolves to
/// - Method calls on anything (e.g.: `foo.`, `@bar.`, `@@baz.`, `Qux.`) collects all methods that exist on the type
///   returned by the receiver
///
/// # Panics
///
/// Will panic if we incorrectly inserted non namespace declarations as ancestors
///
/// # Errors
///
/// Will error if the given `self_decl_id` does not resolve to a namespace declaration (directly or via
/// a constant alias).
pub fn completion_candidates<'a>(
    graph: &'a Graph,
    context: CompletionContext<'a>,
) -> Result<Vec<CompletionCandidate>, Box<dyn Error>> {
    match context.completion_receiver {
        CompletionReceiver::Expression {
            self_decl_id,
            nesting_name_id,
        } => expression_completion(graph, self_decl_id, nesting_name_id, context),
        CompletionReceiver::NamespaceAccess(decl_id) => namespace_access_completion(graph, decl_id, context),
        CompletionReceiver::MethodCall(decl_id) => method_call_completion(graph, decl_id, context),
        CompletionReceiver::MethodArgument {
            self_decl_id,
            nesting_name_id,
            method_decl_id,
        } => method_argument_completion(graph, self_decl_id, nesting_name_id, method_decl_id, context),
    }
}

/// Resolves a declaration ID to a namespace, following constant aliases if necessary.
///
/// Returns:
/// - `Ok(Some(id))` if the declaration is a namespace (directly or via alias)
/// - `Ok(None)` if the declaration does not exist in the graph
/// - `Err(...)` if the declaration exists but is not a namespace or alias to a namespace
fn resolve_to_namespace(graph: &Graph, decl_id: DeclarationId) -> Result<Option<DeclarationId>, Box<dyn Error>> {
    match graph.declarations().get(&decl_id) {
        Some(Declaration::Namespace(_)) => Ok(Some(decl_id)),
        None => Ok(None),
        Some(_) => {
            if let Some(target_id) = graph.resolve_alias(&decl_id)
                && let Some(Declaration::Namespace(_)) = graph.declarations().get(&target_id)
            {
                Ok(Some(target_id))
            } else {
                Err(format!("Expected declaration {decl_id:?} to be a namespace or alias to a namespace").into())
            }
        }
    }
}

/// Collect completion for a namespace access (e.g.: `Foo::`)
fn namespace_access_completion<'a>(
    graph: &'a Graph,
    namespace_decl_id: DeclarationId,
    mut context: CompletionContext<'a>,
) -> Result<Vec<CompletionCandidate>, Box<dyn Error>> {
    let Some(resolved_id) = resolve_to_namespace(graph, namespace_decl_id)? else {
        return Ok(Vec::new());
    };
    let namespace = graph.declarations().get(&resolved_id).unwrap().as_namespace().unwrap();
    let mut candidates = Vec::new();

    // Walk ancestors collecting inherited constants, stopping at Object to avoid surfacing top-level constants
    // from Object, Kernel, BasicObject, etc.
    for ancestor in namespace.ancestors() {
        if let Ancestor::Complete(ancestor_id) = ancestor {
            // Do not offer completion for constants inherited after `Object` (e.g.: `Object::String`). While this is
            // valid Ruby code, it's extremely uncommon and not a super valuable completion suggestion
            if *ancestor_id == *OBJECT_ID {
                break;
            }

            let ancestor_decl = graph.declarations().get(ancestor_id).unwrap().as_namespace().unwrap();

            collect_candidates!(
                graph,
                &ancestor_decl,
                context,
                candidates,
                Declaration::Namespace(_) | Declaration::Constant(_) | Declaration::ConstantAlias(_)
            );
        }
    }

    // Collect singleton methods from the singleton class and its ancestors
    if let Some(singleton_id) = namespace.singleton_class() {
        let singleton = graph.declarations().get(singleton_id).unwrap().as_namespace().unwrap();

        for ancestor in singleton.ancestors() {
            if let Ancestor::Complete(ancestor_id) = ancestor {
                let ancestor_decl = graph.declarations().get(ancestor_id).unwrap().as_namespace().unwrap();
                collect_candidates!(graph, &ancestor_decl, context, candidates, Declaration::Method(_));
            }
        }
    }

    Ok(candidates)
}

/// Collect completion for a method call (e.g.: `foo.`, `@bar.`, `Baz.`)
fn method_call_completion<'a>(
    graph: &'a Graph,
    receiver_decl_id: DeclarationId,
    mut context: CompletionContext<'a>,
) -> Result<Vec<CompletionCandidate>, Box<dyn Error>> {
    let Some(resolved_id) = resolve_to_namespace(graph, receiver_decl_id)? else {
        return Ok(Vec::new());
    };
    let namespace = graph.declarations().get(&resolved_id).unwrap().as_namespace().unwrap();
    let mut candidates = Vec::new();

    for ancestor in namespace.ancestors() {
        if let Ancestor::Complete(ancestor_id) = ancestor {
            let ancestor_decl = graph.declarations().get(ancestor_id).unwrap().as_namespace().unwrap();
            collect_candidates!(graph, &ancestor_decl, context, candidates, Declaration::Method(_));
        }
    }

    Ok(candidates)
}

/// Collect completion for an expression
fn expression_completion<'a>(
    graph: &'a Graph,
    self_decl_id: Option<DeclarationId>,
    nesting_name_id: NameId,
    mut context: CompletionContext<'a>,
) -> Result<Vec<CompletionCandidate>, Box<dyn Error>> {
    let Some(name_ref) = graph.names().get(&nesting_name_id) else {
        return Err(format!("Name {nesting_name_id} not found in graph").into());
    };
    let NameRef::Resolved(name_ref) = name_ref else {
        return Err(format!("Expected name {nesting_name_id} to be resolved").into());
    };
    // When no explicit self is given, self is the innermost lexical scope (the nesting's own declaration).
    // When explicit, follow constant aliases so callers can pass whatever the expression that set self
    // resolves to without having to unwrap aliases themselves. Missing or non-namespace decls are graph
    // inconsistencies and surfaced as errors.
    let resolved_self_decl_id = match self_decl_id {
        Some(id) => {
            resolve_to_namespace(graph, id)?.ok_or_else(|| format!("self declaration {id:?} not found in graph"))?
        }
        None => *name_ref.declaration_id(),
    };
    let self_decl = graph
        .declarations()
        .get(&resolved_self_decl_id)
        .unwrap()
        .as_namespace()
        .ok_or("Expected associated declaration to be a namespace")?;
    let innermost_lexical_decl = graph
        .declarations()
        .get(name_ref.declaration_id())
        .unwrap()
        .as_namespace()
        .unwrap();

    let mut candidates = Vec::new();

    // Collect constants. Immediate scope includes inheritance. Outer scopes only include `Object` inheritance when it's a module
    collect_constants_from_lexical_scope(graph, innermost_lexical_decl, &mut context, &mut candidates);
    collect_constants_from_outer_nesting(graph, name_ref, &mut context, &mut candidates);

    // Collect class variables, which are based on the inheritance chain of the attached object of the immediate lexical scope
    collect_class_variables_from_lexical_scope(graph, name_ref, &mut context, &mut candidates);

    // Globals are accessible from anywhere, regardless of lexical scope or `self` type.
    let object = graph.declarations().get(&OBJECT_ID).unwrap().as_namespace().unwrap();
    collect_candidates!(graph, &object, context, candidates, Declaration::GlobalVariable(_));

    // Collect methods and instance variables, which are based on the inheritance chain of the `self` type (which may
    // not match the immediate lexical scope)
    collect_methods_and_ivars_from_self(graph, self_decl, &mut context, &mut candidates);

    // Keywords are always available in expression contexts
    candidates.extend(keywords::KEYWORDS.iter().map(CompletionCandidate::Keyword));
    Ok(candidates)
}

/// Collects constants reachable from the innermost lexical scope's ancestor chain. Module bodies also fall back to
/// `Object`'s ancestor chain to mirror Ruby's resolution rules.
fn collect_constants_from_lexical_scope<'a>(
    graph: &'a Graph,
    innermost_lexical_decl: &'a Namespace,
    context: &mut CompletionContext<'a>,
    candidates: &mut Vec<CompletionCandidate>,
) {
    for ancestor in innermost_lexical_decl.ancestors() {
        if let Ancestor::Complete(ancestor_id) = ancestor {
            let ancestor_decl = graph.declarations().get(ancestor_id).unwrap().as_namespace().unwrap();

            collect_candidates!(
                graph,
                &ancestor_decl,
                context,
                candidates,
                Declaration::Namespace(_) | Declaration::Constant(_) | Declaration::ConstantAlias(_)
            );
        }
    }

    if matches!(innermost_lexical_decl, Namespace::Module(_)) {
        let object = graph.declarations().get(&OBJECT_ID).unwrap().as_namespace().unwrap();

        for ancestor in object.ancestors() {
            if let Ancestor::Complete(ancestor_id) = ancestor {
                let ancestor_decl = graph.declarations().get(ancestor_id).unwrap().as_namespace().unwrap();

                collect_candidates!(
                    graph,
                    &ancestor_decl,
                    context,
                    candidates,
                    Declaration::Namespace(_) | Declaration::Constant(_) | Declaration::ConstantAlias(_)
                );
            }
        }
    }
}

/// Collects class variables visible from the current lexical scope. Class variables are resolved
/// lexically and singleton classes are skipped: inside `class Bar; class << Foo; @@cvar` the
/// cvar belongs to `Bar`, not `Foo`. We walk the lexical chain (innermost outward) until we find
/// a non-singleton namespace, then walk that namespace's ancestors.
fn collect_class_variables_from_lexical_scope<'a>(
    graph: &'a Graph,
    name_ref: &crate::model::name::ResolvedName,
    context: &mut CompletionContext<'a>,
    candidates: &mut Vec<CompletionCandidate>,
) {
    let mut decl = graph
        .declarations()
        .get(name_ref.declaration_id())
        .unwrap()
        .as_namespace()
        .unwrap();
    let mut current_name_id = *name_ref.nesting();

    while matches!(decl, Namespace::SingletonClass(_)) {
        let Some(parent_name_id) = current_name_id else {
            // No non-singleton lexical scope (invalid Ruby). Skip cvar collection.
            return;
        };
        let NameRef::Resolved(parent_ref) = graph.names().get(&parent_name_id).unwrap() else {
            return;
        };
        decl = graph
            .declarations()
            .get(parent_ref.declaration_id())
            .unwrap()
            .as_namespace()
            .unwrap();
        current_name_id = *parent_ref.nesting();
    }

    for ancestor in decl.ancestors() {
        if let Ancestor::Complete(ancestor_id) = ancestor {
            let ancestor_decl = graph.declarations().get(ancestor_id).unwrap().as_namespace().unwrap();
            collect_candidates!(
                graph,
                &ancestor_decl,
                context,
                candidates,
                Declaration::ClassVariable(_)
            );
        }
    }
}

/// Walks the outer lexical nesting chain (excluding the innermost scope) to collect constants reachable through
/// enclosing classes/modules.
fn collect_constants_from_outer_nesting<'a>(
    graph: &'a Graph,
    name_ref: &crate::model::name::ResolvedName,
    context: &mut CompletionContext<'a>,
    candidates: &mut Vec<CompletionCandidate>,
) {
    let mut current_name_id = *name_ref.nesting();

    while let Some(id) = current_name_id {
        let NameRef::Resolved(parent_ref) = graph.names().get(&id).unwrap() else {
            break;
        };

        let nesting_decl = graph
            .declarations()
            .get(parent_ref.declaration_id())
            .unwrap()
            .as_namespace()
            .unwrap();

        collect_candidates!(
            graph,
            &nesting_decl,
            context,
            candidates,
            Declaration::Namespace(_) | Declaration::Constant(_) | Declaration::ConstantAlias(_)
        );

        current_name_id = *parent_ref.nesting();
    }
}

/// Collects methods and instance variables along `self`'s ancestor chain. The chain may differ
/// from the lexical chain when `self` was rebound (e.g., `def Foo.baz` written inside `class Bar`).
fn collect_methods_and_ivars_from_self<'a>(
    graph: &'a Graph,
    self_decl: &'a Namespace,
    context: &mut CompletionContext<'a>,
    candidates: &mut Vec<CompletionCandidate>,
) {
    for ancestor in self_decl.ancestors() {
        if let Ancestor::Complete(ancestor_id) = ancestor {
            let ancestor_decl = graph.declarations().get(ancestor_id).unwrap().as_namespace().unwrap();

            collect_candidates!(
                graph,
                &ancestor_decl,
                context,
                candidates,
                Declaration::Method(_) | Declaration::InstanceVariable(_)
            );
        }
    }
}

/// Collect completion for a method argument (e.g.: `foo.bar(|)`)
fn method_argument_completion<'a>(
    graph: &'a Graph,
    self_decl_id: Option<DeclarationId>,
    nesting_name_id: NameId,
    method_decl_id: DeclarationId,
    context: CompletionContext<'a>,
) -> Result<Vec<CompletionCandidate>, Box<dyn Error>> {
    let mut candidates = expression_completion(graph, self_decl_id, nesting_name_id, context)?;
    let Some(method_decl) = graph.declarations().get(&method_decl_id) else {
        return Ok(candidates);
    };

    // Find the first Method definition to extract keyword parameters
    for def_id in method_decl.definitions() {
        if let Some(Definition::Method(method_def)) = graph.definitions().get(def_id) {
            for signature in method_def.signatures().as_slice() {
                for param in signature {
                    match param {
                        Parameter::RequiredKeyword(p) | Parameter::OptionalKeyword(p) => {
                            candidates.push(CompletionCandidate::KeywordArgument(*p.str()));
                        }
                        _ => {}
                    }
                }
            }
            break;
        }
    }

    Ok(candidates)
}

/// Finds the given member on the ancestor chain of the declaration. Use `only_inherited` to skip all ancestors until
/// the main namespace and start from its parent.
#[must_use]
pub fn find_member_in_ancestors(
    graph: &Graph,
    declaration_id: DeclarationId,
    member_str_id: StringId,
    only_inherited: bool,
) -> Option<DeclarationId> {
    let namespace = graph.declarations().get(&declaration_id)?.as_namespace()?;
    let mut found_main_namespace = false;

    for ancestor in namespace.ancestors() {
        let Ancestor::Complete(ancestor_id) = ancestor else {
            continue;
        };

        if only_inherited && !found_main_namespace {
            if *ancestor_id == declaration_id {
                found_main_namespace = true;
            }
            continue;
        }

        if let Some(member_id) = graph
            .declarations()
            .get(ancestor_id)?
            .as_namespace()?
            .member(&member_str_id)
        {
            return Some(*member_id);
        }
    }

    None
}

/// Reasons [`follow_method_alias`] could not produce a target declaration.
#[derive(Debug, PartialEq, Eq)]
pub enum AliasResolutionError {
    /// The provided definition id is not a `MethodAlias`.
    NotAnAlias,
    /// The alias's owner could not be resolved (e.g., a `ConstantReceiver` whose name never resolved to a declaration,
    /// or a singleton-class chain whose attached object isn't resolvable).
    UnresolvedOwner,
    /// The chain of aliases forms a cycle. The chain was abandoned at the first revisit.
    Cycle,
    /// The alias's `old_name` does not exist on the owner or any of its ancestors.
    TargetNotFound,
    /// The resolved target is not a method declaration. Indicates a graph inconsistency since method-name lookups
    /// should only land on `Declaration::Method`.
    TargetNotMethod,
}

/// Follows `alias_id` through any chain of further `MethodAlias` definitions and returns the `DeclarationId` of the
/// final method declaration that has at least one non-alias definition (a regular `def`, `attr_*`, etc.).
///
/// # Errors
///
/// Returns an `AliasResolutionError` describing why the chain could not be resolved (not an alias, unresolved owner,
/// cyclic chain, target missing, or target not a method).
///
/// # Panics
///
/// Panics if the graph is internally inconsistent
pub fn follow_method_alias(graph: &Graph, alias_id: DefinitionId) -> Result<DeclarationId, AliasResolutionError> {
    let mut seen: IdentityHashSet<DeclarationId> = IdentityHashSet::default();
    let mut current = alias_id;

    loop {
        let Some(Definition::MethodAlias(alias)) = graph.definitions().get(&current) else {
            return Err(AliasResolutionError::NotAnAlias);
        };

        let owner_id = graph
            .definition_id_to_declaration_id(current)
            .and_then(|decl_id| graph.declarations().get(decl_id))
            .map(|decl| *decl.owner_id())
            .ok_or(AliasResolutionError::UnresolvedOwner)?;

        let target_id = find_member_in_ancestors(graph, owner_id, *alias.old_name_str_id(), false)
            .ok_or(AliasResolutionError::TargetNotFound)?;

        if !seen.insert(target_id) {
            return Err(AliasResolutionError::Cycle);
        }

        let Declaration::Method(target) = graph
            .declarations()
            .get(&target_id)
            .expect("member returned by find_member_in_ancestors must exist")
        else {
            return Err(AliasResolutionError::TargetNotMethod);
        };

        // Stop at the first non-alias definition; otherwise track the smallest alias `DefinitionId` so the trace stays
        // deterministic across runs. (If two aliases target different methods, we just pick one of them.)
        let mut maybe_next_alias: Option<DefinitionId> = None;

        for &def_id in target.definitions() {
            if !matches!(
                graph
                    .definitions()
                    .get(&def_id)
                    .expect("declaration definition_id must exist in the graph"),
                Definition::MethodAlias(_),
            ) {
                return Ok(target_id);
            }

            maybe_next_alias = Some(maybe_next_alias.map_or(def_id, |m| m.min(def_id)));
        }

        current = maybe_next_alias.ok_or(AliasResolutionError::TargetNotFound)?;
    }
}

#[cfg(test)]
mod tests {
    use std::str::FromStr;
    use url::Url;

    use super::*;
    use crate::{
        model::{
            ids::StringId,
            name::{Name, ParentScope},
        },
        test_utils::GraphTest,
    };

    macro_rules! assert_results_eq {
        ($context:expr, $query:expr, $expected:expr) => {
            assert_results_eq!($context, $query, &MatchMode::default(), $expected);
        };
        ($context:expr, $query:expr, $match_mode:expr, $expected:expr) => {
            let actual = declaration_search(&$context.graph(), $query, $match_mode);
            assert_eq!(
                actual,
                $expected
                    .into_iter()
                    .map(|s| DeclarationId::from(s))
                    .collect::<Vec<DeclarationId>>(),
                "Unexpected search results: {:?}",
                actual
                    .iter()
                    .map(|id| $context
                        .graph()
                        .declarations()
                        .get(id)
                        .unwrap()
                        .name()
                        .to_string())
                    .collect::<Vec<String>>()
            );
        };
    }

    fn candidate_label(context: &GraphTest, candidate: &CompletionCandidate) -> String {
        match candidate {
            CompletionCandidate::Declaration(id) => context.graph().declarations().get(id).unwrap().name().to_string(),
            CompletionCandidate::KeywordArgument(str_id) => {
                format!("{}:", context.graph().strings().get(str_id).unwrap().as_str())
            }
            CompletionCandidate::Keyword(kw) => kw.name().to_string(),
        }
    }

    macro_rules! assert_completion_eq {
        ($context:expr, $receiver:expr, $expected:expr) => {
            let mut actual: Vec<String> = completion_candidates($context.graph(), CompletionContext::new($receiver))
                .unwrap()
                .iter()
                .map(|candidate| candidate_label(&$context, candidate))
                .collect();
            actual.sort();

            let mut expected: Vec<String> = $expected.into_iter().map(String::from).collect();
            expected.sort();

            assert_eq!(expected, actual);
        };
    }

    /// Asserts declaration and keyword argument completion candidates, excluding language keywords.
    /// Language keywords are always present in expression contexts and tested separately.
    /// Both sides are sorted before comparison so tests are not coupled to candidate emission order.
    macro_rules! assert_declaration_completion_eq {
        ($context:expr, $receiver:expr, $expected:expr) => {
            let mut actual: Vec<String> = completion_candidates($context.graph(), CompletionContext::new($receiver))
                .unwrap()
                .iter()
                .filter(|c| !matches!(c, CompletionCandidate::Keyword(_)))
                .map(|candidate| candidate_label(&$context, candidate))
                .collect();
            actual.sort();

            let mut expected: Vec<String> = $expected.into_iter().map(String::from).collect();
            expected.sort();

            assert_eq!(expected, actual);
        };
    }

    #[test]
    fn fuzzy_search_returns_partial_matches() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            class Foo
            end
            "
        });
        context.resolve();
        assert_results_eq!(context, "Fo", ["Foo"]);
    }

    #[test]
    fn exact_partial_match_search() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            class Foo
              def is_a_foo?; end
            end

            class Bar < Foo
              def is_a?(other); end
            end
            "
        });
        context.resolve();
        assert_results_eq!(context, "#is_a?()", &MatchMode::Exact, ["Bar#is_a?()"]);
    }

    #[test]
    fn exact_match_empty_query_returns_all() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            class Foo; end
            class Bar; end
            "
        });
        context.resolve();
        let exact_results = declaration_search(context.graph(), "", &MatchMode::Exact);
        let fuzzy_results = declaration_search(context.graph(), "", &MatchMode::Fuzzy);

        assert_eq!(exact_results.len(), fuzzy_results.len());
        assert_eq!(context.graph().declarations().len(), exact_results.len());
    }

    #[test]
    fn exact_match_is_case_sensitive() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            class Foo
              def is_a_foo?; end
            end

            class Bar < Foo
              def is_a?(other); end
            end
            "
        });
        context.resolve();

        assert_results_eq!(context, "#Is_A?()", &MatchMode::Exact, Vec::<&str>::new());
        assert_results_eq!(context, "#Is_A?()", ["Foo#is_a_foo?()", "Bar#is_a?()"]);
    }

    fn test_root() -> PathBuf {
        let root = if cfg!(windows) { "C:\\" } else { "/" };
        PathBuf::from_str(root).unwrap()
    }

    #[test]
    fn test_resolve_require_path() {
        let root = test_root();
        let path = root
            .join("lib")
            .join("foo")
            .join("bar.rb")
            .to_str()
            .unwrap()
            .to_string();
        let uri = Url::from_file_path(path).unwrap().to_string();
        let load_paths = [root.join("lib")];

        let mut context = GraphTest::new();
        context.index_uri(&uri, "class Bar; end");

        // finds basic path
        let uri_id = resolve_require_path(context.graph(), "foo/bar", &load_paths);
        assert!(uri_id.is_some());
        let doc = context.graph().documents().get(&uri_id.unwrap()).unwrap();
        assert_eq!(uri, doc.uri());

        // handles .rb suffix
        let uri_id_with_rb = resolve_require_path(context.graph(), "foo/bar.rb", &load_paths);
        assert_eq!(uri_id, uri_id_with_rb);

        // returns None for nonexistent
        assert!(resolve_require_path(context.graph(), "nonexistent", &load_paths).is_none());
    }

    #[test]
    fn test_resolve_require_path_prefers_earliest_load_path() {
        let root = test_root();
        let lib_path = root.join("lib").join("foo").join("bar.rb");
        let test_path = root.join("test").join("foo").join("bar.rb");
        let lib_uri = Url::from_file_path(&lib_path).unwrap().to_string();
        let test_uri = Url::from_file_path(&test_path).unwrap().to_string();

        let mut context = GraphTest::new();
        context.index_uri(&lib_uri, "class Bar; end");
        context.index_uri(&test_uri, "class Bar; end");

        // lib comes first in load paths
        let load_paths = [root.join("lib"), root.join("test")];
        let uri_id = resolve_require_path(context.graph(), "foo/bar", &load_paths).unwrap();
        let doc = context.graph().documents().get(&uri_id).unwrap();
        assert!(
            doc.uri().contains("lib/foo/bar.rb"),
            "Expected lib path, got {}",
            doc.uri()
        );

        // test comes first in load paths
        let load_paths = [root.join("test"), root.join("lib")];
        let uri_id = resolve_require_path(context.graph(), "foo/bar", &load_paths).unwrap();
        let doc = context.graph().documents().get(&uri_id).unwrap();
        assert!(
            doc.uri().contains("test/foo/bar.rb"),
            "Expected test path, got {}",
            doc.uri()
        );
    }

    #[test]
    fn test_require_paths() {
        let root = test_root();
        let path_bar = root.join("lib").join("foo").join("bar.rb");
        let path_qux = root.join("lib").join("foo").join("qux.rb");
        let path_foobar = root.join("lib").join("foobar.rb");
        let uri_bar = Url::from_file_path(&path_bar).unwrap().to_string();
        let uri_qux = Url::from_file_path(&path_qux).unwrap().to_string();
        let uri_foobar = Url::from_file_path(&path_foobar).unwrap().to_string();
        let load_paths = vec![root.join("lib")];

        let mut context = GraphTest::new();
        context.index_uri(&uri_bar, "class Bar; end");
        context.index_uri(&uri_qux, "class Qux; end");
        context.index_uri(&uri_foobar, "class Foobar; end");

        let results = require_paths(context.graph(), &load_paths);

        assert_eq!(3, results.len());
        assert!(results.contains(&"foo/bar".to_string()));
        assert!(results.contains(&"foo/qux".to_string()));
        assert!(results.contains(&"foobar".to_string()));
    }

    #[test]
    fn test_require_paths_deduplicates_by_load_path_order() {
        let root = test_root();
        let path1 = root.join("lib1").join("foo.rb");
        let path2 = root.join("lib2").join("foo.rb");
        let uri1 = Url::from_file_path(&path1).unwrap().to_string();
        let uri2 = Url::from_file_path(&path2).unwrap().to_string();
        let load_paths = [root.join("lib1"), root.join("lib2")];

        let mut context = GraphTest::new();
        context.index_uri(&uri1, "class Foo; end");
        context.index_uri(&uri2, "class Foo; end");

        let results = require_paths(context.graph(), &load_paths);

        let foo_count = results.iter().filter(|p| *p == "foo").count();
        assert_eq!(1, foo_count);
    }

    #[test]
    fn completion_candidates_on_self() {
        let mut context = GraphTest::new();

        context.index_uri(
            "file:///foo.rb",
            "
            module Foo
              CONST = 1
              def bar; end
            end

            class Parent
              def initialize
                @var = 1
              end
            end

            class Child < Parent
              include Foo

              def baz
                # Completion in this `self` context
              end
            end
            ",
        );
        context.resolve();

        let name_id = Name::new(StringId::from("Child"), ParentScope::None, None).id();
        assert_declaration_completion_eq!(
            context,
            CompletionReceiver::Expression {
                self_decl_id: None,
                nesting_name_id: name_id,
            },
            [
                "Foo::CONST",
                "Class",
                "BasicObject",
                "Child",
                "Parent",
                "Kernel",
                "Module",
                "Foo",
                "Object",
                "Child#baz()",
                "Foo#bar()",
                "Parent#initialize()",
                "Parent#@var"
            ]
        );
    }

    #[test]
    fn completion_candidates_shows_first_option_in_the_ancestor_chain() {
        let mut context = GraphTest::new();

        context.index_uri(
            "file:///foo.rb",
            "
            module Foo
              def bar; end
            end

            class Parent
              def bar; end
            end

            class Child < Parent
              def bar
                # Completion in this `self` context
              end
            end
            ",
        );
        context.resolve();

        let name_id = Name::new(StringId::from("Child"), ParentScope::None, None).id();
        assert_declaration_completion_eq!(
            context,
            CompletionReceiver::Expression {
                self_decl_id: None,
                nesting_name_id: name_id,
            },
            [
                "Class",
                "BasicObject",
                "Child",
                "Parent",
                "Kernel",
                "Module",
                "Foo",
                "Object",
                "Child#bar()"
            ]
        );
    }

    #[test]
    fn completion_candidates_in_a_cyclic_ancestor_chain() {
        let mut context = GraphTest::new();

        context.index_uri(
            "file:///foo.rb",
            "
            module Foo
              include Baz

              def foo_m; end
            end

            module Bar
              include Foo

              def bar_m; end
            end

            module Baz
              include Bar

              def baz_m; end
            end
            ",
        );
        context.resolve();

        let name_id = Name::new(StringId::from("Foo"), ParentScope::None, None).id();
        assert_declaration_completion_eq!(
            context,
            CompletionReceiver::Expression {
                self_decl_id: None,
                nesting_name_id: name_id,
            },
            [
                "Foo",
                "Class",
                "BasicObject",
                "Object",
                "Kernel",
                "Module",
                "Baz",
                "Bar",
                "Foo#foo_m()",
                "Baz#baz_m()",
                "Bar#bar_m()"
            ]
        );
    }

    #[test]
    fn completion_candidates_for_class_variables() {
        let mut context = GraphTest::new();

        context.index_uri(
            "file:///foo.rb",
            "
            class Foo
              @@foo_var = 1

              class << self
                def do_something
                  # Completion in this `self` context
                end
              end
            end

            class Bar < Foo
              def baz
                # Other completion in this `self` context
              end
            end
            ",
        );
        context.resolve();

        let foo_id = Name::new(StringId::from("Foo"), ParentScope::None, None).id();
        let name_id = Name::new(StringId::from("<Foo>"), ParentScope::Attached(foo_id), Some(foo_id)).id();
        assert_declaration_completion_eq!(
            context,
            CompletionReceiver::Expression {
                self_decl_id: None,
                nesting_name_id: name_id,
            },
            [
                "Module",
                "Class",
                "Object",
                "BasicObject",
                "Kernel",
                "Foo",
                "Bar",
                "Foo::<Foo>#do_something()",
                "Foo#@@foo_var"
            ]
        );

        let name_id = Name::new(StringId::from("Bar"), ParentScope::None, None).id();
        assert_declaration_completion_eq!(
            context,
            CompletionReceiver::Expression {
                self_decl_id: None,
                nesting_name_id: name_id,
            },
            [
                "Module",
                "Class",
                "Object",
                "BasicObject",
                "Kernel",
                "Foo",
                "Bar",
                "Bar#baz()",
                "Foo#@@foo_var"
            ]
        );
    }

    #[test]
    fn completion_candidates_includes_constants_accessible_within_lexical_scope() {
        let mut context = GraphTest::new();

        context.index_uri(
            "file:///foo.rb",
            "
            module Foo
              CONST_A = 1

              class ::Bar
                def bar_m
                  # Completion in this `self` context
                end
              end
            end

            class Bar
              def bar_m2
                # Completion in this `self` context
              end
            end
            ",
        );
        context.resolve();

        let name_id = Name::new(
            StringId::from("Bar"),
            ParentScope::TopLevel,
            Some(Name::new(StringId::from("Foo"), ParentScope::None, None).id()),
        )
        .id();
        assert_declaration_completion_eq!(
            context,
            CompletionReceiver::Expression {
                self_decl_id: None,
                nesting_name_id: name_id,
            },
            [
                "Module",
                "Class",
                "Object",
                "BasicObject",
                "Kernel",
                "Foo",
                "Bar",
                "Foo::CONST_A",
                "Bar#bar_m()",
                "Bar#bar_m2()"
            ]
        );

        let name_id = Name::new(StringId::from("Bar"), ParentScope::None, None).id();
        assert_declaration_completion_eq!(
            context,
            CompletionReceiver::Expression {
                self_decl_id: None,
                nesting_name_id: name_id,
            },
            [
                "Module",
                "Class",
                "Object",
                "BasicObject",
                "Kernel",
                "Foo",
                "Bar",
                "Bar#bar_m()",
                "Bar#bar_m2()"
            ]
        );
    }

    #[test]
    fn completion_candidates_finds_unqualified_constant_reachable_from_namespace() {
        let mut context = GraphTest::new();

        context.index_uri(
            "file:///foo.rb",
            "
            module Foo
              CONST = 1

              class Bar
                def baz
                  # Typing CONST here should find Foo::CONST
                end
              end
            end
            ",
        );
        context.resolve();

        let foo_id = Name::new(StringId::from("Foo"), ParentScope::None, None).id();
        let name_id = Name::new(StringId::from("Bar"), ParentScope::None, Some(foo_id)).id();
        // Foo::CONST is reachable from Foo::Bar through lexical scoping, so it must appear as a completion candidate
        // when the user types the unqualified name CONST
        assert_declaration_completion_eq!(
            context,
            CompletionReceiver::Expression {
                self_decl_id: None,
                nesting_name_id: name_id,
            },
            [
                "Class",
                "Object",
                "BasicObject",
                "Kernel",
                "Foo",
                "Module",
                "Foo::CONST",
                "Foo::Bar",
                "Foo::Bar#baz()"
            ]
        );
    }

    #[test]
    fn completion_candidates_includes_globals() {
        let mut context = GraphTest::new();

        context.index_uri(
            "file:///foo.rb",
            "
            $var = 1
            module Foo
              $var2 = 2

              class Bar < BasicObject
                def bar_m
                  # Completion in this `self` context
                end
              end
            end
            ",
        );
        context.resolve();

        let name_id = Name::new(
            StringId::from("Bar"),
            ParentScope::None,
            Some(Name::new(StringId::from("Foo"), ParentScope::None, None).id()),
        )
        .id();
        assert_declaration_completion_eq!(
            context,
            CompletionReceiver::Expression {
                self_decl_id: None,
                nesting_name_id: name_id,
            },
            ["Foo::Bar", "$var2", "$var", "Foo::Bar#bar_m()"]
        );
    }

    #[test]
    fn namespace_access_completion_collects_constants_and_singleton_methods() {
        let mut context = GraphTest::new();

        context.index_uri(
            "file:///foo.rb",
            "
            module Foo
              CONST = 1
              class Bar; end

              class << self
                def class_method; end
              end

              def instance_method; end
            end
            ",
        );
        context.resolve();

        assert_completion_eq!(
            context,
            CompletionReceiver::NamespaceAccess(DeclarationId::from("Foo")),
            ["Foo::CONST", "Foo::Bar", "Foo::<Foo>#class_method()"]
        );
    }

    #[test]
    fn namespace_access_completion_includes_inherited_members() {
        let mut context = GraphTest::new();

        context.index_uri(
            "file:///foo.rb",
            "
            class Parent
              PARENT_CONST = 1

              class << self
                def parent_class_method; end
              end
            end

            class Child < Parent
              CHILD_CONST = 2

              class << self
                def child_class_method; end
              end
            end
            ",
        );
        context.resolve();

        assert_completion_eq!(
            context,
            CompletionReceiver::NamespaceAccess(DeclarationId::from("Child")),
            [
                "Child::CHILD_CONST",
                "Parent::PARENT_CONST",
                "Child::<Child>#child_class_method()",
                "Parent::<Parent>#parent_class_method()",
            ]
        );
    }

    #[test]
    fn namespace_access_completion_deduplicates_overridden_members() {
        let mut context = GraphTest::new();

        context.index_uri(
            "file:///foo.rb",
            "
            class Parent
              CONST = 1

              class << self
                def shared_method; end
              end
            end

            class Child < Parent
              CONST = 2

              class << self
                def shared_method; end
              end
            end
            ",
        );
        context.resolve();

        assert_completion_eq!(
            context,
            CompletionReceiver::NamespaceAccess(DeclarationId::from("Child")),
            ["Child::CONST", "Child::<Child>#shared_method()"]
        );
    }

    #[test]
    fn namespace_access_completion_excludes_object_owned_constants() {
        let mut context = GraphTest::new();

        context.index_uri(
            "file:///foo.rb",
            "
            class Foo
              CONST = 1
            end

            class Bar; end
            ",
        );
        context.resolve();

        assert_completion_eq!(
            context,
            CompletionReceiver::NamespaceAccess(DeclarationId::from("Foo")),
            ["Foo::CONST"]
        );
    }

    #[test]
    fn namespace_access_completion_includes_constant_aliases() {
        let mut context = GraphTest::new();

        context.index_uri(
            "file:///foo.rb",
            "
            module Foo
              Bar = String
              CONST = 1
            end
            ",
        );
        context.resolve();

        assert_completion_eq!(
            context,
            CompletionReceiver::NamespaceAccess(DeclarationId::from("Foo")),
            ["Foo::CONST", "Foo::Bar"]
        );
    }

    #[test]
    fn namespace_access_completion_follows_constant_alias() {
        let mut context = GraphTest::new();

        context.index_uri(
            "file:///foo.rb",
            "
            class Original
              CONST = 1
              class Nested; end

              class << self
                def class_method; end
              end
            end

            module Foo
              MyOriginal = Original
            end
            ",
        );
        context.resolve();

        assert_completion_eq!(
            context,
            CompletionReceiver::NamespaceAccess(DeclarationId::from("Foo::MyOriginal")),
            [
                "Original::CONST",
                "Original::Nested",
                "Original::<Original>#class_method()"
            ]
        );
    }

    #[test]
    fn namespace_access_completion_follows_chained_constant_alias() {
        let mut context = GraphTest::new();

        context.index_uri(
            "file:///foo.rb",
            "
            class Original
              CONST = 1

              class << self
                def class_method; end
              end
            end

            Alias1 = Original
            Alias2 = Alias1
            ",
        );
        context.resolve();

        assert_completion_eq!(
            context,
            CompletionReceiver::NamespaceAccess(DeclarationId::from("Alias2")),
            ["Original::CONST", "Original::<Original>#class_method()"]
        );
    }

    #[test]
    fn namespace_access_completion_on_basic_object_subclass() {
        let mut context = GraphTest::new();

        context.index_uri(
            "file:///foo.rb",
            "
            class Foo < BasicObject
              CONST = 1

              class << self
                def class_method; end
              end
            end

            class Bar; end
            ",
        );
        context.resolve();

        assert_completion_eq!(
            context,
            CompletionReceiver::NamespaceAccess(DeclarationId::from("Foo")),
            ["Foo::CONST", "Foo::<Foo>#class_method()"]
        );
    }

    #[test]
    fn namespace_access_completion_includes_module_members() {
        let mut context = GraphTest::new();

        context.index_uri(
            "file:///foo.rb",
            "
            module Bar
              CONST = 1

              class << self
                def bar_class_method; end
              end
            end

            class Foo
              FOO_CONST = 2
              include Bar

              class << self
                def foo_class_method; end
              end
            end
            ",
        );
        context.resolve();

        assert_completion_eq!(
            context,
            CompletionReceiver::NamespaceAccess(DeclarationId::from("Foo")),
            ["Foo::FOO_CONST", "Bar::CONST", "Foo::<Foo>#foo_class_method()"]
        );
    }

    #[test]
    fn method_call_completion_collects_instance_methods() {
        let mut context = GraphTest::new();

        context.index_uri(
            "file:///foo.rb",
            "
            class Foo
              CONST = 1

              def bar; end
              def baz; end

              class << self
                def class_method; end
              end
            end
            ",
        );
        context.resolve();

        assert_completion_eq!(
            context,
            CompletionReceiver::MethodCall(DeclarationId::from("Foo")),
            ["Foo#baz()", "Foo#bar()"]
        );
    }

    #[test]
    fn method_call_completion_follows_constant_alias() {
        let mut context = GraphTest::new();

        context.index_uri(
            "file:///foo.rb",
            "
            class Original
              def bar; end
              def baz; end

              class << self
                def class_method; end
              end
            end

            module Foo
              MyOriginal = Original
            end
            ",
        );
        context.resolve();

        assert_completion_eq!(
            context,
            CompletionReceiver::MethodCall(DeclarationId::from("Foo::MyOriginal")),
            ["Original#baz()", "Original#bar()"]
        );
    }

    #[test]
    fn method_call_completion_includes_inherited_methods() {
        let mut context = GraphTest::new();

        context.index_uri(
            "file:///foo.rb",
            "
            class Parent
              def parent_method; end
            end

            class Child < Parent
              def child_method; end
            end
            ",
        );
        context.resolve();

        assert_completion_eq!(
            context,
            CompletionReceiver::MethodCall(DeclarationId::from("Child")),
            ["Child#child_method()", "Parent#parent_method()"]
        );
    }

    #[test]
    fn method_call_completion_includes_methods_from_included_modules() {
        let mut context = GraphTest::new();

        context.index_uri(
            "file:///foo.rb",
            "
            module Mixin
              def mixin_method; end
            end

            class Foo
              include Mixin

              def foo_method; end
            end
            ",
        );
        context.resolve();

        assert_completion_eq!(
            context,
            CompletionReceiver::MethodCall(DeclarationId::from("Foo")),
            ["Foo#foo_method()", "Mixin#mixin_method()"]
        );
    }

    #[test]
    fn method_call_completion_deduplicates_overridden_methods() {
        let mut context = GraphTest::new();

        context.index_uri(
            "file:///foo.rb",
            "
            class Parent
              def shared_method; end
              def parent_only; end
            end

            class Child < Parent
              def shared_method; end
              def child_only; end
            end
            ",
        );
        context.resolve();

        assert_completion_eq!(
            context,
            CompletionReceiver::MethodCall(DeclarationId::from("Child")),
            ["Child#shared_method()", "Child#child_only()", "Parent#parent_only()"]
        );
    }

    #[test]
    fn method_call_completion_excludes_non_method_members() {
        let mut context = GraphTest::new();

        context.index_uri(
            "file:///foo.rb",
            "
            class Foo
              CONST = 1
              @@class_var = 2

              def initialize
                @ivar = 3
              end

              def bar; end
            end
            ",
        );
        context.resolve();

        assert_completion_eq!(
            context,
            CompletionReceiver::MethodCall(DeclarationId::from("Foo")),
            ["Foo#initialize()", "Foo#bar()"]
        );
    }

    #[test]
    fn method_call_completion_at_singleton_level() {
        let mut context = GraphTest::new();

        context.index_uri(
            "file:///foo.rb",
            "
            class Foo
              def self.bar; end

              class << self
                def baz; end
              end
            end
            ",
        );
        context.resolve();

        assert_completion_eq!(
            context,
            CompletionReceiver::MethodCall(DeclarationId::from("Foo::<Foo>")),
            ["Foo::<Foo>#baz()", "Foo::<Foo>#bar()"]
        );
    }

    #[test]
    fn method_argument_completion_includes_keyword_params() {
        let mut context = GraphTest::new();

        context.index_uri(
            "file:///foo.rb",
            "
            class Foo
              def greet(name:, greeting: 'hello'); end
            end
            ",
        );
        context.resolve();

        let name_id = Name::new(StringId::from("Foo"), ParentScope::None, None).id();
        assert_declaration_completion_eq!(
            context,
            CompletionReceiver::MethodArgument {
                self_decl_id: None,
                nesting_name_id: name_id,
                method_decl_id: DeclarationId::from("Foo#greet()"),
            },
            [
                "Class",
                "Object",
                "BasicObject",
                "Kernel",
                "Foo",
                "Module",
                "Foo#greet()",
                "name:",
                "greeting:"
            ]
        );
    }

    #[test]
    fn method_argument_completion_no_keyword_params() {
        let mut context = GraphTest::new();

        context.index_uri(
            "file:///foo.rb",
            "
            class Foo
              def bar(x, y); end
            end
            ",
        );
        context.resolve();

        let name_id = Name::new(StringId::from("Foo"), ParentScope::None, None).id();
        assert_declaration_completion_eq!(
            context,
            CompletionReceiver::MethodArgument {
                self_decl_id: None,
                nesting_name_id: name_id,
                method_decl_id: DeclarationId::from("Foo#bar()"),
            },
            ["Class", "Object", "BasicObject", "Kernel", "Foo", "Module", "Foo#bar()"]
        );
    }

    #[test]
    fn method_argument_completion_mixed_params() {
        let mut context = GraphTest::new();

        context.index_uri(
            "file:///foo.rb",
            "
            class Foo
              def search(query, limit:, offset: 0, **opts); end
            end
            ",
        );
        context.resolve();

        let name_id = Name::new(StringId::from("Foo"), ParentScope::None, None).id();
        assert_declaration_completion_eq!(
            context,
            CompletionReceiver::MethodArgument {
                self_decl_id: None,
                nesting_name_id: name_id,
                method_decl_id: DeclarationId::from("Foo#search()"),
            },
            // Only RequiredKeyword and OptionalKeyword, not RestKeyword (**opts)
            [
                "Class",
                "Object",
                "BasicObject",
                "Kernel",
                "Foo",
                "Module",
                "Foo#search()",
                "limit:",
                "offset:"
            ]
        );
    }

    #[test]
    fn first_entry_is_always_used_overridden_methods() {
        let mut context = GraphTest::new();
        context.index_uri(
            "file:///foo.rb",
            "
            class Foo
              def bar(first:, second:); end
            end
            ",
        );
        context.index_uri(
            "file:///foo2.rb",
            "
            class Foo
              def bar(first:); end
            end
            ",
        );
        context.resolve();

        let name_id = Name::new(StringId::from("Foo"), ParentScope::None, None).id();
        assert_declaration_completion_eq!(
            context,
            CompletionReceiver::MethodArgument {
                self_decl_id: None,
                nesting_name_id: name_id,
                method_decl_id: DeclarationId::from("Foo#bar()"),
            },
            [
                "Class",
                "Object",
                "BasicObject",
                "Kernel",
                "Foo",
                "Module",
                "Foo#bar()",
                "first:",
                "second:"
            ]
        );
    }

    #[test]
    fn expression_completion_includes_keywords() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", "class Foo; end");
        context.resolve();

        let name_id = Name::new(StringId::from("Foo"), ParentScope::None, None).id();
        assert_completion_eq!(
            context,
            CompletionReceiver::Expression {
                self_decl_id: None,
                nesting_name_id: name_id,
            },
            [
                "Class",
                "Object",
                "BasicObject",
                "Kernel",
                "Foo",
                "Module",
                "BEGIN",
                "END",
                "__ENCODING__",
                "__FILE__",
                "__LINE__",
                "alias",
                "and",
                "begin",
                "break",
                "case",
                "class",
                "def",
                "defined?",
                "do",
                "else",
                "elsif",
                "end",
                "ensure",
                "false",
                "for",
                "if",
                "in",
                "module",
                "next",
                "nil",
                "not",
                "or",
                "redo",
                "rescue",
                "retry",
                "return",
                "self",
                "super",
                "then",
                "true",
                "undef",
                "unless",
                "until",
                "when",
                "while",
                "yield",
            ]
        );
    }

    #[test]
    fn method_argument_completion_includes_keywords() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", "class Foo; def bar(name:); end; end");
        context.resolve();

        let name_id = Name::new(StringId::from("Foo"), ParentScope::None, None).id();
        assert_completion_eq!(
            context,
            CompletionReceiver::MethodArgument {
                self_decl_id: None,
                nesting_name_id: name_id,
                method_decl_id: DeclarationId::from("Foo#bar()"),
            },
            [
                "Class",
                "Object",
                "BasicObject",
                "Kernel",
                "Foo",
                "Module",
                "Foo#bar()",
                "BEGIN",
                "END",
                "__ENCODING__",
                "__FILE__",
                "__LINE__",
                "alias",
                "and",
                "begin",
                "break",
                "case",
                "class",
                "def",
                "defined?",
                "do",
                "else",
                "elsif",
                "end",
                "ensure",
                "false",
                "for",
                "if",
                "in",
                "module",
                "next",
                "nil",
                "not",
                "or",
                "redo",
                "rescue",
                "retry",
                "return",
                "self",
                "super",
                "then",
                "true",
                "undef",
                "unless",
                "until",
                "when",
                "while",
                "yield",
                "name:",
            ]
        );
    }

    #[test]
    fn namespace_access_completion_excludes_keywords() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", "class Foo; CONST = 1; end");
        context.resolve();

        let candidates = completion_candidates(
            context.graph(),
            CompletionContext::new(CompletionReceiver::NamespaceAccess(DeclarationId::from("Foo"))),
        )
        .unwrap();

        assert!(!candidates.iter().any(|c| matches!(c, CompletionCandidate::Keyword(_))));
    }

    #[test]
    fn method_call_completion_excludes_keywords() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", "class Foo; def bar; end; end");
        context.resolve();

        let candidates = completion_candidates(
            context.graph(),
            CompletionContext::new(CompletionReceiver::MethodCall(DeclarationId::from("Foo"))),
        )
        .unwrap();

        assert!(!candidates.iter().any(|c| matches!(c, CompletionCandidate::Keyword(_))));
    }

    #[test]
    fn expression_completion_class_variables_follow_lexical_scope() {
        // `@@cvar` in Ruby is resolved via the innermost lexical class/module's ancestor chain,
        // NOT `self`'s ancestor chain. Inside `def Foo.bar` written inside `Outer`, the lexical
        // scope is `[Outer]`, so `@@outer_cvar` is reachable and `@@foo_cvar` (which lives on
        // `Foo`, not on any lexical ancestor) must not be offered.
        let mut context = GraphTest::new();

        context.index_uri(
            "file:///foo.rb",
            "
            module Outer
              @@outer_cvar = 1

              class Foo
                @@foo_cvar = 2
                def self.singleton_m; end
              end
            end
            ",
        );
        context.resolve();

        let outer_name_id = Name::new(StringId::from("Outer"), ParentScope::None, None).id();
        assert_declaration_completion_eq!(
            context,
            CompletionReceiver::Expression {
                self_decl_id: Some(DeclarationId::from("Outer::Foo::<Foo>")),
                nesting_name_id: outer_name_id,
            },
            [
                "Module",
                "Class",
                "Object",
                "BasicObject",
                "Kernel",
                "Outer",
                "Outer::Foo",
                "Outer#@@outer_cvar",
                "Outer::Foo::<Foo>#singleton_m()"
            ]
        );
    }

    #[test]
    fn expression_completion_follows_self_decl_alias() {
        let mut context = GraphTest::new();

        context.index_uri(
            "file:///foo.rb",
            "
            module Outer
              class Original
                def original_m; end
              end

              MyAlias = Original
            end
            ",
        );
        context.resolve();

        let name_id = Name::new(StringId::from("Outer"), ParentScope::None, None).id();
        // `self_decl_id` points to the alias `Outer::MyAlias`, which is a `ConstantAlias` rather than a `Namespace`.
        // The completion should still collect members from the aliased namespace (`Outer::Original`) instead of
        // returning an error, so callers do not have to unwrap aliases themselves.
        assert_declaration_completion_eq!(
            context,
            CompletionReceiver::Expression {
                self_decl_id: Some(DeclarationId::from("Outer::MyAlias")),
                nesting_name_id: name_id,
            },
            [
                "Outer::MyAlias",
                "Outer::Original",
                "Class",
                "Object",
                "BasicObject",
                "Outer",
                "Kernel",
                "Module",
                "Outer::Original#original_m()"
            ]
        );
    }

    #[test]
    fn expression_completion_in_method_definition_with_receiver_uses_lexical_scope_for_class_variables() {
        let mut context = GraphTest::new();

        context.index_uri(
            "file:///foo.rb",
            "
            class Foo
              @@class_var = 1
            end

            class Bar
              @@other_class_var = 2

              def Foo.baz
                # completion here
              end
            end
            ",
        );
        context.resolve();

        let name_id = Name::new(StringId::from("Bar"), ParentScope::None, None).id();
        assert_declaration_completion_eq!(
            context,
            CompletionReceiver::Expression {
                self_decl_id: Some(DeclarationId::from("Foo::<Foo>")),
                nesting_name_id: name_id,
            },
            [
                "Module",
                "Class",
                "Object",
                "BasicObject",
                "Kernel",
                "Foo",
                "Bar",
                "Foo::<Foo>#baz()",
                "Bar#@@other_class_var"
            ]
        );
    }

    #[test]
    fn expression_completion_in_method_definition_with_receiver_uses_lexical_scope_for_constants() {
        let mut context = GraphTest::new();

        context.index_uri(
            "file:///foo.rb",
            "
            class Foo
              CONST = 1
            end

            class Bar
              OTHER_CONST = 2

              def Foo.baz
                # completion here
              end
            end
            ",
        );
        context.resolve();

        let name_id = Name::new(StringId::from("Bar"), ParentScope::None, None).id();
        assert_declaration_completion_eq!(
            context,
            CompletionReceiver::Expression {
                self_decl_id: Some(DeclarationId::from("Foo::<Foo>")),
                nesting_name_id: name_id,
            },
            [
                "Bar::OTHER_CONST",
                "Module",
                "Class",
                "Object",
                "BasicObject",
                "Kernel",
                "Foo",
                "Bar",
                "Foo::<Foo>#baz()"
            ]
        );
    }

    #[test]
    fn expression_completion_does_not_leak_constants_reachable_only_through_self_ancestors() {
        let mut context = GraphTest::new();

        context.index_uri(
            "file:///foo.rb",
            "
            module Mixin
              MIXIN_CONST = 1
            end

            class Foo
              extend Mixin
            end

            class Bar
              def Foo.baz
                # completion here
              end
            end
            ",
        );
        context.resolve();

        let name_id = Name::new(StringId::from("Bar"), ParentScope::None, None).id();
        assert_declaration_completion_eq!(
            context,
            CompletionReceiver::Expression {
                self_decl_id: Some(DeclarationId::from("Foo::<Foo>")),
                nesting_name_id: name_id,
            },
            [
                "Class",
                "BasicObject",
                "Mixin",
                "Object",
                "Kernel",
                "Module",
                "Foo",
                "Bar",
                "Foo::<Foo>#baz()"
            ]
        );
    }

    #[test]
    fn expression_completion_at_top_level_includes_object_constants() {
        let mut context = GraphTest::new();

        context.index_uri(
            "file:///foo.rb",
            "
            class Foo
              CONST = 1
            end

            TOP_CONST = 2
            ",
        );
        context.resolve();

        let name_id = Name::new(StringId::from("Object"), ParentScope::None, None).id();
        assert_declaration_completion_eq!(
            context,
            CompletionReceiver::Expression {
                self_decl_id: None,
                nesting_name_id: name_id,
            },
            ["Module", "Class", "Object", "BasicObject", "Kernel", "Foo", "TOP_CONST"]
        );
    }

    #[test]
    fn expression_completion_in_module_body_falls_back_to_object_constants() {
        let mut context = GraphTest::new();

        context.index_uri(
            "file:///foo.rb",
            "
            TOP_CONST = 1

            module Mod
              MOD_CONST = 2
            end
            ",
        );
        context.resolve();

        let name_id = Name::new(StringId::from("Mod"), ParentScope::None, None).id();
        assert_declaration_completion_eq!(
            context,
            CompletionReceiver::Expression {
                self_decl_id: None,
                nesting_name_id: name_id,
            },
            [
                "Mod::MOD_CONST",
                "Module",
                "Class",
                "Object",
                "BasicObject",
                "Kernel",
                "TOP_CONST",
                "Mod"
            ]
        );
    }

    #[test]
    fn expression_completion_in_module_body_falls_back_to_object_ancestors() {
        let mut context = GraphTest::new();

        context.index_uri(
            "file:///foo.rb",
            "
            module Kernel
              CONST = 1
            end

            module Mod
              # completion here
            end
            ",
        );
        context.resolve();

        let name_id = Name::new(StringId::from("Mod"), ParentScope::None, None).id();
        assert_declaration_completion_eq!(
            context,
            CompletionReceiver::Expression {
                self_decl_id: None,
                nesting_name_id: name_id,
            },
            [
                "Class",
                "Object",
                "Kernel",
                "BasicObject",
                "Mod",
                "Module",
                "Kernel::CONST",
            ]
        );
    }

    #[test]
    fn expression_completion_at_top_level_offers_methods_from_object_chain() {
        let mut context = GraphTest::new();

        context.index_uri(
            "file:///foo.rb",
            "
            def my_top_method; end

            module Kernel
              def kernel_helper; end
            end
            ",
        );
        context.resolve();

        let name_id = Name::new(StringId::from("Object"), ParentScope::None, None).id();
        assert_declaration_completion_eq!(
            context,
            CompletionReceiver::Expression {
                self_decl_id: None,
                nesting_name_id: name_id,
            },
            [
                "Object",
                "Kernel",
                "BasicObject",
                "Module",
                "Class",
                "Object#my_top_method()",
                "Kernel#kernel_helper()"
            ]
        );
    }

    #[test]
    fn expression_completion_in_basic_object_subclass_excludes_object_constants() {
        let mut context = GraphTest::new();

        context.index_uri(
            "file:///foo.rb",
            "
            TOP_CONST = 1

            class Bar < BasicObject
              BAR_CONST = 2
            end
            ",
        );
        context.resolve();

        let name_id = Name::new(StringId::from("Bar"), ParentScope::None, None).id();
        assert_declaration_completion_eq!(
            context,
            CompletionReceiver::Expression {
                self_decl_id: None,
                nesting_name_id: name_id,
            },
            ["Bar::BAR_CONST"]
        );
    }

    #[test]
    fn expression_completion_class_variables_in_singleton_class_block_use_outer_lexical_scope() {
        let mut context = GraphTest::new();

        context.index_uri(
            "file:///foo.rb",
            "
            class Foo
              @@foo_cvar = 1
            end

            class Bar
              @@bar_cvar = 2

              class << Foo
                # completion here
              end
            end
            ",
        );
        context.resolve();

        let bar_id = Name::new(StringId::from("Bar"), ParentScope::None, None).id();
        let foo_ref_id = Name::new(StringId::from("Foo"), ParentScope::None, Some(bar_id)).id();
        let nesting_name_id = Name::new(StringId::from("<Foo>"), ParentScope::Attached(foo_ref_id), Some(bar_id)).id();

        assert_declaration_completion_eq!(
            context,
            CompletionReceiver::Expression {
                self_decl_id: None,
                nesting_name_id,
            },
            ["Bar#@@bar_cvar"]
        );
    }

    #[test]
    fn expression_completion_in_def_self_method_inside_module_body() {
        let mut context = GraphTest::new();

        context.index_uri(
            "file:///foo.rb",
            "
            module Mod
              MOD_CONST = 1

              def self.helper
                # completion here
              end
            end
            ",
        );
        context.resolve();

        let name_id = Name::new(StringId::from("Mod"), ParentScope::None, None).id();
        assert_declaration_completion_eq!(
            context,
            CompletionReceiver::Expression {
                self_decl_id: Some(DeclarationId::from("Mod::<Mod>")),
                nesting_name_id: name_id,
            },
            [
                "Mod::MOD_CONST",
                "Module",
                "Class",
                "Object",
                "BasicObject",
                "Kernel",
                "Mod",
                "Mod::<Mod>#helper()"
            ]
        );
    }

    #[test]
    fn expression_completion_in_singleton_class_block_at_top_level() {
        let mut context = GraphTest::new();

        context.index_uri(
            "file:///foo.rb",
            "
            class Foo
              @@foo_cvar = 1
            end

            class << Foo
              # completion here
            end
            ",
        );
        context.resolve();

        let foo_ref_id = Name::new(StringId::from("Foo"), ParentScope::None, None).id();
        let nesting_name_id = Name::new(StringId::from("<Foo>"), ParentScope::Attached(foo_ref_id), None).id();

        assert_declaration_completion_eq!(
            context,
            CompletionReceiver::Expression {
                self_decl_id: None,
                nesting_name_id,
            },
            ["Module", "Class", "Object", "BasicObject", "Kernel", "Foo"]
        );
    }

    #[test]
    fn expression_completion_errors_when_self_decl_id_does_not_exist() {
        let mut context = GraphTest::new();

        context.index_uri(
            "file:///foo.rb",
            "
            class Foo
            end
            ",
        );
        context.resolve();

        let name_id = Name::new(StringId::from("Foo"), ParentScope::None, None).id();
        let result = completion_candidates(
            context.graph(),
            CompletionContext::new(CompletionReceiver::Expression {
                self_decl_id: Some(DeclarationId::from("Nonexistent")),
                nesting_name_id: name_id,
            }),
        );

        assert!(result.is_err(), "missing self_decl_id should surface as an error");
    }

    #[test]
    fn expression_completion_errors_when_self_decl_id_is_not_a_namespace() {
        let mut context = GraphTest::new();

        context.index_uri(
            "file:///foo.rb",
            "
            class Foo
              CONST = 1
            end
            ",
        );
        context.resolve();

        let name_id = Name::new(StringId::from("Foo"), ParentScope::None, None).id();
        let result = completion_candidates(
            context.graph(),
            CompletionContext::new(CompletionReceiver::Expression {
                // CONST resolves to a `Constant`, not a `Namespace` and not an alias.
                self_decl_id: Some(DeclarationId::from("Foo::CONST")),
                nesting_name_id: name_id,
            }),
        );

        assert!(result.is_err(), "non-namespace self_decl_id should surface as an error");
    }

    #[test]
    fn expression_completion_follows_chained_self_decl_alias() {
        let mut context = GraphTest::new();

        context.index_uri(
            "file:///foo.rb",
            "
            module Outer
              class Original
                def original_m; end
              end

              FirstAlias = Original
              SecondAlias = FirstAlias
            end
            ",
        );
        context.resolve();

        let name_id = Name::new(StringId::from("Outer"), ParentScope::None, None).id();
        assert_declaration_completion_eq!(
            context,
            CompletionReceiver::Expression {
                self_decl_id: Some(DeclarationId::from("Outer::SecondAlias")),
                nesting_name_id: name_id,
            },
            [
                "Outer::FirstAlias",
                "Outer::SecondAlias",
                "Outer::Original",
                "Module",
                "Class",
                "Object",
                "BasicObject",
                "Outer",
                "Kernel",
                "Outer::Original#original_m()"
            ]
        );
    }

    /// Returns the smallest `MethodAlias` `DefinitionId` for the declaration named `alias_decl_fqn`
    /// (e.g., `"Foo#aliased()"`). Picking the smallest mirrors `follow_method_alias`'s own
    /// determinism rule for tests where multiple aliases share a declaration (e.g. cross-file fixtures).
    fn alias_def_id(context: &GraphTest, alias_decl_fqn: &str) -> DefinitionId {
        let decl = context
            .graph()
            .declarations()
            .get(&DeclarationId::from(alias_decl_fqn))
            .unwrap_or_else(|| panic!("expected declaration {alias_decl_fqn}"));

        decl.definitions()
            .iter()
            .copied()
            .filter(|def_id| {
                matches!(
                    context.graph().definitions().get(def_id),
                    Some(Definition::MethodAlias(_)),
                )
            })
            .min()
            .unwrap_or_else(|| panic!("declaration {alias_decl_fqn} has no MethodAlias definition"))
    }

    /// Asserts that the alias declared as `$alias_fqn` follows to the declaration `$target_fqn`.
    macro_rules! assert_alias_target {
        ($context:expr, $alias_fqn:expr, $target_fqn:expr $(,)?) => {{
            let context = $context;
            assert_eq!(
                follow_method_alias(context.graph(), alias_def_id(context, $alias_fqn)),
                Ok(DeclarationId::from($target_fqn)),
            );
        }};
    }

    #[test]
    fn follow_method_alias_to_local_method() {
        let mut context = GraphTest::new();
        context.index_uri(
            "file:///foo.rb",
            r"
            class Foo
              def original; end
              alias aliased original
            end
            ",
        );
        context.resolve();

        assert_alias_target!(&context, "Foo#aliased()", "Foo#original()");
    }

    #[test]
    fn follow_method_alias_through_chain_of_aliases() {
        let mut context = GraphTest::new();
        context.index_uri(
            "file:///foo.rb",
            r"
            class Foo
              def real; end
              alias mid real
              alias outer mid
            end
            ",
        );
        context.resolve();

        assert_alias_target!(&context, "Foo#outer()", "Foo#real()");
    }

    #[test]
    fn follow_method_alias_detects_two_step_cycle() {
        let mut context = GraphTest::new();
        context.index_uri(
            "file:///foo.rb",
            r"
            class Foo
              alias a b
              alias b a
            end
            ",
        );
        context.resolve();

        assert_eq!(
            follow_method_alias(context.graph(), alias_def_id(&context, "Foo#a()")),
            Err(AliasResolutionError::Cycle),
        );
        assert_eq!(
            follow_method_alias(context.graph(), alias_def_id(&context, "Foo#b()")),
            Err(AliasResolutionError::Cycle),
        );
    }

    #[test]
    fn follow_method_alias_detects_multi_step_cycle() {
        let mut context = GraphTest::new();
        context.index_uri(
            "file:///foo.rb",
            r"
            class Foo
              alias a b
              alias b c
              alias c a
            end
            ",
        );
        context.resolve();

        for alias_fqn in ["Foo#a()", "Foo#b()", "Foo#c()"] {
            assert_eq!(
                follow_method_alias(context.graph(), alias_def_id(&context, alias_fqn)),
                Err(AliasResolutionError::Cycle),
                "expected {alias_fqn} to be detected as part of the cycle",
            );
        }
    }

    #[test]
    fn follow_method_alias_detects_self_cycle() {
        let mut context = GraphTest::new();
        context.index_uri(
            "file:///foo.rb",
            r"
            class Foo
              alias foo foo
            end
            ",
        );
        context.resolve();

        assert_eq!(
            follow_method_alias(context.graph(), alias_def_id(&context, "Foo#foo()")),
            Err(AliasResolutionError::Cycle),
        );
    }

    #[test]
    fn follow_method_alias_to_inherited_method() {
        let mut context = GraphTest::new();
        context.index_uri(
            "file:///foo.rb",
            r"
            class Parent
              def inherited_m; end
            end

            class Child < Parent
              alias aliased inherited_m
            end
            ",
        );
        context.resolve();

        assert_alias_target!(&context, "Child#aliased()", "Parent#inherited_m()");
    }

    #[test]
    fn follow_method_alias_with_constant_receiver() {
        let mut context = GraphTest::new();
        context.index_uri(
            "file:///foo.rb",
            r"
            class Bar
              def to_s; end
            end

            class Foo
              Bar.alias_method(:new_to_s, :to_s)
            end
            ",
        );
        context.resolve();

        assert_alias_target!(&context, "Bar#new_to_s()", "Bar#to_s()");
    }

    #[test]
    fn follow_method_alias_in_singleton_class_body() {
        let mut context = GraphTest::new();
        context.index_uri(
            "file:///foo.rb",
            r"
            class Foo
              def self.find; end

              class << self
                alias_method :find_old, :find
              end
            end
            ",
        );
        context.resolve();

        assert_alias_target!(&context, "Foo::<Foo>#find_old()", "Foo::<Foo>#find()");
    }

    #[test]
    fn follow_method_alias_in_singleton_class_body_misses_instance_method() {
        let mut context = GraphTest::new();
        context.index_uri(
            "file:///foo.rb",
            r"
            class Foo
              def regular; end

              class << self
                alias_method :other, :regular
              end
            end
            ",
        );
        context.resolve();

        assert_eq!(
            follow_method_alias(context.graph(), alias_def_id(&context, "Foo::<Foo>#other()")),
            Err(AliasResolutionError::TargetNotFound),
        );
    }

    #[test]
    fn follow_method_alias_to_attr_reader() {
        let mut context = GraphTest::new();
        context.index_uri(
            "file:///foo.rb",
            r"
            class Foo
              attr_reader :name
              alias display name
            end
            ",
        );
        context.resolve();

        assert_alias_target!(&context, "Foo#display()", "Foo#name()");
    }

    #[test]
    fn follow_method_alias_to_attr_accessor() {
        let mut context = GraphTest::new();
        context.index_uri(
            "file:///foo.rb",
            r"
            class Foo
              attr_accessor :age
              alias years age
            end
            ",
        );
        context.resolve();

        assert_alias_target!(&context, "Foo#years()", "Foo#age()");
    }

    #[test]
    fn follow_method_alias_to_method_in_prepended_module() {
        let mut context = GraphTest::new();
        context.index_uri(
            "file:///foo.rb",
            r"
            module M
              def original; end
            end

            class Foo
              prepend M
              alias aliased original
            end
            ",
        );
        context.resolve();

        assert_alias_target!(&context, "Foo#aliased()", "M#original()");
    }

    #[test]
    fn follow_method_alias_ignores_visibility_of_target() {
        let mut context = GraphTest::new();
        context.index_uri(
            "file:///foo.rb",
            r"
            class Foo
              private def secret; end
              alias revealed secret
            end
            ",
        );
        context.resolve();

        assert_alias_target!(&context, "Foo#revealed()", "Foo#secret()");
    }

    #[test]
    fn follow_method_alias_returns_target_not_found_when_target_missing() {
        let mut context = GraphTest::new();
        context.index_uri(
            "file:///foo.rb",
            r"
            class Foo
              alias aliased nonexistent
            end
            ",
        );
        context.resolve();

        assert_eq!(
            follow_method_alias(context.graph(), alias_def_id(&context, "Foo#aliased()")),
            Err(AliasResolutionError::TargetNotFound),
        );
    }
}
