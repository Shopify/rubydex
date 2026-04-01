use std::collections::HashSet;
use std::error::Error;
use std::path::PathBuf;
use std::thread;

use url::Url;

use crate::model::declaration::{Ancestor, Declaration};
use crate::model::definitions::{Definition, MethodAliasDefinition, Parameter, Receiver};
use crate::model::graph::{Graph, OBJECT_ID};
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
    let num_threads = thread::available_parallelism().map(std::num::NonZero::get).unwrap_or(4);
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
    let num_threads = thread::available_parallelism().map(std::num::NonZero::get).unwrap_or(4);
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
    Expression(NameId),
    /// Completion requested after a namespace access operator (e.g.: `Foo::`)
    /// Includes: all constants and singleton methods for the namespace and its ancestors
    NamespaceAccess(DeclarationId),
    /// Completion requested after a method call operator (e.g.: `foo.`, `@bar.`, `@@baz.`, `Qux.`).
    /// In the case of singleton completion (e.g.: `Foo.`), the declaration ID should be for the singleton class (i.e.: `Foo::<Foo>`)
    /// Includes: all methods that exist on the type of the receiver and its ancestors
    MethodCall(DeclarationId),
    /// Completion requested inside a method call's argument list (e.g.: `foo.bar(|)`)
    /// Includes: everything expressions do plus keyword parameter names of the method being called
    MethodArgument {
        self_name_id: NameId,
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
/// Will error if the given `self_name_id` does not point to a namespace declaration
pub fn completion_candidates<'a>(
    graph: &'a Graph,
    context: CompletionContext<'a>,
) -> Result<Vec<CompletionCandidate>, Box<dyn Error>> {
    match context.completion_receiver {
        CompletionReceiver::Expression(self_name_id) => expression_completion(graph, self_name_id, context),
        CompletionReceiver::NamespaceAccess(decl_id) => namespace_access_completion(graph, decl_id, context),
        CompletionReceiver::MethodCall(decl_id) => method_call_completion(graph, decl_id, context),
        CompletionReceiver::MethodArgument {
            self_name_id,
            method_decl_id,
        } => method_argument_completion(graph, self_name_id, method_decl_id, context),
    }
}

/// Resolves a declaration ID to a namespace, following constant aliases if necessary.
fn resolve_to_namespace(graph: &Graph, decl_id: DeclarationId) -> Result<DeclarationId, Box<dyn Error>> {
    if let Some(Declaration::Namespace(_)) = graph.declarations().get(&decl_id) {
        return Ok(decl_id);
    }

    if let Some(target_id) = graph.resolve_alias(&decl_id)
        && let Some(Declaration::Namespace(_)) = graph.declarations().get(&target_id)
    {
        return Ok(target_id);
    }

    Err(format!("Expected declaration {decl_id:?} to be a namespace or alias to a namespace").into())
}

/// Collect completion for a namespace access (e.g.: `Foo::`)
fn namespace_access_completion<'a>(
    graph: &'a Graph,
    namespace_decl_id: DeclarationId,
    mut context: CompletionContext<'a>,
) -> Result<Vec<CompletionCandidate>, Box<dyn Error>> {
    let resolved_id = resolve_to_namespace(graph, namespace_decl_id)?;
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
    let resolved_id = resolve_to_namespace(graph, receiver_decl_id)?;
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
    self_name_id: NameId,
    mut context: CompletionContext<'a>,
) -> Result<Vec<CompletionCandidate>, Box<dyn Error>> {
    let Some(name_ref) = graph.names().get(&self_name_id) else {
        return Err(format!("Name {self_name_id} not found in graph").into());
    };
    let NameRef::Resolved(name_ref) = name_ref else {
        return Err(format!("Expected name {self_name_id} to be resolved").into());
    };
    let Some(self_decl) = graph
        .declarations()
        .get(name_ref.declaration_id())
        .and_then(|d| d.as_namespace())
    else {
        return Err("Expected associated declaration to be a namespace".into());
    };
    let mut candidates = Vec::new();

    // Walk the name's lexical scopes, collecting all constant completion members
    let mut current_name_id = Some(self_name_id);

    while let Some(id) = current_name_id {
        let NameRef::Resolved(name_ref) = graph.names().get(&id).unwrap() else {
            break;
        };

        let nesting_decl = graph
            .declarations()
            .get(name_ref.declaration_id())
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

        current_name_id = *name_ref.nesting();
    }

    // Include all top level constants and globals, which are accessible everywhere
    let object = graph.declarations().get(&OBJECT_ID).unwrap().as_namespace().unwrap();
    collect_candidates!(
        graph,
        &object,
        context,
        candidates,
        Declaration::Namespace(_)
            | Declaration::Constant(_)
            | Declaration::ConstantAlias(_)
            | Declaration::GlobalVariable(_)
    );

    // Walk ancestors collecting all applicable completion members
    for ancestor in self_decl.ancestors() {
        if let Ancestor::Complete(ancestor_id) = ancestor {
            let ancestor_decl = graph.declarations().get(ancestor_id).unwrap().as_namespace().unwrap();
            collect_candidates!(&ancestor_decl, context, candidates);

            // Collect class variables from the attached object, which are available at any singleton class level
            // within self
            let attached_object = graph.attached_object(ancestor_decl);
            collect_candidates!(
                graph,
                &attached_object,
                context,
                candidates,
                Declaration::ClassVariable(_)
            );
        }
    }

    // Keywords are always available in expression contexts
    candidates.extend(keywords::KEYWORDS.iter().map(CompletionCandidate::Keyword));
    Ok(candidates)
}

/// Collect completion for a method argument (e.g.: `foo.bar(|)`)
fn method_argument_completion<'a>(
    graph: &'a Graph,
    self_name_id: NameId,
    method_decl_id: DeclarationId,
    context: CompletionContext<'a>,
) -> Result<Vec<CompletionCandidate>, Box<dyn Error>> {
    let mut candidates = expression_completion(graph, self_name_id, context)?;
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

/// Result of looking up the enclosing namespace for a definition.
#[allow(dead_code)]
enum EnclosingNamespace {
    /// Found a namespace (class, module, or singleton class).
    Found(DeclarationId),
    /// The definition has no enclosing namespace (top-level).
    NotFound,
    /// The enclosing namespace could not be determined because name resolution failed.
    Unresolved(DefinitionId),
}

/// Walks up the lexical nesting chain from the given `DefinitionId` to find
/// the nearest enclosing namespace (class, module, or singleton class).
fn enclosing_namespace(graph: &Graph, starting_id: Option<&DefinitionId>) -> EnclosingNamespace {
    let mut current = starting_id;
    while let Some(id) = current {
        let def = graph.definitions().get(id).unwrap();
        let is_namespace_def = matches!(
            def,
            Definition::Class(_) | Definition::Module(_) | Definition::SingletonClass(_)
        );

        let Some(decl_id) = graph.definition_id_to_declaration_id(*id) else {
            if is_namespace_def {
                return EnclosingNamespace::Unresolved(*id);
            }
            current = def.lexical_nesting_id().as_ref();
            continue;
        };
        if is_namespace_def {
            return EnclosingNamespace::Found(*decl_id);
        }
        current = def.lexical_nesting_id().as_ref();
    }
    EnclosingNamespace::NotFound
}

/// Result of dealiasing a single level of method alias.
pub enum DealiasMethodResult {
    /// The alias target is a concrete method definition.
    Method(DefinitionId),
    /// The alias target is another alias.
    Alias(DefinitionId),
}

/// Error type for `dealias_method`.
#[derive(Debug)]
pub enum DealiasMethodError {
    /// The constant receiver (e.g., `Foo` in `def Foo.bar`) could not be resolved.
    UnresolvedReceiver,
    /// The enclosing namespace's name could not be resolved.
    UnresolvedEnclosingNamespace(DefinitionId),
    /// No enclosing namespace exists (e.g., top-level alias with no lexical nesting).
    NoEnclosingNamespace,
    /// The aliased method name was not found in the namespace or its ancestors.
    MemberNotFound,
}

/// Dereferences a `MethodAliasDefinition` by one level, returning the definitions
/// found under the aliased name.
///
/// # Errors
///
/// Returns a `DealiasMethodError` if the alias target cannot be resolved.
///
/// # Panics
///
/// Panics if a `SelfReceiver` definition cannot be resolved to a namespace with a singleton class.
pub fn dealias_method(
    graph: &Graph,
    alias: &MethodAliasDefinition,
) -> Result<Vec<DealiasMethodResult>, DealiasMethodError> {
    let owner_id = match alias.receiver() {
        Some(Receiver::SelfReceiver(def_id)) => {
            let decl_id = graph.definition_id_to_declaration_id(*def_id).unwrap();
            let decl = graph.declarations().get(decl_id).unwrap();
            let ns = decl.as_namespace().unwrap();
            *ns.singleton_class().unwrap()
        }
        Some(Receiver::ConstantReceiver(name_id)) => {
            let Some(&id) = graph.name_id_to_declaration_id(*name_id) else {
                return Err(DealiasMethodError::UnresolvedReceiver);
            };
            id
        }
        None => match enclosing_namespace(graph, alias.lexical_nesting_id().as_ref()) {
            EnclosingNamespace::Found(id) => id,
            EnclosingNamespace::Unresolved(def_id) => {
                return Err(DealiasMethodError::UnresolvedEnclosingNamespace(def_id));
            }
            EnclosingNamespace::NotFound => {
                return Err(DealiasMethodError::NoEnclosingNamespace);
            }
        },
    };

    let Some(method_decl_id) =
        find_member_in_ancestors(graph, owner_id, *alias.old_name_str_id(), false)
    else {
        return Err(DealiasMethodError::MemberNotFound);
    };
    let method_decl = graph.declarations().get(&method_decl_id).unwrap();
    assert!(matches!(method_decl, Declaration::Method(_)));

    Ok(method_decl
        .definitions()
        .iter()
        .filter_map(|def_id| match graph.definitions().get(def_id) {
            Some(Definition::Method(_)) => Some(DealiasMethodResult::Method(*def_id)),
            Some(Definition::MethodAlias(_)) => Some(DealiasMethodResult::Alias(*def_id)),
            _ => None,
        })
        .collect())
}

/// Result of following a `MethodAliasDefinition` chain to completion.
#[derive(Debug)]
pub struct DeepDealiasMethodResult {
    /// Successfully resolved method definition IDs.
    pub method_ids: Vec<DefinitionId>,
    /// `DefinitionId`s where circular alias chains were detected.
    pub circular_aliases: Vec<DefinitionId>,
    /// Alias resolution errors (`DefinitionId` of the failing alias + the error).
    pub errors: Vec<(DefinitionId, DealiasMethodError)>,
}

/// Follows a `MethodAliasDefinition` chain to completion, returning all resolved
/// `MethodDefinition` IDs along with any errors encountered. Unlike `dealias_method`
/// which resolves one level, this function keeps following alias chains until only
/// method definitions remain.
///
/// # Panics
///
/// Panics if a `SelfReceiver` definition cannot be resolved to a namespace with a singleton class.
#[must_use]
pub fn deep_dealias_method(
    graph: &Graph,
    alias: &MethodAliasDefinition,
    alias_def_id: DefinitionId,
) -> DeepDealiasMethodResult {
    let mut result = DeepDealiasMethodResult {
        method_ids: Vec::new(),
        circular_aliases: Vec::new(),
        errors: Vec::new(),
    };

    let mut current_results = match dealias_method(graph, alias) {
        Ok(results) => results,
        Err(err) => {
            result.errors.push((alias_def_id, err));
            return result;
        }
    };

    let mut visited = HashSet::new();
    visited.insert(alias_def_id);

    loop {
        let mut next_aliases = Vec::new();

        for item in &current_results {
            match item {
                DealiasMethodResult::Method(id) => {
                    result.method_ids.push(*id);
                }
                DealiasMethodResult::Alias(id) => {
                    if !visited.insert(*id) {
                        result.circular_aliases.push(*id);
                        continue;
                    }
                    if let Some(Definition::MethodAlias(next_alias)) = graph.definitions().get(id) {
                        match dealias_method(graph, next_alias) {
                            Ok(next) => next_aliases.extend(next),
                            Err(err) => result.errors.push((*id, err)),
                        }
                    }
                }
            }
        }

        if next_aliases.is_empty() {
            return result;
        }

        current_results = next_aliases;
    }
}

/// Searches for a member by `StringId` in the given namespace's ancestor chain.
///
/// If `only_inherited` is true, all ancestors up to and including `namespace_id` itself
/// are skipped, so only ancestors after the namespace are searched. This excludes
/// the namespace's own members and any prepended modules.
///
/// # Panics
///
/// Panics if `namespace_id` does not exist in declarations, is not a namespace, or
/// if `only_inherited` is true and `namespace_id` is not found in its own ancestor chain.
#[must_use]
pub fn find_member_in_ancestors(
    graph: &Graph,
    namespace_id: DeclarationId,
    str_id: StringId,
    only_inherited: bool,
) -> Option<DeclarationId> {
    let ns = graph
        .declarations()
        .get(&namespace_id)
        .expect("namespace_id should exist in declarations")
        .as_namespace()
        .expect("namespace_id should be a namespace declaration");

    let ancestors: Vec<_> = ns.ancestors().iter().collect();

    let search_start = if only_inherited {
        let pos = ancestors
            .iter()
            .position(|a| matches!(a, Ancestor::Complete(id) if *id == namespace_id))
            .expect("namespace_id should be present in its own ancestor chain");
        pos + 1
    } else {
        0
    };

    for ancestor in &ancestors[search_start..] {
        if let Ancestor::Complete(ancestor_id) = ancestor {
            let ancestor_decl = graph.declarations().get(ancestor_id).unwrap();
            let ancestor_ns = ancestor_decl.as_namespace().unwrap();
            if let Some(&decl_id) = ancestor_ns.member(&str_id) {
                return Some(decl_id);
            }
        }
    }

    None
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
            assert_eq!(
                $expected,
                *completion_candidates($context.graph(), CompletionContext::new($receiver))
                    .unwrap()
                    .iter()
                    .map(|candidate| candidate_label(&$context, candidate))
                    .collect::<Vec<_>>()
            );
        };
    }

    /// Asserts declaration and keyword argument completion candidates, excluding language keywords.
    /// Language keywords are always present in expression contexts and tested separately.
    macro_rules! assert_declaration_completion_eq {
        ($context:expr, $receiver:expr, $expected:expr) => {
            assert_eq!(
                $expected,
                *completion_candidates($context.graph(), CompletionContext::new($receiver))
                    .unwrap()
                    .iter()
                    .filter(|c| !matches!(c, CompletionCandidate::Keyword(_)))
                    .map(|candidate| candidate_label(&$context, candidate))
                    .collect::<Vec<_>>()
            );
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
            CompletionReceiver::Expression(name_id),
            [
                "Child",
                "Foo",
                "Parent",
                "Child#baz()",
                "Foo::CONST",
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
            CompletionReceiver::Expression(name_id),
            ["Child", "Foo", "Parent", "Child#bar()"]
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
            CompletionReceiver::Expression(name_id),
            ["Baz", "Foo", "Bar", "Foo#foo_m()", "Baz#baz_m()", "Bar#bar_m()"]
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
        let name_id = Name::new(StringId::from("<Foo>"), ParentScope::Attached(foo_id), None).id();
        assert_declaration_completion_eq!(
            context,
            CompletionReceiver::Expression(name_id),
            ["Foo", "Bar", "Foo::<Foo>#do_something()", "Foo#@@foo_var"]
        );

        let name_id = Name::new(StringId::from("Bar"), ParentScope::None, None).id();
        assert_declaration_completion_eq!(
            context,
            CompletionReceiver::Expression(name_id),
            ["Foo", "Bar", "Bar#baz()", "Foo#@@foo_var"]
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
            CompletionReceiver::Expression(name_id),
            ["Foo::CONST_A", "Foo", "Bar", "Bar#bar_m()", "Bar#bar_m2()"]
        );

        let name_id = Name::new(StringId::from("Bar"), ParentScope::None, None).id();
        assert_declaration_completion_eq!(
            context,
            CompletionReceiver::Expression(name_id),
            ["Foo", "Bar", "Bar#bar_m()", "Bar#bar_m2()"]
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
            CompletionReceiver::Expression(name_id),
            ["Foo::CONST", "Foo::Bar", "Foo", "Foo::Bar#baz()"]
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
            CompletionReceiver::Expression(name_id),
            ["Foo::Bar", "$var", "Foo", "$var2", "Foo::Bar#bar_m()"]
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
                self_name_id: name_id,
                method_decl_id: DeclarationId::from("Foo#greet()"),
            },
            ["Foo", "Foo#greet()", "name:", "greeting:"]
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
                self_name_id: name_id,
                method_decl_id: DeclarationId::from("Foo#bar()"),
            },
            ["Foo", "Foo#bar()"]
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
                self_name_id: name_id,
                method_decl_id: DeclarationId::from("Foo#search()"),
            },
            // Only RequiredKeyword and OptionalKeyword, not RestKeyword (**opts)
            ["Foo", "Foo#search()", "limit:", "offset:"]
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
                self_name_id: name_id,
                method_decl_id: DeclarationId::from("Foo#bar()"),
            },
            ["Foo", "Foo#bar()", "first:", "second:"]
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
            CompletionReceiver::Expression(name_id),
            [
                "Foo",
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
                self_name_id: name_id,
                method_decl_id: DeclarationId::from("Foo#bar()"),
            },
            [
                "Foo",
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

    fn get_method_alias_def<'a>(graph: &'a Graph, decl_name: &str) -> &'a MethodAliasDefinition {
        let (_, alias) = get_method_alias_def_with_id(graph, decl_name);
        alias
    }

    fn get_method_alias_def_with_id<'a>(
        graph: &'a Graph,
        decl_name: &str,
    ) -> (DefinitionId, &'a MethodAliasDefinition) {
        let decl = graph.declarations().get(&DeclarationId::from(decl_name)).unwrap();
        for def_id in decl.definitions() {
            if let Some(Definition::MethodAlias(alias)) = graph.definitions().get(def_id) {
                return (*def_id, alias);
            }
        }
        panic!("No MethodAliasDefinition found for {decl_name}");
    }

    #[test]
    fn dealias_method_basic() {
        let mut context = GraphTest::new();
        context.index_uri(
            "file:///foo.rb",
            "
            class Foo
              def foo(a, b); end
              alias bar foo
            end
        ",
        );
        context.resolve();

        let alias = get_method_alias_def(context.graph(), "Foo#bar()");
        let results = dealias_method(context.graph(), alias).unwrap();
        assert_eq!(results.len(), 1);
        assert!(matches!(results[0], DealiasMethodResult::Method(_)));
    }

    #[test]
    fn dealias_method_chained() {
        let mut context = GraphTest::new();
        context.index_uri(
            "file:///foo.rb",
            "
            class Foo
              def foo(x); end
              alias bar foo
              alias baz bar
            end
        ",
        );
        context.resolve();

        // baz -> bar: one level returns the alias to bar
        let alias = get_method_alias_def(context.graph(), "Foo#baz()");
        let results = dealias_method(context.graph(), alias).unwrap();
        assert_eq!(results.len(), 1);
        assert!(matches!(results[0], DealiasMethodResult::Alias(_)));

        // bar -> foo: one more level returns the method definition
        let alias = get_method_alias_def(context.graph(), "Foo#bar()");
        let results = dealias_method(context.graph(), alias).unwrap();
        assert_eq!(results.len(), 1);
        assert!(matches!(results[0], DealiasMethodResult::Method(_)));
    }

    #[test]
    fn dealias_method_unresolved() {
        let mut context = GraphTest::new();
        context.index_uri(
            "file:///foo.rb",
            "
            class Foo
              alias bar nonexistent
            end
        ",
        );
        context.resolve();

        let alias = get_method_alias_def(context.graph(), "Foo#bar()");
        let result = dealias_method(context.graph(), alias);
        assert!(matches!(result, Err(DealiasMethodError::MemberNotFound)));
    }

    #[test]
    fn dealias_method_multiple_definitions() {
        let mut context = GraphTest::new();
        context.index_uri(
            "file:///foo.rb",
            "
            class Foo
              def foo(a); end
            end
        ",
        );
        context.index_uri(
            "file:///foo2.rb",
            "
            class Foo
              alias foo baz # another alias definition of #foo()
              alias bar foo
            end
        ",
        );
        context.resolve();

        let alias = get_method_alias_def(context.graph(), "Foo#bar()");
        let results = dealias_method(context.graph(), alias).unwrap();
        assert_eq!(results.len(), 2);
        assert_eq!(
            results
                .iter()
                .filter(|r| matches!(r, DealiasMethodResult::Method(_)))
                .count(),
            1
        );
        assert_eq!(
            results
                .iter()
                .filter(|r| matches!(r, DealiasMethodResult::Alias(_)))
                .count(),
            1
        );
    }

    #[test]
    fn dealias_method_inherited() {
        let mut context = GraphTest::new();
        context.index_uri(
            "file:///foo.rb",
            "
            class Parent
              def foo(a); end
            end
            class Child < Parent
              alias bar foo
            end
        ",
        );
        context.resolve();

        let alias = get_method_alias_def(context.graph(), "Child#bar()");
        let results = dealias_method(context.graph(), alias).unwrap();
        assert_eq!(results.len(), 1);
        assert!(matches!(results[0], DealiasMethodResult::Method(_)));
    }

    #[test]
    fn deep_dealias_method_mixed() {
        let mut context = GraphTest::new();
        // `then` has three definitions:
        //   - a Method (from file1)
        //   - an alias to `baz` which is circular (from file2)
        //   - an alias to `nonexistent` which is unresolved (from file2)
        // `start` aliases `then`, so deep_dealias_method(start) sees all three.
        context.index_uri("file:///foo1.rb", "
            class Foo
              def then(a); end
              alias bar baz
              alias baz bar
            end
        ");
        context.index_uri("file:///foo2.rb", "
            class Foo
              alias then baz
              alias then nonexistent
              alias start then
            end
        ");
        context.resolve();

        let (id, alias) = get_method_alias_def_with_id(context.graph(), "Foo#start()");
        let result = deep_dealias_method(context.graph(), alias, id);

        // Method definition of `then` is found
        assert_eq!(result.method_ids.len(), 1);
        // Circular chain via baz -> bar -> baz
        assert_eq!(result.circular_aliases.len(), 1);
        // Unresolved alias to nonexistent
        assert_eq!(result.errors.len(), 1);
        assert!(matches!(result.errors[0].1, DealiasMethodError::MemberNotFound));
    }

    #[test]
    fn find_member_in_ancestors_direct() {
        let mut context = GraphTest::new();
        context.index_uri(
            "file:///foo.rb",
            "
            class Foo
              def bar; end
            end
        ",
        );
        context.resolve();

        let result = find_member_in_ancestors(
            context.graph(),
            DeclarationId::from("Foo"),
            StringId::from("bar()"),
            false,
        );
        assert_eq!(result, Some(DeclarationId::from("Foo#bar()")));
    }

    #[test]
    fn find_member_in_ancestors_inherited() {
        let mut context = GraphTest::new();
        context.index_uri(
            "file:///foo.rb",
            "
            class Parent
              def foo; end
            end
            class Child < Parent
            end
        ",
        );
        context.resolve();

        let result = find_member_in_ancestors(
            context.graph(),
            DeclarationId::from("Child"),
            StringId::from("foo()"),
            false,
        );
        assert_eq!(result, Some(DeclarationId::from("Parent#foo()")));
    }

    #[test]
    fn find_member_in_ancestors_overridden() {
        let mut context = GraphTest::new();
        context.index_uri(
            "file:///foo.rb",
            "
            class Parent
              def foo; end
            end
            class Child < Parent
              def foo; end
            end
        ",
        );
        context.resolve();

        let result = find_member_in_ancestors(
            context.graph(),
            DeclarationId::from("Child"),
            StringId::from("foo()"),
            false,
        );
        assert_eq!(result, Some(DeclarationId::from("Child#foo()")));
    }

    #[test]
    fn find_member_in_ancestors_not_found() {
        let mut context = GraphTest::new();
        context.index_uri(
            "file:///foo.rb",
            "
            class Foo
            end
        ",
        );
        context.resolve();

        let result = find_member_in_ancestors(
            context.graph(),
            DeclarationId::from("Foo"),
            StringId::from("nonexistent()"),
            false,
        );
        assert_eq!(result, None);
    }

    #[test]
    fn find_member_in_ancestors_only_inherited() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", "
            class Parent
              def foo; end
            end
            class Child < Parent
              def foo; end
              def bar; end
            end
        ");
        context.resolve();

        // own method is skipped with only_inherited
        let result = find_member_in_ancestors(
            context.graph(),
            DeclarationId::from("Child"),
            StringId::from("foo()"),
            true,
        );
        assert_eq!(result, Some(DeclarationId::from("Parent#foo()")));

        // method only in self returns None with only_inherited
        let result = find_member_in_ancestors(
            context.graph(),
            DeclarationId::from("Child"),
            StringId::from("bar()"),
            true,
        );
        assert_eq!(result, None);
    }

    #[test]
    fn find_member_in_ancestors_only_inherited_with_prepend() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", "
            module M
              def foo; end
            end
            class Parent
              def foo; end
            end
            class Child < Parent
              prepend M
              def foo; end
            end
        ");
        context.resolve();

        // prepended module and self are skipped, finds Parent's foo
        let result = find_member_in_ancestors(
            context.graph(),
            DeclarationId::from("Child"),
            StringId::from("foo()"),
            true,
        );
        assert_eq!(result, Some(DeclarationId::from("Parent#foo()")));
    }

    #[test]
    fn find_member_in_ancestors_via_module() {
        let mut context = GraphTest::new();
        context.index_uri(
            "file:///foo.rb",
            "
            module Greetable
              def greet; end
            end
            class Foo
              include Greetable
            end
        ",
        );
        context.resolve();

        let result = find_member_in_ancestors(
            context.graph(),
            DeclarationId::from("Foo"),
            StringId::from("greet()"),
            false,
        );
        assert_eq!(result, Some(DeclarationId::from("Greetable#greet()")));
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
}
