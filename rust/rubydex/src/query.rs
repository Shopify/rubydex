use std::collections::HashSet;
use std::error::Error;
use std::path::PathBuf;
use std::thread;

use url::Url;

use crate::model::declaration::{Ancestor, Declaration};
use crate::model::graph::{Graph, OBJECT_ID};
use crate::model::identity_maps::IdentityHashSet;
use crate::model::ids::{DeclarationId, NameId, StringId, UriId};
use crate::model::name::NameRef;

/// # Panics
///
/// Will panic if any of the threads panic
pub fn declaration_search(graph: &Graph, query: &str) -> Vec<DeclarationId> {
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
                            // When the query is empty, we return everything as per the LSP specification.
                            // Otherwise, we compute the match score and return anything with a score greater than zero
                            query.is_empty() || match_score(query, declaration.name()) > 0
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

/// The context in which completion is being requested
pub enum CompletionReceiver {
    /// Completion requested for an expression with no previous token (e.g.: at the start of a line with nothing before)
    /// Includes: all keywords, all global variables and reacheable instance variables, class variables, constants and methods
    Expression(NameId),
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
                $candidates.push(*member_declaration_id);
            }
        }
    };
    // Collect only members matching certain kinds
    ($graph:expr, $declaration:expr, $context:expr, $candidates:expr, $kinds:pat) => {
        for (member_str_id, member_declaration_id) in $declaration.members() {
            let member = $graph.declarations().get(member_declaration_id).unwrap();

            if matches!(member, $kinds) && $context.dedup(member_str_id) {
                $candidates.push(*member_declaration_id);
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
/// - Require path completion collects all require paths accessible from the `$LOAD_PATH`
/// - Relative require path completion collects all require paths accessible from the directory of the current file
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
) -> Result<Vec<DeclarationId>, Box<dyn Error>> {
    match context.completion_receiver {
        CompletionReceiver::Expression(self_name_id) => expression_completion(graph, self_name_id, context),
    }
}

/// Collect completion for an expression
fn expression_completion<'a>(
    graph: &'a Graph,
    self_name_id: NameId,
    mut context: CompletionContext<'a>,
) -> Result<Vec<DeclarationId>, Box<dyn Error>> {
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

    Ok(candidates)
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
            let actual = declaration_search(&$context.graph(), $query);
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
        let candidates = completion_candidates(
            context.graph(),
            CompletionContext::new(CompletionReceiver::Expression(name_id)),
        )
        .unwrap()
        .iter()
        .map(|id| context.graph().declarations().get(id).unwrap().name().to_string())
        .collect::<Vec<_>>();

        assert_eq!(
            vec![
                "Child",
                "Foo",
                "Parent",
                "Child#baz()",
                "Foo::CONST",
                "Foo#bar()",
                "Parent#initialize()",
                "Parent#@var"
            ],
            candidates
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
        let candidates = completion_candidates(
            context.graph(),
            CompletionContext::new(CompletionReceiver::Expression(name_id)),
        )
        .unwrap()
        .iter()
        .map(|id| context.graph().declarations().get(id).unwrap().name().to_string())
        .collect::<Vec<_>>();

        assert_eq!(vec!["Child", "Foo", "Parent", "Child#bar()"], candidates);
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
        let candidates = completion_candidates(
            context.graph(),
            CompletionContext::new(CompletionReceiver::Expression(name_id)),
        )
        .unwrap()
        .iter()
        .map(|id| context.graph().declarations().get(id).unwrap().name().to_string())
        .collect::<Vec<_>>();

        assert_eq!(
            vec!["Baz", "Foo", "Bar", "Foo#foo_m()", "Baz#baz_m()", "Bar#bar_m()"],
            candidates
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

        let candidates = completion_candidates(
            context.graph(),
            CompletionContext::new(CompletionReceiver::Expression(name_id)),
        )
        .unwrap()
        .iter()
        .map(|id| context.graph().declarations().get(id).unwrap().name().to_string())
        .collect::<Vec<_>>();

        assert_eq!(
            vec!["Foo", "Bar", "Foo::<Foo>#do_something()", "Foo#@@foo_var"],
            candidates
        );

        let name_id = Name::new(StringId::from("Bar"), ParentScope::None, None).id();
        let candidates = completion_candidates(
            context.graph(),
            CompletionContext::new(CompletionReceiver::Expression(name_id)),
        )
        .unwrap()
        .iter()
        .map(|id| context.graph().declarations().get(id).unwrap().name().to_string())
        .collect::<Vec<_>>();

        assert_eq!(vec!["Foo", "Bar", "Bar#baz()", "Foo#@@foo_var"], candidates);
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
        let candidates = completion_candidates(
            context.graph(),
            CompletionContext::new(CompletionReceiver::Expression(name_id)),
        )
        .unwrap()
        .iter()
        .map(|id| context.graph().declarations().get(id).unwrap().name().to_string())
        .collect::<Vec<_>>();

        assert_eq!(
            vec!["Foo::CONST_A", "Foo", "Bar", "Bar#bar_m()", "Bar#bar_m2()"],
            candidates
        );

        let name_id = Name::new(StringId::from("Bar"), ParentScope::None, None).id();
        let candidates = completion_candidates(
            context.graph(),
            CompletionContext::new(CompletionReceiver::Expression(name_id)),
        )
        .unwrap()
        .iter()
        .map(|id| context.graph().declarations().get(id).unwrap().name().to_string())
        .collect::<Vec<_>>();

        assert_eq!(vec!["Foo", "Bar", "Bar#bar_m()", "Bar#bar_m2()"], candidates);
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
        let candidates = completion_candidates(
            context.graph(),
            CompletionContext::new(CompletionReceiver::Expression(name_id)),
        )
        .unwrap()
        .iter()
        .map(|id| context.graph().declarations().get(id).unwrap().name().to_string())
        .collect::<Vec<_>>();

        // Foo::CONST is reachable from Foo::Bar through lexical scoping, so it must
        // appear as a completion candidate when the user types the unqualified name CONST
        assert!(
            candidates.contains(&"Foo::CONST".to_string()),
            "Expected Foo::CONST in candidates, got: {candidates:?}",
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
        let candidates = completion_candidates(
            context.graph(),
            CompletionContext::new(CompletionReceiver::Expression(name_id)),
        )
        .unwrap()
        .iter()
        .map(|id| context.graph().declarations().get(id).unwrap().name().to_string())
        .collect::<Vec<_>>();

        assert_eq!(vec!["Foo::Bar", "$var", "Foo", "$var2", "Foo::Bar#bar_m()"], candidates);
    }
}
