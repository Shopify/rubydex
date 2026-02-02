use std::collections::HashSet;
use std::path::PathBuf;
use std::sync::atomic::{AtomicBool, Ordering};
use std::thread;

use crate::model::graph::Graph;
use crate::model::ids::{DeclarationId, UriId};

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
/// # Panics
///
/// Panics if one of the search threads panics
#[must_use]
pub fn resolve_require_path(graph: &Graph, require_path: &str, load_paths: &[PathBuf]) -> Option<UriId> {
    let found = AtomicBool::new(false);
    let found_ref = &found;
    let normalized = require_path.trim_end_matches(".rb");
    let num_threads = thread::available_parallelism().map(std::num::NonZero::get).unwrap_or(4);
    let documents = graph.documents().iter().collect::<Vec<_>>();
    let chunk_size = documents.len().div_ceil(num_threads);

    if chunk_size == 0 {
        return None;
    }

    thread::scope(|scope| {
        let handles: Vec<_> = documents
            .chunks(chunk_size)
            .map(|chunk| {
                scope.spawn(move || {
                    for (id, document) in chunk {
                        // Check if another thread found it
                        if found_ref.load(Ordering::Relaxed) {
                            return None;
                        }
                        if let Some((computed, _)) = document.require_path(load_paths)
                            && computed == normalized
                        {
                            found_ref.store(true, Ordering::Relaxed);
                            return Some(**id);
                        }
                    }
                    None
                })
            })
            .collect();

        handles.into_iter().find_map(|handle| handle.join().unwrap())
    })
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

#[cfg(test)]
mod tests {
    use std::str::FromStr;
    use url::Url;

    use super::*;
    use crate::test_utils::GraphTest;

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
        let root = if cfg!(windows) {
            PathBuf::from_str("C:\\")
        } else {
            PathBuf::from_str("/")
        }
        .unwrap();

        root.join("test")
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
}
