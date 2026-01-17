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
                        if let Some(computed) = document.require_path(load_paths)
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

    #[test]
    fn test_resolve_require_path() {
        let root = if cfg!(windows) {
            PathBuf::from_str("C:\\")
        } else {
            PathBuf::from_str("/")
        }
        .unwrap();

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
}
