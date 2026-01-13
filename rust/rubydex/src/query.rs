use rayon::prelude::*;

use crate::model::graph::Graph;
use crate::model::ids::DeclarationId;

/// Searches declarations in parallel using rayon.
///
/// # Panics
///
/// Panics if the declarations read lock has been poisoned.
pub fn declaration_search(graph: &Graph, query: &str) -> Vec<DeclarationId> {
    let declarations = graph
        .declarations()
        .read()
        .expect("Failed to get read lock on declarations");

    declarations
        .par_iter()
        .filter(|(_, declaration)| {
            // When the query is empty, we return everything as per the LSP specification.
            // Otherwise, we compute the match score and return anything with a score greater than zero
            query.is_empty() || match_score(query, declaration.name()) > 0
        })
        .map(|(id, _)| *id)
        .collect()
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

#[cfg(test)]
mod tests {
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
                        .read()
                        .unwrap()
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
        assert_results_eq!(context, "Fo", vec!["Foo"]);
    }
}
