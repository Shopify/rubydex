//! Checks that the result of resolution does not depend on the order in which the resolver processes its units.
//!
//! `GraphTest::resolve` re-resolves the same input under shuffled unit orders and compares a snapshot of every fact
//! resolution produced against the default order. Fixtures known to depend on the order are recorded at the test site
//! with `GraphTest::order_dependent`. See step 1 of `docs/resolution-fixed-point.md`.

use std::collections::VecDeque;

use crate::model::{
    built_in::BUILT_IN_URI_ID,
    declaration::{Ancestor, Ancestors},
    definitions::Definition,
    graph::{Graph, Unit},
    ids::{DeclarationId, DefinitionId, UriId},
    name::NameRef,
};
use crate::offset::Offset;

/// Number of shuffled orders tried at each perturbation level
pub(crate) const SEEDS: u64 = 64;

/// How much freedom the check takes when reordering the units `Resolver::prepare_units` produced. Each level allows
/// every order of the previous one.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Perturbation {
    /// Shuffles what the resolver sort claims to order only for determinism: units of the same kind and name depth in
    /// any URI and offset order, and the remaining definitions (methods, attributes, variables, visibility changes) in
    /// any order
    Ties,
    /// Also ignores name depth: units of the same kind in any order, with the kinds still in sequence (definitions,
    /// constant references, singleton methods, ancestors)
    Kinds,
    /// The convergence loop queue in any order, with all kinds interleaved
    Full,
}

impl Perturbation {
    pub(crate) const ALL: [Perturbation; 3] = [Perturbation::Ties, Perturbation::Kinds, Perturbation::Full];
}

/// Why a fixture resolves differently when its units are reordered. Each variant names a hidden ordering assumption in
/// `resolution.rs`, so that the later steps of `docs/resolution-fixed-point.md` can burn the records down.
#[derive(Clone, Copy, Debug)]
pub enum OrderDependence {
    /// A lookup concluded that a constant does not exist while a definition that declares it was still pending: a
    /// reference was marked unresolved or resolved through an outer scope or the `Object` fallback, or a `Todo` was
    /// created for a scope defined later. References and definitions share interned names, so a reference concluded
    /// unresolved can also end with a resolved name that its declaration does not list. The quiescence step (step 3)
    /// is meant to remove these
    AbsenceReadEarly,
    /// The first definition processed for a declaration decides what later ones cannot change: its kind, whether a
    /// constant is promoted to a namespace, or the primary target of an alias. No step addresses this yet; it needs a
    /// rule that does not depend on processing order, such as source order
    DefinitionOrder,
    /// An ancestor chain was linearized and cached before every superclass, mixin or definition of the declaration was
    /// attached. Generic retractions (step 5) are meant to remove these
    StaleAncestors,
    /// The declaration where linearization enters an inheritance cycle decides the cyclic chain of every declaration
    /// in the cycle. No step addresses this yet
    CycleEntryPoint,
}

/// A deterministic generator (`SplitMix64`), so that every mismatch reproduces from its seed
pub(crate) struct Rng(u64);

impl Rng {
    pub(crate) fn new(seed: u64) -> Self {
        Self(seed)
    }

    fn next(&mut self) -> u64 {
        self.0 = self.0.wrapping_add(0x9E37_79B9_7F4A_7C15);
        let mut z = self.0;
        z = (z ^ (z >> 30)).wrapping_mul(0xBF58_476D_1CE4_E5B9);
        z = (z ^ (z >> 27)).wrapping_mul(0x94D0_49BB_1331_11EB);
        z ^ (z >> 31)
    }

    /// Fisher-Yates shuffle
    fn shuffle<T>(&mut self, items: &mut [T]) {
        for i in (1..items.len()).rev() {
            let bound = u64::try_from(i + 1).unwrap();
            let j = usize::try_from(self.next() % bound).unwrap();
            items.swap(i, j);
        }
    }
}

/// Reorders the prepared units within the freedom `level` allows
pub(crate) fn reorder(
    graph: &Graph,
    queue: &mut VecDeque<Unit>,
    others: &mut [DefinitionId],
    level: Perturbation,
    rng: &mut Rng,
) {
    let units = queue.make_contiguous();
    rng.shuffle(units);

    // A stable sort, so units with the same key stay in their shuffled order. Built-in definitions always go first:
    // they are the environment every fixture runs in, not part of the fixture, and `prepare_units` always enqueues
    // linearizing the built-in ancestors, which panics if it runs before `module Kernel` is declared
    units.sort_by_key(|unit| {
        let fixture = !is_built_in_definition(unit, graph);
        match level {
            Perturbation::Ties => (fixture, kind_rank(graph, unit), depth(graph, unit)),
            Perturbation::Kinds => (fixture, kind_rank(graph, unit), 0),
            Perturbation::Full => (fixture, 0, 0),
        }
    });

    rng.shuffle(others);
}

fn is_built_in_definition(unit: &Unit, graph: &Graph) -> bool {
    matches!(unit, Unit::Definition(id) if *graph.definitions().get(id).unwrap().uri_id() == *BUILT_IN_URI_ID)
}

/// The position of the unit's kind in the queue `prepare_units` builds
fn kind_rank(graph: &Graph, unit: &Unit) -> u8 {
    match unit {
        // Methods only enter the queue when they have a `self` receiver
        Unit::Definition(id) => match graph.definitions().get(id).unwrap() {
            Definition::Method(_) => 2,
            _ => 0,
        },
        Unit::ConstantRef(_) => 1,
        Unit::Ancestors(_) => 3,
    }
}

fn depth(graph: &Graph, unit: &Unit) -> u16 {
    let name_id = match unit {
        Unit::Definition(id) => graph.definitions().get(id).unwrap().name_id(),
        Unit::ConstantRef(id) => Some(graph.constant_references().get(id).unwrap().name_id()),
        Unit::Ancestors(_) => None,
    };

    name_id.map_or(0, |name_id| graph.names().get(name_id).unwrap().depth())
}

/// Every fact resolution produced, one per line and sorted, so that two graphs compare regardless of the order in which
/// the facts were stored and a mismatch reads as a diff
pub(crate) fn snapshot(graph: &Graph) -> Vec<String> {
    let name_of = |id: &DeclarationId| graph.declarations().get(id).map_or("<missing>", |d| d.name());
    let location = |uri_id: UriId, offset: &Offset| {
        let document = graph.documents().get(&uri_id).unwrap();
        format!("{}:{}", document.uri(), offset.to_display_range(document))
    };
    let mut facts = Vec::new();

    for (id, declaration) in graph.declarations() {
        let name = declaration.name();
        facts.push(format!(
            "declaration {name}: {} owned by {}",
            declaration.kind(),
            name_of(declaration.owner_id())
        ));

        for definition_id in declaration.definitions() {
            let definition = graph.definitions().get(definition_id).unwrap();
            facts.push(format!(
                "declaration {name}: defined by {} at {}",
                definition.kind(),
                location(*definition.uri_id(), definition.offset())
            ));
        }

        for reference_id in declaration.constant_references().unwrap_or_default() {
            let reference = graph.constant_references().get(reference_id).unwrap();
            facts.push(format!(
                "declaration {name}: referenced at {}",
                location(reference.uri_id(), reference.offset())
            ));
        }

        // Ordered: the first target is the primary one
        if let Some(targets) = graph.alias_targets(id) {
            let targets = targets.iter().map(name_of).collect::<Vec<_>>().join(", ");
            facts.push(format!("declaration {name}: alias of [{targets}]"));
        }

        let Some(namespace) = declaration.as_namespace() else {
            continue;
        };

        for (str_id, member_id) in namespace.members() {
            let member = graph.strings().get(str_id).unwrap().as_str();
            facts.push(format!("declaration {name}: member {member} is {}", name_of(member_id)));
        }

        if let Some(singleton_id) = namespace.singleton_class() {
            facts.push(format!("declaration {name}: singleton class {}", name_of(singleton_id)));
        }

        let (state, ancestors) = match namespace.ancestors() {
            Ancestors::Complete(ancestors) => ("complete", ancestors),
            Ancestors::Cyclic(ancestors) => ("cyclic", ancestors),
            Ancestors::Partial(ancestors) => ("partial", ancestors),
        };
        let ancestors = ancestors
            .iter()
            .map(|ancestor| match ancestor {
                Ancestor::Complete(id) => name_of(id).to_string(),
                Ancestor::Partial(name_id) => {
                    let str_id = graph.names().get(name_id).unwrap().str();
                    format!("?{}", graph.strings().get(str_id).unwrap().as_str())
                }
            })
            .collect::<Vec<_>>()
            .join(", ");
        facts.push(format!("declaration {name}: {state} ancestors [{ancestors}]"));

        for descendant_id in namespace.descendants() {
            facts.push(format!("declaration {name}: descendant {}", name_of(descendant_id)));
        }
    }

    for reference in graph.constant_references().values() {
        let target = match graph.names().get(reference.name_id()).unwrap() {
            NameRef::Resolved(resolved) => name_of(resolved.declaration_id()),
            NameRef::Unresolved(_) => "<unresolved>",
        };
        facts.push(format!(
            "reference at {}: resolved to {target}",
            location(reference.uri_id(), reference.offset())
        ));
    }

    for document in graph.documents().values() {
        for diagnostic in document.diagnostics() {
            facts.push(format!(
                "diagnostic in {}: {}",
                document.uri(),
                diagnostic.formatted(document)
            ));
        }
    }

    facts.sort();
    facts
}

/// Lines only in `expected` prefixed with `-` and lines only in `actual` prefixed with `+`. Both must be sorted
pub(crate) fn diff(expected: &[String], actual: &[String]) -> String {
    let mut lines = Vec::new();
    let mut expected = expected.iter().peekable();
    let mut actual = actual.iter().peekable();

    loop {
        match (expected.peek(), actual.peek()) {
            (None, None) => break,
            (Some(e), Some(a)) if e == a => {
                expected.next();
                actual.next();
            }
            (Some(e), Some(a)) if e < a => {
                lines.push(format!("- {e}"));
                expected.next();
            }
            (Some(e), None) => {
                lines.push(format!("- {e}"));
                expected.next();
            }
            (_, Some(a)) => {
                lines.push(format!("+ {a}"));
                actual.next();
            }
        }
    }

    lines.join("\n")
}

#[cfg(test)]
mod tests {
    use crate::test_utils::{GraphTest, OrderDependence, Perturbation};

    /// Order-dependent today: the first `FOO` definition processed becomes the primary alias target, and `Ties` puts
    /// either file first. Replace it with another recorded fixture once that is fixed
    fn order_dependent_fixture() -> GraphTest {
        let mut context = GraphTest::new();
        context.index_uri("file:///a.rb", "module A; end\nFOO = A");
        context.index_uri("file:///b.rb", "module B; end\nFOO = B");
        context
    }

    #[test]
    #[should_panic(
        expected = "- default order\n+ reordered\n- declaration FOO: alias of [A, B]\n+ declaration FOO: alias of [B, A]"
    )]
    fn unrecorded_order_dependence_fails_with_a_diff() {
        order_dependent_fixture().resolve();
    }

    #[test]
    #[should_panic(expected = "reordered under `Perturbation::Ties`")]
    fn levels_below_the_recorded_one_are_still_checked() {
        let mut context = order_dependent_fixture();
        context.order_dependent(Perturbation::Kinds, OrderDependence::DefinitionOrder);
        context.resolve();
    }

    #[test]
    #[should_panic(expected = "no resolve changes under `Perturbation::Ties` anymore")]
    fn record_that_no_longer_fails_is_reported() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", "class Foo; end");
        context.order_dependent(Perturbation::Ties, OrderDependence::DefinitionOrder);
        context.resolve();
    }
}
