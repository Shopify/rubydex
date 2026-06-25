//! Maps the rubydex [`Graph`] onto a property-graph schema for Cypher execution.
//!
//! Node labels:
//! - `Document` — a source file.
//! - `Definition` — a per-file occurrence of a Ruby construct.
//! - `Declaration` — the global, merged concept of a named entity. Declarations also carry
//!   kind sub-labels (`Class`, `Module`, `SingletonClass`, `Method`, `Constant`, `ConstantAlias`,
//!   `GlobalVariable`, `InstanceVariable`, `ClassVariable`) plus the grouping label `Namespace`
//!   (any of `Class`/`Module`/`SingletonClass`).
//!
//! Relationship types mirror `dot.rs`:
//! - `DEFINES`: `Document` → `Definition`
//! - `DECLARES`: `Definition` → `Declaration`
//! - `CONTAINS`: `Definition` → `Definition` (lexical nesting in one file, e.g. a class written
//!   inside a module; the source-level counterpart of declaration-level `OWNS`)
//! - `INHERITS`: `Declaration` → `Declaration` (superclass)
//! - `INCLUDES` / `PREPENDS` / `EXTENDS`: `Declaration` → `Declaration` (mixins)
//! - `OWNS`: `Declaration` → `Declaration` (declaration-level membership, e.g. a namespace's methods
//!   and nested constants, merged across all files)
//! - `ANCESTOR`: `Declaration` → `Declaration` (linearized ancestor chain)
//! - `DESCENDANT`: `Declaration` → `Declaration`
//! - `REFERENCES`: `Document` → `Declaration` (constant references)

use std::collections::{HashSet, VecDeque};

use crate::model::declaration::Declaration;
use crate::model::definitions::{Definition, Mixin};
use crate::model::graph::Graph;
use crate::model::ids::{ConstantReferenceId, DeclarationId, DefinitionId, UriId};

use cypher_parser::{CypherValue, GraphProvider};

/// A handle to a node in the graph.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum NodeRef {
    Declaration(DeclarationId),
    Definition(DefinitionId),
    Document(UriId),
}

/// A relationship type.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum RelType {
    /// `Document` → `Definition`: a file defines a construct occurrence.
    Defines,
    /// `Definition` → `Declaration`: a per-file occurrence declares the global merged entity.
    Declares,
    /// `Definition` → `Definition`: lexical nesting within a single file, e.g. a class written
    /// inside a module. This is the source-level structure; the declaration-level (merged across
    /// files) counterpart is [`RelType::Owns`].
    Contains,
    /// `Declaration` → `Declaration`: direct superclass (a single hop, not the full chain).
    Inherits,
    /// `Declaration` → `Declaration`: an included module mixin.
    Includes,
    /// `Declaration` → `Declaration`: a prepended module mixin.
    Prepends,
    /// `Declaration` → `Declaration`: an extended module mixin.
    Extends,
    /// `Declaration` → `Declaration`: declaration-level membership (a namespace's methods and
    /// nested constants), merged across all files. The per-file source counterpart is
    /// [`RelType::Contains`].
    Owns,
    /// `Declaration` → `Declaration`: an entry in the linearized ancestor chain (transitive
    /// superclasses plus included/prepended modules).
    Ancestor,
    /// `Declaration` → `Declaration`: the reverse of [`RelType::Ancestor`].
    Descendant,
    /// `Document` → `Declaration`: a constant reference in the file resolves to a declaration.
    References,
}

impl RelType {
    /// Parses a relationship type name (case-insensitive). Returns `None` if unknown.
    #[must_use]
    pub fn parse(name: &str) -> Option<Self> {
        match name.to_ascii_uppercase().as_str() {
            "DEFINES" => Some(RelType::Defines),
            "DECLARES" => Some(RelType::Declares),
            "CONTAINS" => Some(RelType::Contains),
            "INHERITS" => Some(RelType::Inherits),
            "INCLUDES" => Some(RelType::Includes),
            "PREPENDS" => Some(RelType::Prepends),
            "EXTENDS" => Some(RelType::Extends),
            "OWNS" => Some(RelType::Owns),
            "ANCESTOR" => Some(RelType::Ancestor),
            "DESCENDANT" => Some(RelType::Descendant),
            "REFERENCES" => Some(RelType::References),
            _ => None,
        }
    }

    /// All relationship types, used when a pattern leaves the type unspecified.
    #[must_use]
    pub fn all() -> &'static [RelType] {
        &[
            RelType::Defines,
            RelType::Declares,
            RelType::Contains,
            RelType::Inherits,
            RelType::Includes,
            RelType::Prepends,
            RelType::Extends,
            RelType::Owns,
            RelType::Ancestor,
            RelType::Descendant,
            RelType::References,
        ]
    }

    /// The canonical uppercase name of this relationship type.
    #[must_use]
    pub fn name(self) -> &'static str {
        match self {
            RelType::Defines => "DEFINES",
            RelType::Declares => "DECLARES",
            RelType::Contains => "CONTAINS",
            RelType::Inherits => "INHERITS",
            RelType::Includes => "INCLUDES",
            RelType::Prepends => "PREPENDS",
            RelType::Extends => "EXTENDS",
            RelType::Owns => "OWNS",
            RelType::Ancestor => "ANCESTOR",
            RelType::Descendant => "DESCENDANT",
            RelType::References => "REFERENCES",
        }
    }
}

/// Exposes the rubydex [`Graph`] to the `cypher-parser` executor as a property graph. This is the
/// rubydex-specific mapping; the executor itself is generic over this trait.
impl GraphProvider for Graph {
    type NodeId = NodeRef;

    fn scan(&self, labels: &[String]) -> Vec<NodeRef> {
        scan(self, labels)
    }

    fn matches_label(&self, node: NodeRef, label: &str) -> bool {
        matches_label(self, node, label)
    }

    fn relationship_types(&self) -> Vec<String> {
        RelType::all().iter().map(|rel| rel.name().to_string()).collect()
    }

    fn expand(&self, node: NodeRef, rel_type: &str) -> Vec<NodeRef> {
        RelType::parse(rel_type).map_or_else(Vec::new, |rel| expand_out(self, node, rel))
    }

    fn rel_sources(&self, rel_type: &str) -> Vec<NodeRef> {
        RelType::parse(rel_type).map_or_else(Vec::new, |rel| rel_source_nodes(self, rel))
    }

    fn property(&self, node: NodeRef, prop: &str) -> CypherValue {
        property(self, node, prop)
    }

    fn label(&self, node: NodeRef) -> String {
        node_label(self, node)
    }

    fn name(&self, node: NodeRef) -> String {
        node_name(self, node)
    }
}

/// Returns all nodes matching the given labels. An empty slice matches every node; otherwise a node
/// is returned if it matches **any** of the labels (label disjunction, e.g. `(:Class|Module)`).
#[must_use]
pub fn scan(graph: &Graph, labels: &[String]) -> Vec<NodeRef> {
    if labels.is_empty() {
        let mut nodes = Vec::new();
        nodes.extend(graph.documents().keys().map(|id| NodeRef::Document(*id)));
        nodes.extend(graph.definitions().keys().map(|id| NodeRef::Definition(*id)));
        nodes.extend(graph.declarations().keys().map(|id| NodeRef::Declaration(*id)));
        return nodes;
    }

    let mut seen = HashSet::new();
    let mut nodes = Vec::new();
    for label in labels {
        for node in scan_label(graph, label) {
            if seen.insert(node) {
                nodes.push(node);
            }
        }
    }
    nodes
}

/// Returns all nodes matching a single label.
fn scan_label(graph: &Graph, label: &str) -> Vec<NodeRef> {
    match label {
        "Document" => graph.documents().keys().map(|id| NodeRef::Document(*id)).collect(),
        "Definition" => graph.definitions().keys().map(|id| NodeRef::Definition(*id)).collect(),
        other => graph
            .declarations()
            .iter()
            .filter(|(_, declaration)| declaration_matches_label(declaration, other))
            .map(|(id, _)| NodeRef::Declaration(*id))
            .collect(),
    }
}

/// Returns whether a node matches a single label.
#[must_use]
pub fn matches_label(graph: &Graph, node: NodeRef, label: &str) -> bool {
    match node {
        NodeRef::Document(_) => label == "Document",
        NodeRef::Definition(_) => label == "Definition",
        NodeRef::Declaration(id) => graph
            .declarations()
            .get(&id)
            .is_some_and(|declaration| declaration_matches_label(declaration, label)),
    }
}

fn declaration_matches_label(declaration: &Declaration, label: &str) -> bool {
    match label {
        "Declaration" => true,
        "Namespace" => declaration.as_namespace().is_some(),
        other => declaration.kind() == other,
    }
}

/// Returns the top-level label name of a node, used for display and JSON output.
#[must_use]
pub fn node_label(graph: &Graph, node: NodeRef) -> String {
    match node {
        NodeRef::Document(_) => "Document".to_string(),
        NodeRef::Definition(id) => graph
            .definitions()
            .get(&id)
            .map_or_else(|| "Definition".to_string(), |definition| definition.kind().to_string()),
        NodeRef::Declaration(id) => graph.declarations().get(&id).map_or_else(
            || "Declaration".to_string(),
            |declaration| declaration.kind().to_string(),
        ),
    }
}

/// The primary display name of a node (FQN for declarations, URI basename for documents).
#[must_use]
pub fn node_name(graph: &Graph, node: NodeRef) -> String {
    match node {
        NodeRef::Declaration(id) => graph
            .declarations()
            .get(&id)
            .map_or_else(String::new, |declaration| declaration.name().to_string()),
        NodeRef::Definition(id) => graph
            .definitions()
            .get(&id)
            .and_then(|definition| graph.definition_to_declaration_id(definition))
            .and_then(|decl_id| graph.declarations().get(decl_id))
            .map_or_else(String::new, |declaration| declaration.name().to_string()),
        NodeRef::Document(id) => graph.documents().get(&id).map_or_else(String::new, |document| {
            let uri = document.uri();
            uri.rsplit('/').next().unwrap_or(uri).to_string()
        }),
    }
}

/// Resolves a node property to a value. Unknown properties yield `NULL`.
#[must_use]
pub fn property(graph: &Graph, node: NodeRef, prop: &str) -> CypherValue {
    match prop {
        "label" | "kind" => CypherValue::Str(node_label(graph, node)),
        _ => match node {
            NodeRef::Declaration(id) => declaration_property(graph, id, prop),
            NodeRef::Definition(id) => definition_property(graph, id, prop),
            NodeRef::Document(id) => document_property(graph, id, prop),
        },
    }
}

fn declaration_property(graph: &Graph, id: DeclarationId, prop: &str) -> CypherValue {
    let Some(declaration) = graph.declarations().get(&id) else {
        return CypherValue::Null;
    };

    match prop {
        "name" => CypherValue::Str(declaration.name().to_string()),
        "unqualified_name" => CypherValue::Str(declaration.unqualified_name()),
        "visibility" => graph
            .visibility(&id)
            .map_or(CypherValue::Null, |visibility| CypherValue::Str(visibility.to_string())),
        "definition_count" => CypherValue::Int(i64::try_from(declaration.definitions().len()).unwrap_or(i64::MAX)),
        _ => CypherValue::Null,
    }
}

fn definition_property(graph: &Graph, id: DefinitionId, prop: &str) -> CypherValue {
    let Some(definition) = graph.definitions().get(&id) else {
        return CypherValue::Null;
    };

    match prop {
        "name" => CypherValue::Str(node_name(graph, NodeRef::Definition(id))),
        "file" => graph
            .documents()
            .get(definition.uri_id())
            .map_or(CypherValue::Null, |document| {
                CypherValue::Str(document.uri().to_string())
            }),
        "line" => graph
            .documents()
            .get(definition.uri_id())
            .map_or(CypherValue::Null, |document| {
                let location = definition.offset().to_location(document).to_presentation();
                CypherValue::Int(i64::from(location.start_line()))
            }),
        _ => CypherValue::Null,
    }
}

fn document_property(graph: &Graph, id: UriId, prop: &str) -> CypherValue {
    let Some(document) = graph.documents().get(&id) else {
        return CypherValue::Null;
    };

    match prop {
        "uri" => CypherValue::Str(document.uri().to_string()),
        "path" | "name" => {
            let uri = document.uri();
            CypherValue::Str(uri.rsplit('/').next().unwrap_or(uri).to_string())
        }
        _ => CypherValue::Null,
    }
}

/// Returns the candidate source nodes for a relationship type, used to build reverse adjacency.
#[must_use]
pub fn rel_source_nodes(graph: &Graph, rel: RelType) -> Vec<NodeRef> {
    match rel {
        RelType::Defines | RelType::References => graph.documents().keys().map(|id| NodeRef::Document(*id)).collect(),
        RelType::Declares | RelType::Contains => {
            graph.definitions().keys().map(|id| NodeRef::Definition(*id)).collect()
        }
        RelType::Inherits
        | RelType::Includes
        | RelType::Prepends
        | RelType::Extends
        | RelType::Owns
        | RelType::Ancestor
        | RelType::Descendant => graph
            .declarations()
            .keys()
            .map(|id| NodeRef::Declaration(*id))
            .collect(),
    }
}

/// Expands the outgoing edges of `node` for the given relationship type.
#[must_use]
pub fn expand_out(graph: &Graph, node: NodeRef, rel: RelType) -> Vec<NodeRef> {
    match (node, rel) {
        (NodeRef::Document(uri_id), RelType::Defines) => graph
            .documents()
            .get(&uri_id)
            .map(|document| {
                document
                    .definitions()
                    .iter()
                    .map(|id| NodeRef::Definition(*id))
                    .collect()
            })
            .unwrap_or_default(),
        (NodeRef::Document(uri_id), RelType::References) => document_references(graph, uri_id),
        (NodeRef::Definition(def_id), RelType::Declares) => graph
            .definitions()
            .get(&def_id)
            .and_then(|definition| graph.definition_to_declaration_id(definition))
            .map(|decl_id| vec![NodeRef::Declaration(*decl_id)])
            .unwrap_or_default(),
        (NodeRef::Definition(def_id), RelType::Contains) => definition_children(graph, def_id),
        (NodeRef::Declaration(decl_id), RelType::Inherits) => superclasses(graph, decl_id),
        (NodeRef::Declaration(decl_id), RelType::Includes) => mixin_targets(graph, decl_id, MixinKind::Include),
        (NodeRef::Declaration(decl_id), RelType::Prepends) => mixin_targets(graph, decl_id, MixinKind::Prepend),
        (NodeRef::Declaration(decl_id), RelType::Extends) => mixin_targets(graph, decl_id, MixinKind::Extend),
        (NodeRef::Declaration(decl_id), RelType::Owns) => members(graph, decl_id),
        (NodeRef::Declaration(decl_id), RelType::Ancestor) => ancestors(graph, decl_id),
        (NodeRef::Declaration(decl_id), RelType::Descendant) => descendants(graph, decl_id),
        _ => Vec::new(),
    }
}

fn document_references(graph: &Graph, uri_id: UriId) -> Vec<NodeRef> {
    let Some(document) = graph.documents().get(&uri_id) else {
        return Vec::new();
    };

    let mut seen = HashSet::new();
    let mut targets = Vec::new();
    for ref_id in document.constant_references() {
        if let Some(decl_id) = resolve_ref(graph, *ref_id)
            && seen.insert(decl_id)
        {
            targets.push(NodeRef::Declaration(decl_id));
        }
    }
    targets
}

fn definition_children(graph: &Graph, def_id: DefinitionId) -> Vec<NodeRef> {
    let Some(definition) = graph.definitions().get(&def_id) else {
        return Vec::new();
    };

    let children: &[DefinitionId] = match definition {
        Definition::Class(d) => d.members(),
        Definition::Module(d) => d.members(),
        Definition::SingletonClass(d) => d.members(),
        _ => &[],
    };
    children.iter().map(|id| NodeRef::Definition(*id)).collect()
}

fn superclasses(graph: &Graph, decl_id: DeclarationId) -> Vec<NodeRef> {
    let Some(declaration) = graph.declarations().get(&decl_id) else {
        return Vec::new();
    };

    let mut seen = HashSet::new();
    let mut targets = Vec::new();
    for definition_id in declaration.definitions() {
        if let Some(Definition::Class(class_def)) = graph.definitions().get(definition_id)
            && let Some(superclass_ref) = class_def.superclass_ref()
            && let Some(target) = resolve_ref_to_namespace(graph, *superclass_ref)
            && seen.insert(target)
        {
            targets.push(NodeRef::Declaration(target));
        }
    }
    targets
}

#[derive(Clone, Copy)]
enum MixinKind {
    Include,
    Prepend,
    Extend,
}

fn mixin_targets(graph: &Graph, decl_id: DeclarationId, kind: MixinKind) -> Vec<NodeRef> {
    let Some(declaration) = graph.declarations().get(&decl_id) else {
        return Vec::new();
    };

    let mut seen = HashSet::new();
    let mut targets = Vec::new();
    for definition_id in declaration.definitions() {
        let mixins: &[Mixin] = match graph.definitions().get(definition_id) {
            Some(Definition::Class(d)) => d.mixins(),
            Some(Definition::Module(d)) => d.mixins(),
            Some(Definition::SingletonClass(d)) => d.mixins(),
            _ => &[],
        };

        for mixin in mixins {
            let matches = matches!(
                (kind, mixin),
                (MixinKind::Include, Mixin::Include(_))
                    | (MixinKind::Prepend, Mixin::Prepend(_))
                    | (MixinKind::Extend, Mixin::Extend(_))
            );
            if matches
                && let Some(target) = resolve_ref_to_namespace(graph, *mixin.constant_reference_id())
                && seen.insert(target)
            {
                targets.push(NodeRef::Declaration(target));
            }
        }
    }
    targets
}

fn members(graph: &Graph, decl_id: DeclarationId) -> Vec<NodeRef> {
    graph
        .declarations()
        .get(&decl_id)
        .and_then(Declaration::as_namespace)
        .map(|namespace| {
            namespace
                .members()
                .values()
                .map(|id| NodeRef::Declaration(*id))
                .collect()
        })
        .unwrap_or_default()
}

fn ancestors(graph: &Graph, decl_id: DeclarationId) -> Vec<NodeRef> {
    use crate::model::declaration::Ancestor;

    graph
        .declarations()
        .get(&decl_id)
        .and_then(Declaration::as_namespace)
        .map(|namespace| {
            namespace
                .ancestors()
                .iter()
                .filter_map(|ancestor| match ancestor {
                    Ancestor::Complete(id) if *id != decl_id => Some(NodeRef::Declaration(*id)),
                    _ => None,
                })
                .collect()
        })
        .unwrap_or_default()
}

fn descendants(graph: &Graph, decl_id: DeclarationId) -> Vec<NodeRef> {
    graph
        .declarations()
        .get(&decl_id)
        .and_then(Declaration::as_namespace)
        .map(|namespace| {
            namespace
                .descendants()
                .iter()
                .map(|id| NodeRef::Declaration(*id))
                .collect()
        })
        .unwrap_or_default()
}

/// Resolves a constant reference to the declaration of the name it points to.
fn resolve_ref(graph: &Graph, ref_id: ConstantReferenceId) -> Option<DeclarationId> {
    let constant_ref = graph.constant_references().get(&ref_id)?;
    graph.name_id_to_declaration_id(*constant_ref.name_id()).copied()
}

/// Resolves a constant reference to a namespace declaration, following constant aliases.
fn resolve_ref_to_namespace(graph: &Graph, ref_id: ConstantReferenceId) -> Option<DeclarationId> {
    resolve_to_namespace(graph, resolve_ref(graph, ref_id)?)
}

/// Walks constant-alias chains until reaching a namespace declaration.
fn resolve_to_namespace(graph: &Graph, declaration_id: DeclarationId) -> Option<DeclarationId> {
    let mut queue = VecDeque::from([declaration_id]);
    let mut seen = HashSet::new();

    while let Some(current_id) = queue.pop_front() {
        if !seen.insert(current_id) {
            continue;
        }

        match graph.declarations().get(&current_id)? {
            Declaration::Namespace(_) => return Some(current_id),
            Declaration::ConstantAlias(_) => {
                queue.extend(graph.alias_targets(&current_id)?);
            }
            _ => {}
        }
    }

    None
}
