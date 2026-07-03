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
//! - `HAS_PARENT`: `Declaration` → `Declaration` (direct superclass; reverse-traverse for direct
//!   subclasses, `*` for the full chain)
//! - `INCLUDES` / `PREPENDS` / `EXTENDS`: `Declaration` → `Declaration` (mixins)
//! - `OWNS`: `Declaration` → `Declaration` (declaration-level membership, e.g. a namespace's methods
//!   and nested constants, merged across all files)
//! - `HAS_ANCESTOR`: `Declaration` → `Declaration` (linearized ancestor chain, incl. modules)
//! - `HAS_DESCENDANT`: `Declaration` → `Declaration` (reverse of `HAS_ANCESTOR`)
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
    /// `Declaration` → `Declaration`: the direct superclass (a single hop). Reverse-traverse for
    /// direct subclasses, or use `*` for the full class chain. For the transitive relation
    /// (including modules) see [`RelType::HasAncestor`].
    HasParent,
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
    HasAncestor,
    /// `Declaration` → `Declaration`: the reverse of [`RelType::HasAncestor`].
    HasDescendant,
    /// `Document` → `Declaration`: a constant reference in the file resolves to a declaration.
    References,
}

/// Catalog metadata for a relationship type: the type itself, its canonical name, endpoint labels,
/// and a short description. Entries live in [`REL_SCHEMAS`], the single source of truth that
/// `RelType`'s accessors and the `--schema` catalog both derive from.
pub struct RelSchema {
    pub rel: RelType,
    pub name: &'static str,
    pub from: &'static str,
    pub to: &'static str,
    pub description: &'static str,
}

/// The single source of truth for every relationship type: its name, endpoints, and description.
/// `RelType::all`/`name`/`parse`/`schema` and the `--schema` catalog all derive from this table.
const REL_SCHEMAS: &[RelSchema] = &[
    RelSchema {
        rel: RelType::Defines,
        name: "DEFINES",
        from: "Document",
        to: "Definition",
        description: "A file defines a construct occurrence",
    },
    RelSchema {
        rel: RelType::Declares,
        name: "DECLARES",
        from: "Definition",
        to: "Declaration",
        description: "An occurrence contributes to a declaration",
    },
    RelSchema {
        rel: RelType::Contains,
        name: "CONTAINS",
        from: "Definition",
        to: "Definition",
        description: "Lexical nesting of definitions",
    },
    RelSchema {
        rel: RelType::HasParent,
        name: "HAS_PARENT",
        from: "Class",
        to: "Class",
        description: "Direct superclass (use `*` for the full chain)",
    },
    RelSchema {
        rel: RelType::Includes,
        name: "INCLUDES",
        from: "Declaration",
        to: "Declaration",
        description: "`include` mixin",
    },
    RelSchema {
        rel: RelType::Prepends,
        name: "PREPENDS",
        from: "Declaration",
        to: "Declaration",
        description: "`prepend` mixin",
    },
    RelSchema {
        rel: RelType::Extends,
        name: "EXTENDS",
        from: "Declaration",
        to: "Declaration",
        description: "`extend` mixin",
    },
    RelSchema {
        rel: RelType::Owns,
        name: "OWNS",
        from: "Declaration",
        to: "Declaration",
        description: "A namespace owns a member declaration",
    },
    RelSchema {
        rel: RelType::HasAncestor,
        name: "HAS_ANCESTOR",
        from: "Declaration",
        to: "Declaration",
        description: "An entry in the linearized ancestor chain (incl. modules)",
    },
    RelSchema {
        rel: RelType::HasDescendant,
        name: "HAS_DESCENDANT",
        from: "Declaration",
        to: "Declaration",
        description: "A declaration that descends from this one",
    },
    RelSchema {
        rel: RelType::References,
        name: "REFERENCES",
        from: "Document",
        to: "Declaration",
        description: "A file references a constant declaration",
    },
];

impl RelType {
    /// All relationship types, in catalog order. Used when a pattern leaves the type unspecified.
    pub fn all() -> impl Iterator<Item = RelType> {
        REL_SCHEMAS.iter().map(|schema| schema.rel)
    }

    /// The catalog metadata for this relationship type, looked up in the [`REL_SCHEMAS`] table.
    ///
    /// # Panics
    ///
    /// Panics if a `RelType` variant has no `REL_SCHEMAS` entry. The table is exhaustive by
    /// construction (guarded by a test), so this cannot happen in practice.
    #[must_use]
    pub fn schema(self) -> &'static RelSchema {
        REL_SCHEMAS
            .iter()
            .find(|schema| schema.rel == self)
            .expect("every RelType has a REL_SCHEMAS entry")
    }

    /// The canonical uppercase name of this relationship type.
    #[must_use]
    pub fn name(self) -> &'static str {
        self.schema().name
    }

    /// Parses a relationship type name (case-insensitive). Returns `None` if unknown.
    #[must_use]
    pub fn parse(name: &str) -> Option<Self> {
        let upper = name.to_ascii_uppercase();
        REL_SCHEMAS
            .iter()
            .find(|schema| schema.name == upper)
            .map(|schema| schema.rel)
    }
}

/// A node label and what it matches, for the `--schema` catalog.
pub struct LabelSchema {
    pub label: &'static str,
    pub matches: &'static str,
    pub description: &'static str,
}

/// A property exposed on a node type, for the `--schema` catalog.
pub struct PropertySchema {
    pub node_type: &'static str,
    pub property: &'static str,
    pub description: &'static str,
}

/// The node labels queries can match against. Single source of truth for the `--schema` catalog;
/// the matching behavior lives in [`scan_label`] and [`declaration_matches_label`].
pub const NODE_LABELS: &[LabelSchema] = &[
    LabelSchema {
        label: "Document",
        matches: "source files",
        description: "A source file in the workspace",
    },
    LabelSchema {
        label: "Definition",
        matches: "per-file occurrences",
        description: "A single occurrence of a Ruby construct in one file",
    },
    LabelSchema {
        label: "Declaration",
        matches: "merged entities",
        description: "The global, merged concept of a named entity",
    },
    LabelSchema {
        label: "Namespace",
        matches: "Class | Module | SingletonClass declarations",
        description: "Grouping label for namespace-like declarations",
    },
    LabelSchema {
        label: "Class",
        matches: "declarations of kind Class",
        description: "A class declaration",
    },
    LabelSchema {
        label: "Module",
        matches: "declarations of kind Module",
        description: "A module declaration",
    },
    LabelSchema {
        label: "SingletonClass",
        matches: "declarations of kind SingletonClass",
        description: "A singleton class declaration",
    },
    LabelSchema {
        label: "Method",
        matches: "declarations of kind Method",
        description: "A method declaration",
    },
    LabelSchema {
        label: "Constant",
        matches: "declarations of kind Constant",
        description: "A constant declaration",
    },
    LabelSchema {
        label: "ConstantAlias",
        matches: "declarations of kind ConstantAlias",
        description: "A constant alias declaration",
    },
    LabelSchema {
        label: "GlobalVariable",
        matches: "declarations of kind GlobalVariable",
        description: "A global variable declaration",
    },
    LabelSchema {
        label: "InstanceVariable",
        matches: "declarations of kind InstanceVariable",
        description: "An instance variable declaration",
    },
    LabelSchema {
        label: "ClassVariable",
        matches: "declarations of kind ClassVariable",
        description: "A class variable declaration",
    },
];

/// The node properties queries can read. Single source of truth for the `--schema` catalog; the
/// value resolution lives in [`property`] and its per-node-type helpers.
pub const NODE_PROPERTIES: &[PropertySchema] = &[
    PropertySchema {
        node_type: "(any)",
        property: "label",
        description: "The node's top-level label / kind",
    },
    PropertySchema {
        node_type: "(any)",
        property: "kind",
        description: "Alias of `label`",
    },
    PropertySchema {
        node_type: "Declaration",
        property: "name",
        description: "Fully qualified name",
    },
    PropertySchema {
        node_type: "Declaration",
        property: "unqualified_name",
        description: "Name without its namespace prefix",
    },
    PropertySchema {
        node_type: "Declaration",
        property: "visibility",
        description: "public / protected / private (when applicable)",
    },
    PropertySchema {
        node_type: "Declaration",
        property: "definition_count",
        description: "Number of definitions that compose the declaration",
    },
    PropertySchema {
        node_type: "Definition",
        property: "name",
        description: "Name of the declaration this definition contributes to",
    },
    PropertySchema {
        node_type: "Definition",
        property: "file",
        description: "URI of the file containing the definition",
    },
    PropertySchema {
        node_type: "Definition",
        property: "line",
        description: "1-indexed start line of the definition",
    },
    PropertySchema {
        node_type: "Document",
        property: "uri",
        description: "Full document URI",
    },
    PropertySchema {
        node_type: "Document",
        property: "path",
        description: "File system path of the document",
    },
    PropertySchema {
        node_type: "Document",
        property: "name",
        description: "Base file name of the document",
    },
];

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
        RelType::all().map(|rel| rel.name().to_string()).collect()
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
            document.file_name().unwrap_or_else(|| document.uri().to_string())
        }),
    }
}

/// Resolves a node property to a value, where `prop` is the property name read off the node (the
/// `x` in `RETURN n.x` / `WHERE n.x = ...`). Unknown properties yield `NULL`.
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

    // Non-`file://` URIs (the synthetic built-in document) have no file path, so `path`/`name` fall
    // back to the raw URI.
    match prop {
        // Full document URI, e.g. `file:///app/models/user.rb`.
        "uri" => CypherValue::Str(document.uri().to_string()),
        // File-system path, e.g. `/app/models/user.rb`.
        "path" => CypherValue::Str(document.file_path().map_or_else(
            || document.uri().to_string(),
            |path| path.to_string_lossy().into_owned(),
        )),
        // Base file name, e.g. `user.rb`.
        "name" => CypherValue::Str(document.file_name().unwrap_or_else(|| document.uri().to_string())),
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
        RelType::HasParent
        | RelType::Includes
        | RelType::Prepends
        | RelType::Extends
        | RelType::Owns
        | RelType::HasAncestor
        | RelType::HasDescendant => graph
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
        (NodeRef::Declaration(decl_id), RelType::HasParent) => superclasses(graph, decl_id),
        (NodeRef::Declaration(decl_id), RelType::Includes) => mixin_targets(graph, decl_id, MixinKind::Include),
        (NodeRef::Declaration(decl_id), RelType::Prepends) => mixin_targets(graph, decl_id, MixinKind::Prepend),
        (NodeRef::Declaration(decl_id), RelType::Extends) => mixin_targets(graph, decl_id, MixinKind::Extend),
        (NodeRef::Declaration(decl_id), RelType::Owns) => members(graph, decl_id),
        (NodeRef::Declaration(decl_id), RelType::HasAncestor) => ancestors(graph, decl_id),
        (NodeRef::Declaration(decl_id), RelType::HasDescendant) => descendants(graph, decl_id),
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
