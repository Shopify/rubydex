//! Static, self-describing catalog of the Cypher property-graph model: the node labels,
//! relationship types, and node properties that queries can use. This mirrors the mapping
//! implemented in [`super::schema`] and is exposed via `--schema` for discoverability.

use cypher_parser::OutputFormat;
use cypher_parser::value::write_json_string;

/// A node label and what graph entity it matches.
struct LabelInfo {
    label: &'static str,
    matches: &'static str,
    description: &'static str,
}

/// A relationship type and its endpoints.
struct RelInfo {
    name: &'static str,
    from: &'static str,
    to: &'static str,
    description: &'static str,
}

/// A property exposed on a node type.
struct PropInfo {
    node_type: &'static str,
    property: &'static str,
    description: &'static str,
}

const LABELS: &[LabelInfo] = &[
    LabelInfo {
        label: "Document",
        matches: "source files",
        description: "A source file in the workspace",
    },
    LabelInfo {
        label: "Definition",
        matches: "per-file occurrences",
        description: "A single occurrence of a Ruby construct in one file",
    },
    LabelInfo {
        label: "Declaration",
        matches: "merged entities",
        description: "The global, merged concept of a named entity",
    },
    LabelInfo {
        label: "Namespace",
        matches: "Class | Module | SingletonClass declarations",
        description: "Grouping label for namespace-like declarations",
    },
    LabelInfo {
        label: "Class",
        matches: "declarations of kind Class",
        description: "A class declaration",
    },
    LabelInfo {
        label: "Module",
        matches: "declarations of kind Module",
        description: "A module declaration",
    },
    LabelInfo {
        label: "SingletonClass",
        matches: "declarations of kind SingletonClass",
        description: "A singleton class declaration",
    },
    LabelInfo {
        label: "Method",
        matches: "declarations of kind Method",
        description: "A method declaration",
    },
    LabelInfo {
        label: "Constant",
        matches: "declarations of kind Constant",
        description: "A constant declaration",
    },
    LabelInfo {
        label: "ConstantAlias",
        matches: "declarations of kind ConstantAlias",
        description: "A constant alias declaration",
    },
    LabelInfo {
        label: "GlobalVariable",
        matches: "declarations of kind GlobalVariable",
        description: "A global variable declaration",
    },
    LabelInfo {
        label: "InstanceVariable",
        matches: "declarations of kind InstanceVariable",
        description: "An instance variable declaration",
    },
    LabelInfo {
        label: "ClassVariable",
        matches: "declarations of kind ClassVariable",
        description: "A class variable declaration",
    },
];

const RELATIONSHIPS: &[RelInfo] = &[
    RelInfo {
        name: "DEFINES",
        from: "Document",
        to: "Definition",
        description: "A file defines a construct occurrence",
    },
    RelInfo {
        name: "DECLARES",
        from: "Definition",
        to: "Declaration",
        description: "An occurrence contributes to a declaration",
    },
    RelInfo {
        name: "CONTAINS",
        from: "Definition",
        to: "Definition",
        description: "Lexical nesting of definitions",
    },
    RelInfo {
        name: "INHERITS",
        from: "Class",
        to: "Class",
        description: "Superclass relationship",
    },
    RelInfo {
        name: "INCLUDES",
        from: "Declaration",
        to: "Declaration",
        description: "`include` mixin",
    },
    RelInfo {
        name: "PREPENDS",
        from: "Declaration",
        to: "Declaration",
        description: "`prepend` mixin",
    },
    RelInfo {
        name: "EXTENDS",
        from: "Declaration",
        to: "Declaration",
        description: "`extend` mixin",
    },
    RelInfo {
        name: "OWNS",
        from: "Declaration",
        to: "Declaration",
        description: "A namespace owns a member declaration",
    },
    RelInfo {
        name: "ANCESTOR",
        from: "Declaration",
        to: "Declaration",
        description: "An entry in the linearized ancestor chain",
    },
    RelInfo {
        name: "DESCENDANT",
        from: "Declaration",
        to: "Declaration",
        description: "A declaration that descends from this one",
    },
    RelInfo {
        name: "REFERENCES",
        from: "Document",
        to: "Declaration",
        description: "A file references a constant declaration",
    },
];

const PROPERTIES: &[PropInfo] = &[
    PropInfo {
        node_type: "(any)",
        property: "label",
        description: "The node's top-level label / kind",
    },
    PropInfo {
        node_type: "(any)",
        property: "kind",
        description: "Alias of `label`",
    },
    PropInfo {
        node_type: "Declaration",
        property: "name",
        description: "Fully qualified name",
    },
    PropInfo {
        node_type: "Declaration",
        property: "unqualified_name",
        description: "Name without its namespace prefix",
    },
    PropInfo {
        node_type: "Declaration",
        property: "visibility",
        description: "public / protected / private (when applicable)",
    },
    PropInfo {
        node_type: "Declaration",
        property: "definition_count",
        description: "Number of definitions that compose the declaration",
    },
    PropInfo {
        node_type: "Definition",
        property: "name",
        description: "Name of the declaration this definition contributes to",
    },
    PropInfo {
        node_type: "Definition",
        property: "file",
        description: "URI of the file containing the definition",
    },
    PropInfo {
        node_type: "Definition",
        property: "line",
        description: "1-indexed start line of the definition",
    },
    PropInfo {
        node_type: "Document",
        property: "uri",
        description: "Full document URI",
    },
    PropInfo {
        node_type: "Document",
        property: "path",
        description: "File system path of the document",
    },
    PropInfo {
        node_type: "Document",
        property: "name",
        description: "Base file name of the document",
    },
];

/// Renders the schema catalog in the requested format.
#[must_use]
pub fn describe(format: OutputFormat) -> String {
    match format {
        OutputFormat::Table => render_table(),
        OutputFormat::Json => render_json(),
    }
}

fn render_table() -> String {
    let mut out = String::new();

    out.push_str("Node labels\n");
    let label_rows: Vec<[&str; 3]> = LABELS.iter().map(|l| [l.label, l.matches, l.description]).collect();
    push_table(&mut out, &["Label", "Matches", "Description"], &label_rows);

    out.push_str("\nRelationship types\n");
    let rel_rows: Vec<[&str; 4]> = RELATIONSHIPS
        .iter()
        .map(|r| [r.name, r.from, r.to, r.description])
        .collect();
    push_table(&mut out, &["Type", "From", "To", "Description"], &rel_rows);

    out.push_str("\nProperties\n");
    let prop_rows: Vec<[&str; 3]> = PROPERTIES
        .iter()
        .map(|p| [p.node_type, p.property, p.description])
        .collect();
    push_table(&mut out, &["Node type", "Property", "Description"], &prop_rows);

    out
}

/// Renders a single aligned table section. `N` is the column count.
fn push_table<const N: usize>(out: &mut String, headers: &[&str; N], rows: &[[&str; N]]) {
    let mut widths: [usize; N] = std::array::from_fn(|i| headers[i].chars().count());
    for row in rows {
        for (index, cell) in row.iter().enumerate() {
            widths[index] = widths[index].max(cell.chars().count());
        }
    }

    push_table_row(out, headers, &widths);
    for (index, width) in widths.iter().enumerate() {
        if index > 0 {
            out.push_str("-+-");
        }
        for _ in 0..*width {
            out.push('-');
        }
    }
    out.push('\n');
    for row in rows {
        push_table_row(out, row, &widths);
    }
}

fn push_table_row<const N: usize>(out: &mut String, cells: &[&str; N], widths: &[usize; N]) {
    for (index, width) in widths.iter().enumerate() {
        if index > 0 {
            out.push_str(" | ");
        }
        let cell = cells[index];
        out.push_str(cell);
        for _ in 0..width.saturating_sub(cell.chars().count()) {
            out.push(' ');
        }
    }
    out.push('\n');
}

fn render_json() -> String {
    let mut out = String::from("{\"node_labels\":[");
    for (index, label) in LABELS.iter().enumerate() {
        if index > 0 {
            out.push(',');
        }
        out.push_str("{\"label\":");
        write_json_string(&mut out, label.label);
        out.push_str(",\"matches\":");
        write_json_string(&mut out, label.matches);
        out.push_str(",\"description\":");
        write_json_string(&mut out, label.description);
        out.push('}');
    }

    out.push_str("],\"relationships\":[");
    for (index, rel) in RELATIONSHIPS.iter().enumerate() {
        if index > 0 {
            out.push(',');
        }
        out.push_str("{\"type\":");
        write_json_string(&mut out, rel.name);
        out.push_str(",\"from\":");
        write_json_string(&mut out, rel.from);
        out.push_str(",\"to\":");
        write_json_string(&mut out, rel.to);
        out.push_str(",\"description\":");
        write_json_string(&mut out, rel.description);
        out.push('}');
    }

    out.push_str("],\"properties\":[");
    for (index, prop) in PROPERTIES.iter().enumerate() {
        if index > 0 {
            out.push(',');
        }
        out.push_str("{\"node_type\":");
        write_json_string(&mut out, prop.node_type);
        out.push_str(",\"property\":");
        write_json_string(&mut out, prop.property);
        out.push_str(",\"description\":");
        write_json_string(&mut out, prop.description);
        out.push('}');
    }

    out.push_str("]}");
    out
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn table_lists_labels_relationships_and_properties() {
        let output = describe(OutputFormat::Table);
        assert!(output.contains("Node labels"));
        assert!(output.contains("Relationship types"));
        assert!(output.contains("Properties"));
        assert!(output.contains("Namespace"));
        assert!(output.contains("INHERITS"));
        assert!(output.contains("unqualified_name"));
    }

    #[test]
    fn json_is_well_formed_object() {
        let output = describe(OutputFormat::Json);
        assert!(output.starts_with("{\"node_labels\":["));
        assert!(output.contains("\"relationships\":["));
        assert!(output.contains("\"properties\":["));
        assert!(output.contains("\"type\":\"DEFINES\""));
        assert!(output.ends_with("]}"));
    }
}
