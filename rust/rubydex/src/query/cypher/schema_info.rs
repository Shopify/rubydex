//! Renders the Cypher property-graph model — node labels, relationship types, and node properties —
//! for the `--schema` output. The model itself is defined once in [`super::schema`] (labels and
//! properties as `NODE_LABELS`/`NODE_PROPERTIES`, relationships as [`RelType`]); this module only
//! formats it, so there is a single source of truth.

use cypher_parser::OutputFormat;
use cypher_parser::value::write_json_string;

use super::schema::{NODE_LABELS, NODE_PROPERTIES, RelType};

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
    let label_rows: Vec<[&str; 3]> = NODE_LABELS
        .iter()
        .map(|l| [l.label, l.matches, l.description])
        .collect();
    push_table(&mut out, &["Label", "Matches", "Description"], &label_rows);

    out.push_str("\nRelationship types\n");
    let rel_rows: Vec<[&str; 4]> = RelType::all()
        .map(|rel| {
            let schema = rel.schema();
            [schema.name, schema.from, schema.to, schema.description]
        })
        .collect();
    push_table(&mut out, &["Type", "From", "To", "Description"], &rel_rows);

    out.push_str("\nProperties\n");
    let prop_rows: Vec<[&str; 3]> = NODE_PROPERTIES
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
    for (index, label) in NODE_LABELS.iter().enumerate() {
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
    for (index, rel) in RelType::all().enumerate() {
        if index > 0 {
            out.push(',');
        }
        let schema = rel.schema();
        out.push_str("{\"type\":");
        write_json_string(&mut out, schema.name);
        out.push_str(",\"from\":");
        write_json_string(&mut out, schema.from);
        out.push_str(",\"to\":");
        write_json_string(&mut out, schema.to);
        out.push_str(",\"description\":");
        write_json_string(&mut out, schema.description);
        out.push('}');
    }

    out.push_str("],\"properties\":[");
    for (index, prop) in NODE_PROPERTIES.iter().enumerate() {
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
        assert!(output.contains("HAS_PARENT"));
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
