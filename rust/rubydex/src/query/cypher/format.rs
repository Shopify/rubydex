use std::fmt::Write;

use super::executor::ResultSet;
use super::value::{CypherValue, write_json_string};

/// The output format for query results.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum OutputFormat {
    Table,
    Json,
}

/// Renders a result set in the requested format.
#[must_use]
pub fn format(result: &ResultSet, format: OutputFormat) -> String {
    match format {
        OutputFormat::Table => format_table(result),
        OutputFormat::Json => format_json(result),
    }
}

fn format_table(result: &ResultSet) -> String {
    if result.columns.is_empty() {
        return String::new();
    }

    let rendered: Vec<Vec<String>> = result
        .rows
        .iter()
        .map(|row| row.iter().map(CypherValue::to_display_string).collect())
        .collect();

    let mut widths: Vec<usize> = result.columns.iter().map(String::len).collect();
    for row in &rendered {
        for (index, cell) in row.iter().enumerate() {
            if let Some(width) = widths.get_mut(index) {
                *width = (*width).max(cell.chars().count());
            }
        }
    }

    let mut output = String::new();
    push_row(&mut output, &result.columns, &widths);
    push_separator(&mut output, &widths);
    for row in &rendered {
        push_row(&mut output, row, &widths);
    }

    let count = result.rows.len();
    let suffix = if count == 1 { "row" } else { "rows" };
    let _ = write!(output, "\n{count} {suffix}\n");
    output
}

fn push_row(output: &mut String, cells: &[String], widths: &[usize]) {
    for (index, width) in widths.iter().enumerate() {
        if index > 0 {
            output.push_str(" | ");
        }
        let empty = String::new();
        let cell = cells.get(index).unwrap_or(&empty);
        let pad = width.saturating_sub(cell.chars().count());
        output.push_str(cell);
        for _ in 0..pad {
            output.push(' ');
        }
    }
    output.push('\n');
}

fn push_separator(output: &mut String, widths: &[usize]) {
    for (index, width) in widths.iter().enumerate() {
        if index > 0 {
            output.push_str("-+-");
        }
        for _ in 0..*width {
            output.push('-');
        }
    }
    output.push('\n');
}

fn format_json(result: &ResultSet) -> String {
    let mut output = String::from("[");
    for (row_index, row) in result.rows.iter().enumerate() {
        if row_index > 0 {
            output.push(',');
        }
        output.push('{');
        for (column_index, column) in result.columns.iter().enumerate() {
            if column_index > 0 {
                output.push(',');
            }
            write_json_string(&mut output, column);
            output.push(':');
            row.get(column_index)
                .unwrap_or(&CypherValue::Null)
                .write_json(&mut output);
        }
        output.push('}');
    }
    output.push(']');
    output
}
