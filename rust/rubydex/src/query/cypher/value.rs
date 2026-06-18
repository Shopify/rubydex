use std::cmp::Ordering;
use std::fmt::Write;

/// A scalar or composite value produced by evaluating a Cypher expression or projecting a result.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum CypherValue {
    Null,
    Bool(bool),
    Int(i64),
    Str(String),
    /// A graph node, rendered with its label and primary name.
    Node {
        label: String,
        name: String,
    },
    List(Vec<CypherValue>),
}

impl CypherValue {
    /// Returns the truthiness of a value for use in `WHERE` filtering.
    /// `NULL` and `false` are falsy; everything else (including bound nodes) is truthy.
    #[must_use]
    pub fn is_truthy(&self) -> bool {
        match self {
            CypherValue::Null => false,
            CypherValue::Bool(b) => *b,
            _ => true,
        }
    }

    /// Returns a numeric view of the value if it is an integer.
    #[must_use]
    pub fn as_int(&self) -> Option<i64> {
        match self {
            CypherValue::Int(i) => Some(*i),
            _ => None,
        }
    }

    /// Returns a string view of the value if it is a string.
    #[must_use]
    pub fn as_str(&self) -> Option<&str> {
        match self {
            CypherValue::Str(s) => Some(s),
            _ => None,
        }
    }

    /// Rank of each variant, used to order values of differing types deterministically.
    fn type_rank(&self) -> u8 {
        match self {
            CypherValue::Null => 0,
            CypherValue::Bool(_) => 1,
            CypherValue::Int(_) => 2,
            CypherValue::Str(_) => 3,
            CypherValue::Node { .. } => 4,
            CypherValue::List(_) => 5,
        }
    }

    /// Total ordering across all value types. `NULL` sorts first. Differing types are ordered by
    /// their variant rank so that sorting is always deterministic.
    #[must_use]
    pub fn total_cmp(&self, other: &CypherValue) -> Ordering {
        match (self, other) {
            (CypherValue::Bool(a), CypherValue::Bool(b)) => a.cmp(b),
            (CypherValue::Int(a), CypherValue::Int(b)) => a.cmp(b),
            (CypherValue::Str(a), CypherValue::Str(b))
            | (CypherValue::Node { name: a, .. }, CypherValue::Node { name: b, .. }) => a.cmp(b),
            (CypherValue::List(a), CypherValue::List(b)) => {
                for (x, y) in a.iter().zip(b.iter()) {
                    let ordering = x.total_cmp(y);
                    if ordering != Ordering::Equal {
                        return ordering;
                    }
                }
                a.len().cmp(&b.len())
            }
            _ => self.type_rank().cmp(&other.type_rank()),
        }
    }

    /// Renders the value for display in a plain-text table cell.
    #[must_use]
    pub fn to_display_string(&self) -> String {
        match self {
            CypherValue::Null => String::new(),
            CypherValue::Bool(b) => b.to_string(),
            CypherValue::Int(i) => i.to_string(),
            CypherValue::Str(s) => s.clone(),
            CypherValue::Node { name, .. } => name.clone(),
            CypherValue::List(items) => {
                let rendered: Vec<String> = items.iter().map(CypherValue::to_display_string).collect();
                format!("[{}]", rendered.join(", "))
            }
        }
    }

    /// Renders the value as a JSON fragment, appending to `out`.
    pub fn write_json(&self, out: &mut String) {
        match self {
            CypherValue::Null => out.push_str("null"),
            CypherValue::Bool(b) => {
                let _ = write!(out, "{b}");
            }
            CypherValue::Int(i) => {
                let _ = write!(out, "{i}");
            }
            CypherValue::Str(s) => write_json_string(out, s),
            CypherValue::Node { label, name } => {
                out.push_str("{\"label\":");
                write_json_string(out, label);
                out.push_str(",\"name\":");
                write_json_string(out, name);
                out.push('}');
            }
            CypherValue::List(items) => {
                out.push('[');
                for (index, item) in items.iter().enumerate() {
                    if index > 0 {
                        out.push(',');
                    }
                    item.write_json(out);
                }
                out.push(']');
            }
        }
    }
}

/// Escapes and quotes a string as a JSON string literal.
pub fn write_json_string(out: &mut String, value: &str) {
    out.push('"');
    for ch in value.chars() {
        match ch {
            '"' => out.push_str("\\\""),
            '\\' => out.push_str("\\\\"),
            '\n' => out.push_str("\\n"),
            '\r' => out.push_str("\\r"),
            '\t' => out.push_str("\\t"),
            c if (c as u32) < 0x20 => {
                let _ = write!(out, "\\u{:04x}", c as u32);
            }
            c => out.push(c),
        }
    }
    out.push('"');
}
