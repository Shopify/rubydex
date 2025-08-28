mod graph_test;

pub use graph_test::GraphTest;

#[must_use]
pub(crate) fn normalize_indentation(input: &str) -> String {
    let input = if let Some(rest) = input.strip_prefix('\n') {
        match rest.chars().next() {
            Some(' ' | '\t') => rest,
            _ => input,
        }
    } else {
        input
    };

    let lines: Vec<&str> = input.lines().collect();
    if lines.is_empty() {
        return String::new();
    }

    let first_non_empty_line = match lines.iter().find(|line| !line.trim().is_empty()) {
        Some(line) => *line,
        None => return input.to_string(),
    };

    let base_indent = first_non_empty_line.len() - first_non_empty_line.trim_start().len();

    let mut normalized = lines
        .iter()
        .map(|line| {
            if line.trim().is_empty() {
                ""
            } else if line.len() >= base_indent && line.chars().take(base_indent).all(char::is_whitespace) {
                &line[base_indent..]
            } else {
                line
            }
        })
        .collect::<Vec<_>>()
        .join("\n");

    if input.ends_with('\n') {
        normalized.push('\n');
    }

    normalized
}
