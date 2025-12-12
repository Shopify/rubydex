use crate::errors::{Errors, MultipleErrors};
use glob::glob;
use std::path::Path;

/// Recursively collects all Ruby files for the given workspace and dependencies, returning a vector of document instances
///
/// # Errors
///
/// Returns a `MultipleErrors` if any of the paths do not exist
pub fn collect_file_paths(paths: Vec<String>) -> Result<Vec<String>, MultipleErrors> {
    let mut errors = Vec::new();
    let mut file_paths = Vec::new();

    for path in paths {
        let path_obj = Path::new(&path);

        if path_obj.is_dir() {
            match glob(&format!("{path}/**/*.rb")) {
                Ok(entries) => {
                    for entry in entries {
                        match entry {
                            Ok(path) => file_paths.push(path.to_string_lossy().into_owned()),
                            Err(e) => errors.push(Errors::FileReadError(format!(
                                "Failed to read glob entry in '{path}': {e}"
                            ))),
                        }
                    }
                }
                Err(e) => {
                    errors.push(Errors::FileReadError(format!(
                        "Failed to read glob pattern '{path}/**/*.rb': {e}"
                    )));
                }
            }

            continue;
        }

        if path_obj.exists() {
            file_paths.push(path);

            continue;
        }

        errors.push(Errors::FileReadError(format!("Path '{path}' does not exist")));
    }

    if errors.is_empty() {
        Ok(file_paths)
    } else {
        Err(MultipleErrors(errors))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_utils::Context;

    fn collect_document_paths(context: &Context, paths: &[&str]) -> Result<Vec<String>, MultipleErrors> {
        let result = collect_file_paths(
            paths
                .iter()
                .map(|p| context.absolute_path_to(p).to_string_lossy().into_owned())
                .collect(),
        )?;

        let mut paths: Vec<String> = result
            .iter()
            .map(|path| context.relative_path_to(path).to_string_lossy().into_owned())
            .collect();

        paths.sort();

        Ok(paths)
    }

    #[test]
    fn collect_all_documents() {
        let context = Context::new();
        let baz = Path::new("bar").join("baz.rb");
        let qux = Path::new("bar").join("qux.rb");
        let bar = Path::new("foo").join("bar.rb");
        context.touch(&baz);
        context.touch(&qux);
        context.touch(&bar);

        let result = collect_document_paths(&context, &["foo", "bar"]);

        assert_eq!(
            result.unwrap(),
            vec![
                baz.to_str().unwrap().to_string(),
                qux.to_str().unwrap().to_string(),
                bar.to_str().unwrap().to_string()
            ]
        );
    }

    #[test]
    fn collect_some_documents_based_on_paths() {
        let context = Context::new();
        let baz = Path::new("bar").join("baz.rb");
        let qux = Path::new("bar").join("qux.rb");
        let bar = Path::new("foo").join("bar.rb");

        context.touch(&baz);
        context.touch(&qux);
        context.touch(&bar);

        let result = collect_document_paths(&context, &["bar"]);

        assert_eq!(
            result.unwrap(),
            vec![baz.to_str().unwrap().to_string(), qux.to_str().unwrap().to_string()]
        );
    }

    #[test]
    fn collect_non_existing_paths() {
        let context = Context::new();

        let result = collect_file_paths(vec![
            context
                .absolute_path_to("non_existing_path")
                .to_string_lossy()
                .into_owned(),
        ]);

        assert_eq!(
            result.unwrap_err().to_string(),
            format!(
                "File read error: Path '{}' does not exist",
                context.absolute_path_to("non_existing_path").display()
            )
        );
    }
}
