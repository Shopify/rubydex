use crate::errors::Errors;
use glob::glob;
use std::path::Path;

/// Recursively collects all Ruby files for the given workspace and dependencies, returning a vector of document instances
///
/// # Panics
///
/// Panics if there's a bug in how we're handling the arc mutex, like trying to acquire locks twice
#[must_use]
pub fn collect_file_paths(paths: Vec<String>) -> (Vec<String>, Vec<Errors>) {
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

    (file_paths, errors)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_utils::Context;

    fn collect_document_paths(context: &Context, paths: &[&str]) -> (Vec<String>, Vec<Errors>) {
        let (file_paths, errors) = collect_file_paths(
            paths
                .iter()
                .map(|p| context.absolute_path_to(p).to_string_lossy().into_owned())
                .collect(),
        );

        let mut paths: Vec<String> = file_paths
            .iter()
            .map(|path| context.relative_path_to(path).to_string_lossy().into_owned())
            .collect();

        paths.sort();

        (paths, errors)
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

        let (paths, errors) = collect_document_paths(&context, &["foo", "bar"]);

        assert_eq!(
            paths,
            vec![
                baz.to_str().unwrap().to_string(),
                qux.to_str().unwrap().to_string(),
                bar.to_str().unwrap().to_string()
            ]
        );
        assert!(errors.is_empty());
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
        let (paths, errors) = collect_document_paths(&context, &["bar"]);

        assert_eq!(
            paths,
            vec![baz.to_str().unwrap().to_string(), qux.to_str().unwrap().to_string()]
        );
        assert!(errors.is_empty());
    }

    #[test]
    fn collect_non_existing_paths() {
        let context = Context::new();

        let (documents, errors) = collect_file_paths(vec![
            context
                .absolute_path_to("non_existing_path")
                .to_string_lossy()
                .into_owned(),
        ]);

        assert!(documents.is_empty());

        assert_eq!(
            errors
                .iter()
                .map(std::string::ToString::to_string)
                .collect::<Vec<String>>(),
            vec![format!(
                "File read error: Path '{}' does not exist",
                context.absolute_path_to("non_existing_path").display()
            )]
        );
    }
}
