use crate::errors::Errors;
use std::{
    fs,
    path::{Path, PathBuf},
};

/// Recursively collects all Ruby files for the given paths.
#[must_use]
pub fn collect_file_paths(paths: Vec<String>) -> (Vec<PathBuf>, Vec<Errors>) {
    let mut files = Vec::new();
    let mut errors = Vec::new();

    for path in paths {
        let Ok(canonicalized) = fs::canonicalize(&path) else {
            errors.push(Errors::FileError(format!("Path `{path}` does not exist")));
            continue;
        };

        collect_from_path(&canonicalized, &mut files, &mut errors);
    }

    (files, errors)
}

/// Recursively collects Ruby files from a single path.
fn collect_from_path(path: &Path, files: &mut Vec<PathBuf>, errors: &mut Vec<Errors>) {
    if path.is_file() {
        if path.extension().is_some_and(|ext| ext == "rb") {
            files.push(path.to_path_buf());
        }
    } else if path.is_dir() {
        let Ok(read_dir) = path.read_dir() else {
            errors.push(Errors::FileError(format!(
                "Failed to read directory `{}`",
                path.display(),
            )));
            return;
        };

        for result in read_dir {
            let Ok(entry) = result else {
                errors.push(Errors::FileError(format!(
                    "Failed to read directory entry in `{}`: {result:?}",
                    path.display(),
                )));
                continue;
            };

            let entry_path = entry.path();
            let Ok(kind) = entry.file_type() else {
                errors.push(Errors::FileError(format!(
                    "Failed to get file type for `{}`",
                    entry_path.display(),
                )));
                continue;
            };

            if kind.is_dir() {
                collect_from_path(&entry_path, files, errors);
            } else if kind.is_file() {
                if entry_path.extension().is_some_and(|ext| ext == "rb") {
                    files.push(entry_path);
                }
            } else if kind.is_symlink() {
                if let Ok(resolved) = fs::canonicalize(&entry_path) {
                    collect_from_path(&resolved, files, errors);
                } else {
                    errors.push(Errors::FileError(format!(
                        "Failed to resolve symlink `{}`",
                        entry_path.display(),
                    )));
                }
            }
        }
    } else if path.is_symlink() {
        if let Ok(resolved) = fs::canonicalize(path) {
            collect_from_path(&resolved, files, errors);
        } else {
            errors.push(Errors::FileError(format!(
                "Failed to resolve symlink `{}`",
                path.display(),
            )));
        }
    } else {
        errors.push(Errors::FileError(format!(
            "Path `{}` is not a file or directory",
            path.display()
        )));
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_utils::Context;

    fn collect_document_paths(context: &Context, paths: &[&str]) -> (Vec<String>, Vec<Errors>) {
        let (files, errors) = collect_file_paths(
            paths
                .iter()
                .map(|p| context.absolute_path_to(p).to_string_lossy().into_owned())
                .collect(),
        );

        let mut files: Vec<String> = files
            .iter()
            .map(|path| context.relative_path_to(path).to_string_lossy().into_owned())
            .collect();

        files.sort();

        (files, errors)
    }

    #[test]
    fn collect_all_documents() {
        let context = Context::new();
        let baz = PathBuf::from("bar").join("baz.rb");
        let qux = PathBuf::from("bar").join("qux.rb");
        let bar = PathBuf::from("foo").join("bar.rb");
        context.touch(&baz);
        context.touch(&qux);
        context.touch(&bar);

        let (files, errors) = collect_document_paths(&context, &["foo", "bar"]);

        assert!(errors.is_empty());

        assert_eq!(
            files,
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
        let baz = PathBuf::from("bar").join("baz.rb");
        let qux = PathBuf::from("bar").join("qux.rb");
        let bar = PathBuf::from("foo").join("bar.rb");

        context.touch(&baz);
        context.touch(&qux);
        context.touch(&bar);

        let (files, errors) = collect_document_paths(&context, &["bar"]);

        assert!(errors.is_empty());

        assert_eq!(
            files,
            vec![baz.to_str().unwrap().to_string(), qux.to_str().unwrap().to_string()]
        );
    }

    #[test]
    fn collect_non_existing_paths() {
        let context = Context::new();

        let (files, errors) = collect_file_paths(vec![
            context
                .absolute_path_to("non_existing_path")
                .to_string_lossy()
                .into_owned(),
        ]);

        assert!(files.is_empty());

        assert_eq!(
            errors,
            vec![Errors::FileError(format!(
                "Path `{}` does not exist",
                context.absolute_path_to("non_existing_path").display()
            ))]
        );
    }
}
