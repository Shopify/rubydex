use crate::{
    errors::Errors,
    job_queue::{Job, JobQueue},
};
use crossbeam_channel::{Sender, unbounded};
use glob::Pattern;
use std::{
    collections::HashSet,
    hash::BuildHasher,
    path::{Path, PathBuf},
    sync::Arc,
};

pub struct FileDiscoveryJob {
    path: PathBuf,
    queue: Arc<JobQueue>,
    paths_tx: Sender<PathBuf>,
    errors_tx: Sender<Errors>,
    excluded_patterns: Arc<Vec<Pattern>>,
}

impl FileDiscoveryJob {
    #[must_use]
    pub fn new(
        path: PathBuf,
        queue: Arc<JobQueue>,
        paths_tx: Sender<PathBuf>,
        errors_tx: Sender<Errors>,
        excluded_patterns: Arc<Vec<Pattern>>,
    ) -> Self {
        Self {
            path,
            queue,
            paths_tx,
            errors_tx,
            excluded_patterns,
        }
    }
}

fn is_indexable_file(path: &Path) -> bool {
    path.extension()
        .is_some_and(|ext| ext == "rb" || ext == "rake" || ext == "rbs" || ext == "ru")
}

fn is_excluded(excluded_patterns: &[Pattern], path: &Path) -> bool {
    excluded_patterns.iter().any(|pattern| pattern.matches_path(path))
}

impl FileDiscoveryJob {
    fn handle_file(&self, path: &Path) {
        if is_indexable_file(path) {
            self.paths_tx
                .send(path.to_path_buf())
                .expect("file receiver dropped before run completion");
        }
    }

    fn send_error(&self, error: Errors) {
        self.errors_tx
            .send(error)
            .expect("error receiver dropped before run completion");
    }
}

impl Job for FileDiscoveryJob {
    fn run(&self) {
        let Ok(read_dir) = self.path.read_dir() else {
            if self.path.is_file() {
                self.handle_file(&self.path);
            } else {
                self.send_error(Errors::FileError(format!(
                    "Failed to read directory `{}`",
                    self.path.display()
                )));
            }

            return;
        };

        for result in read_dir {
            let Ok(entry) = result else {
                self.send_error(Errors::FileError(format!(
                    "Failed to read directory `{}`: {result:?}",
                    self.path.display(),
                )));

                continue;
            };

            let path = entry.path();

            if is_excluded(&self.excluded_patterns, &path) {
                continue;
            }

            if entry.file_type().unwrap().is_dir() {
                self.queue.push(Box::new(FileDiscoveryJob::new(
                    path,
                    Arc::clone(&self.queue),
                    self.paths_tx.clone(),
                    self.errors_tx.clone(),
                    Arc::clone(&self.excluded_patterns),
                )));
            } else {
                self.handle_file(&path);
            }
        }
    }
}

/// Recursively collects all Ruby files for the given workspace and dependencies, returning a vector of document instances
///
/// # Errors
///
/// Returns a `MultipleErrors` if any of the paths do not exist
///
/// # Panics
///
/// Panics if the errors receiver is dropped before the run completion
#[must_use]
pub fn collect_file_paths<S: BuildHasher>(
    paths: Vec<String>,
    excluded: &HashSet<Box<str>, S>,
) -> (Vec<PathBuf>, Vec<Errors>) {
    let queue = Arc::new(JobQueue::new());
    let (files_tx, files_rx) = unbounded();
    let (errors_tx, errors_rx) = unbounded();

    let excluded_patterns: Arc<Vec<Pattern>> =
        Arc::new(excluded.iter().filter_map(|entry| Pattern::new(entry).ok()).collect());

    for path in paths {
        let Ok(path) = std::path::absolute(&path) else {
            errors_tx
                .send(Errors::FileError(format!("Failed to resolve path `{path}`")))
                .expect("errors receiver dropped before run completion");

            continue;
        };

        if !path.exists() {
            errors_tx
                .send(Errors::FileError(format!("Path `{}` does not exist", path.display())))
                .expect("errors receiver dropped before run completion");

            continue;
        }

        if is_excluded(&excluded_patterns, &path) {
            continue;
        }

        queue.push(Box::new(FileDiscoveryJob::new(
            path,
            Arc::clone(&queue),
            files_tx.clone(),
            errors_tx.clone(),
            Arc::clone(&excluded_patterns),
        )));
    }

    JobQueue::run(&queue);

    drop(files_tx);
    drop(errors_tx);

    (files_rx.iter().collect(), errors_rx.iter().collect())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_utils::Context;

    fn collect_document_paths(context: &Context, paths: &[&str]) -> (Vec<String>, Vec<Errors>) {
        collect_document_paths_with_exclusions(context, paths, &HashSet::new())
    }

    fn collect_document_paths_with_exclusions(
        context: &Context,
        paths: &[&str],
        excluded: &HashSet<Box<str>>,
    ) -> (Vec<String>, Vec<Errors>) {
        let (files, errors) = collect_file_paths(
            paths
                .iter()
                .map(|p| context.absolute_path_to(p).to_string_lossy().into_owned())
                .collect(),
            excluded,
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
            [
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
            [baz.to_str().unwrap().to_string(), qux.to_str().unwrap().to_string()]
        );
    }

    #[test]
    fn collect_indexable_files() {
        let context = Context::new();
        let ruby_file = PathBuf::from("lib").join("foo.rb");
        let rake_file = PathBuf::from("lib").join("task.rake");
        let rbs_file = PathBuf::from("sig").join("foo.rbs");
        let rack_file = PathBuf::from("config.ru");
        let txt_file = PathBuf::from("lib").join("notes.txt");
        context.touch(&ruby_file);
        context.touch(&rake_file);
        context.touch(&rbs_file);
        context.touch(&rack_file);
        context.touch(&txt_file);

        let (files, errors) = collect_document_paths(&context, &["lib", "sig", "config.ru"]);

        assert!(errors.is_empty());

        assert_eq!(
            [
                rack_file.to_str().unwrap().to_string(),
                ruby_file.to_str().unwrap().to_string(),
                rake_file.to_str().unwrap().to_string(),
                rbs_file.to_str().unwrap().to_string(),
            ],
            files.as_slice()
        );
    }

    #[test]
    fn collect_non_existing_paths() {
        let context = Context::new();

        let (files, errors) = collect_file_paths(
            vec![
                context
                    .absolute_path_to("non_existing_path")
                    .to_string_lossy()
                    .into_owned(),
            ],
            &HashSet::new(),
        );

        assert!(files.is_empty());

        assert_eq!(
            errors,
            [Errors::FileError(format!(
                "Path `{}` does not exist",
                context.absolute_path_to("non_existing_path").display()
            ))]
        );
    }

    #[cfg(unix)]
    #[test]
    fn collect_files_emits_absolute_paths_for_relative_roots() {
        let context = Context::new();
        context.touch(PathBuf::from("project").join("foo.rb"));

        // Express the project directory as a path relative to the process working directory.
        let working_directory = std::env::current_dir().unwrap();
        let mut relative_root = PathBuf::new();
        for _ in 0..working_directory.components().count() - 1 {
            relative_root.push("..");
        }
        let project = context.absolute_path_to("project");
        let relative_root = relative_root.join(project.strip_prefix("/").unwrap());

        let (files, errors) = collect_file_paths(vec![relative_root.to_string_lossy().into_owned()], &HashSet::new());

        assert!(errors.is_empty());
        assert!(!files.is_empty());
        assert!(
            files.iter().all(|path| path.is_absolute()),
            "expected only absolute paths, got {files:?}"
        );
    }

    #[test]
    fn collect_files_excludes_directories() {
        let context = Context::new();
        let included = PathBuf::from("included").join("foo.rb");
        let excluded_file = PathBuf::from("excluded").join("bar.rb");
        context.touch(&included);
        context.touch(&excluded_file);

        let mut excluded = HashSet::new();
        excluded.insert(context.absolute_path_to("excluded").to_string_lossy().into());

        let (files, errors) = collect_document_paths_with_exclusions(&context, &["included", "excluded"], &excluded);

        assert!(errors.is_empty());
        assert_eq!(files, [included.to_str().unwrap().to_string()]);
    }

    #[test]
    fn collect_files_excludes_nested_directories() {
        let context = Context::new();
        let kept = PathBuf::from("root").join("kept.rb");
        let nested = PathBuf::from("root").join("skip").join("nested.rb");
        context.touch(&kept);
        context.touch(&nested);

        let mut excluded = HashSet::new();
        excluded.insert("**/skip".into());

        let (files, errors) = collect_document_paths_with_exclusions(&context, &["root"], &excluded);

        assert!(errors.is_empty());
        assert_eq!(files, [kept.to_str().unwrap().to_string()]);
    }

    #[cfg(unix)]
    #[test]
    fn collect_files_indexes_symlinked_files_at_their_own_path() {
        let context = Context::new();
        let target = PathBuf::from("outside").join("real.rb");
        context.touch(&target);
        context.mkdir("project");

        // Create a symlink to a file outside the traversed tree: project/alias.rb -> outside/real.rb
        std::os::unix::fs::symlink(
            context.absolute_path_to("outside/real.rb"),
            context.absolute_path_to("project/alias.rb"),
        )
        .unwrap();

        let (files, errors) = collect_document_paths(&context, &["project"]);

        assert!(errors.is_empty());
        // The symlink is indexed at its own path, not resolved to the target.
        let alias = PathBuf::from("project").join("alias.rb");
        assert_eq!(files, [alias.to_str().unwrap().to_string()]);
    }

    #[cfg(unix)]
    #[test]
    fn collect_files_does_not_follow_symlinked_directories() {
        let context = Context::new();
        let kept = PathBuf::from("project").join("foo.rb");
        let outside = PathBuf::from("outside").join("bar.rb");
        context.touch(&kept);
        context.touch(&outside);

        // Create a symlink inside the traversed tree: project/link -> outside
        std::os::unix::fs::symlink(
            context.absolute_path_to("outside"),
            context.absolute_path_to("project/link"),
        )
        .unwrap();

        let (files, errors) = collect_document_paths(&context, &["project"]);

        assert!(errors.is_empty());
        // The symlinked directory is not followed, so `outside/bar.rb` is never reached.
        assert_eq!(files, [kept.to_str().unwrap().to_string()]);
    }

    #[cfg(unix)]
    #[test]
    fn collect_files_indexes_symlinked_directory_roots() {
        let context = Context::new();
        let target = PathBuf::from("real").join("foo.rb");
        context.touch(&target);

        // A symlink to a directory passed as an explicit root, as `Graph#workspace_paths` does via `File.directory?`.
        std::os::unix::fs::symlink(context.absolute_path_to("real"), context.absolute_path_to("link")).unwrap();

        let (files, errors) = collect_document_paths(&context, &["link"]);

        assert!(errors.is_empty());
        // The requested root is traversed; files are indexed under the requested (symlink) path.
        let foo = PathBuf::from("link").join("foo.rb");
        assert_eq!(files, [foo.to_str().unwrap().to_string()]);
    }

    #[test]
    fn collect_files_excludes_nested_files_matching_globs() {
        let context = Context::new();
        let kept = PathBuf::from("lib").join("foo.rb");
        let excluded_file = PathBuf::from("lib").join("version.rb");
        context.touch(&kept);
        context.touch(&excluded_file);

        let mut excluded = HashSet::new();
        excluded.insert("**/version.rb".into());

        let (files, errors) = collect_document_paths_with_exclusions(&context, &["lib"], &excluded);

        assert!(errors.is_empty());
        assert_eq!(files, [kept.to_str().unwrap().to_string()]);
    }
}
