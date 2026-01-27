use crate::{
    errors::Errors,
    job_queue::{Job, JobQueue},
};
use crossbeam_channel::{Sender, unbounded};
use std::{
    fs,
    path::{Path, PathBuf},
    sync::Arc,
};

pub struct FileDiscoveryJob {
    path: PathBuf,
    queue: Arc<JobQueue>,
    paths_tx: Sender<PathBuf>,
    errors_tx: Sender<Errors>,
}

impl FileDiscoveryJob {
    #[must_use]
    pub fn new(path: PathBuf, queue: Arc<JobQueue>, paths_tx: Sender<PathBuf>, errors_tx: Sender<Errors>) -> Self {
        Self {
            path,
            queue,
            paths_tx,
            errors_tx,
        }
    }
}

impl FileDiscoveryJob {
    fn handle_file(&self, path: &Path) {
        if path.extension().is_some_and(|ext| ext == "rb") {
            self.paths_tx
                .send(path.to_path_buf())
                .expect("file receiver dropped before run completion");
        }
    }

    fn handle_symlink(&self, path: &PathBuf) {
        let Ok(canonicalized) = fs::canonicalize(path) else {
            self.send_error(Errors::FileError(format!(
                "Failed to canonicalize symlink: `{}`",
                path.display(),
            )));

            return;
        };

        self.queue.push(Box::new(FileDiscoveryJob::new(
            canonicalized,
            Arc::clone(&self.queue),
            self.paths_tx.clone(),
            self.errors_tx.clone(),
        )));
    }

    fn send_error(&self, error: Errors) {
        self.errors_tx
            .send(error)
            .expect("error receiver dropped before run completion");
    }
}

impl Job for FileDiscoveryJob {
    fn run(&self) {
        if self.path.is_dir() {
            let Ok(read_dir) = self.path.read_dir() else {
                self.send_error(Errors::FileError(format!(
                    "Failed to read directory `{}`",
                    self.path.display(),
                )));

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

                let kind = entry.file_type().unwrap();

                if kind.is_dir() {
                    self.queue.push(Box::new(FileDiscoveryJob::new(
                        entry.path(),
                        Arc::clone(&self.queue),
                        self.paths_tx.clone(),
                        self.errors_tx.clone(),
                    )));
                } else if kind.is_file() {
                    self.handle_file(&entry.path());
                } else if kind.is_symlink() {
                    self.handle_symlink(&entry.path());
                } else {
                    self.send_error(Errors::FileError(format!(
                        "Path `{}` is not a file or directory",
                        entry.path().display()
                    )));
                }
            }
        } else if self.path.is_file() {
            self.handle_file(&self.path);
        } else if self.path.is_symlink() {
            self.handle_symlink(&self.path);
        } else {
            self.send_error(Errors::FileError(format!(
                "Path `{}` is not a file or directory",
                self.path.display()
            )));
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
pub fn collect_file_paths(paths: Vec<String>) -> (Vec<PathBuf>, Vec<Errors>) {
    let queue = Arc::new(JobQueue::new());
    let (files_tx, files_rx) = unbounded();
    let (errors_tx, errors_rx) = unbounded();

    for path in paths {
        let Ok(canonicalized) = fs::canonicalize(&path) else {
            errors_tx
                .send(Errors::FileError(format!("Path `{path}` does not exist")))
                .expect("errors receiver dropped before run completion");

            continue;
        };

        queue.push(Box::new(FileDiscoveryJob::new(
            canonicalized,
            Arc::clone(&queue),
            files_tx.clone(),
            errors_tx.clone(),
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
            [Errors::FileError(format!(
                "Path `{}` does not exist",
                context.absolute_path_to("non_existing_path").display()
            ))]
        );
    }
}
