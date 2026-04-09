use crate::errors::Errors;
use crossbeam_deque::{Injector, Steal, Stealer, Worker};
use crossbeam_utils::Backoff;
use std::{
    collections::HashSet,
    ffi::OsStr,
    fs,
    hash::BuildHasher,
    path::{Path, PathBuf},
    sync::atomic::{AtomicUsize, Ordering},
    thread,
};

/// Check if a filename has a Ruby extension without allocating a PathBuf.
fn has_ruby_extension(name: &OsStr) -> bool {
    let bytes = name.as_encoded_bytes();
    bytes.ends_with(b".rb") || bytes.ends_with(b".rbs")
}

/// Try to steal a path from local queue, global injector, or peer workers.
fn steal_path(
    local: &Worker<PathBuf>,
    injector: &Injector<PathBuf>,
    stealers: &[Stealer<PathBuf>],
) -> Option<PathBuf> {
    if let Some(path) = local.pop() {
        return Some(path);
    }

    match injector.steal_batch_and_pop(local) {
        Steal::Success(path) => return Some(path),
        Steal::Retry => return None,
        Steal::Empty => {}
    }

    for stealer in stealers {
        match stealer.steal_batch_and_pop(local) {
            Steal::Success(path) => return Some(path),
            Steal::Retry => return None,
            Steal::Empty => {}
        }
    }

    None
}

/// Process a single directory, pushing subdirectories to the worker's local queue for better locality.
fn process_directory(
    path: &Path,
    local: &Worker<PathBuf>,
    in_flight: &AtomicUsize,
    excluded: &HashSet<PathBuf>,
    files: &mut Vec<PathBuf>,
    errors: &mut Vec<Errors>,
) {
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
                "Failed to read directory `{}`: {result:?}",
                path.display(),
            )));
            continue;
        };

        let kind = entry.file_type().unwrap();

        if kind.is_dir() {
            let p = entry.path();
            if !excluded.contains(&p) {
                in_flight.fetch_add(1, Ordering::Relaxed);
                local.push(p);
            }
        } else if kind.is_file() {
            if has_ruby_extension(&entry.file_name()) {
                files.push(entry.path());
            }
        } else if kind.is_symlink() {
            let entry_path = entry.path();
            let Ok(canonicalized) = fs::canonicalize(&entry_path) else {
                errors.push(Errors::FileError(format!(
                    "Failed to canonicalize symlink: `{}`",
                    entry_path.display(),
                )));
                continue;
            };

            if !excluded.contains(&canonicalized) {
                in_flight.fetch_add(1, Ordering::Relaxed);
                local.push(canonicalized);
            }
        } else {
            errors.push(Errors::FileError(format!(
                "Path `{}` is not a file or directory",
                entry.path().display()
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
pub fn collect_file_paths<S: BuildHasher>(
    paths: Vec<String>,
    excluded: &HashSet<PathBuf, S>,
) -> (Vec<PathBuf>, Vec<Errors>) {
    let excluded: HashSet<PathBuf> = excluded
        .iter()
        .filter_map(|p| fs::canonicalize(p).ok())
        .collect();

    let injector: Injector<PathBuf> = Injector::new();
    let in_flight = AtomicUsize::new(0);
    let mut initial_errors = Vec::new();

    for path in paths {
        let Ok(canonicalized) = fs::canonicalize(&path) else {
            initial_errors.push(Errors::FileError(format!("Path `{path}` does not exist")));
            continue;
        };

        if excluded.contains(&canonicalized) {
            continue;
        }

        in_flight.fetch_add(1, Ordering::Relaxed);
        injector.push(canonicalized);
    }

    let worker_count = thread::available_parallelism()
        .map(|n| n.get() / 2)
        .unwrap_or(4)
        .max(2);

    let mut all_files = Vec::new();
    let mut all_errors = initial_errors;

    let mut workers = Vec::with_capacity(worker_count);
    let mut stealers = Vec::with_capacity(worker_count);

    for _ in 0..worker_count {
        let worker = Worker::new_lifo();
        stealers.push(worker.stealer());
        workers.push(worker);
    }

    thread::scope(|s| {
        let stealers = &stealers[..];
        let injector = &injector;
        let in_flight = &in_flight;
        let excluded = &excluded;
        let mut handles = Vec::with_capacity(worker_count);

        for worker in workers {
            handles.push(s.spawn(move || {
                let mut local_files = Vec::new();
                let mut local_errors = Vec::new();
                let backoff = Backoff::new();

                loop {
                    let Some(path) = steal_path(&worker, &injector, stealers) else {
                        if in_flight.load(Ordering::Acquire) == 0 {
                            break;
                        }
                        backoff.snooze();
                        continue;
                    };

                    backoff.reset();

                    if path.is_dir() {
                        process_directory(
                            &path,
                            &worker,
                            &in_flight,
                            &excluded,
                            &mut local_files,
                            &mut local_errors,
                        );
                    } else if path.is_file() {
                        if path.extension().is_some_and(|ext| ext == "rb" || ext == "rbs") {
                            local_files.push(path.clone());
                        }
                    } else if path.is_symlink() {
                        if let Ok(canonicalized) = fs::canonicalize(&path) {
                            if !excluded.contains(&canonicalized) {
                                in_flight.fetch_add(1, Ordering::Relaxed);
                                worker.push(canonicalized);
                            }
                        } else {
                            local_errors.push(Errors::FileError(format!(
                                "Failed to canonicalize symlink: `{}`",
                                path.display(),
                            )));
                        }
                    } else {
                        local_errors.push(Errors::FileError(format!(
                            "Path `{}` is not a file or directory",
                            path.display()
                        )));
                    }

                    in_flight.fetch_sub(1, Ordering::Release);
                }

                (local_files, local_errors)
            }));
        }

        for handle in handles {
            let (files, errors) = handle.join().unwrap();
            all_files.extend(files);
            all_errors.extend(errors);
        }
    });

    (all_files, all_errors)
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
        excluded: &HashSet<PathBuf>,
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
    fn collect_rbs_files() {
        let context = Context::new();
        let ruby_file = PathBuf::from("lib").join("foo.rb");
        let rbs_file = PathBuf::from("sig").join("foo.rbs");
        let txt_file = PathBuf::from("lib").join("notes.txt");
        context.touch(&ruby_file);
        context.touch(&rbs_file);
        context.touch(&txt_file);

        let (files, errors) = collect_document_paths(&context, &["lib", "sig"]);

        assert!(errors.is_empty());

        assert_eq!(
            [
                ruby_file.to_str().unwrap().to_string(),
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

    #[test]
    fn collect_files_excludes_directories() {
        let context = Context::new();
        let included = PathBuf::from("included").join("foo.rb");
        let excluded_file = PathBuf::from("excluded").join("bar.rb");
        context.touch(&included);
        context.touch(&excluded_file);

        let mut excluded = HashSet::new();
        excluded.insert(context.absolute_path_to("excluded"));

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
        excluded.insert(context.absolute_path_to("root/skip"));

        let (files, errors) = collect_document_paths_with_exclusions(&context, &["root"], &excluded);

        assert!(errors.is_empty());
        assert_eq!(files, [kept.to_str().unwrap().to_string()]);
    }

    #[cfg(unix)]
    #[test]
    fn collect_files_excludes_symlinked_directories() {
        let context = Context::new();
        let included = PathBuf::from("included").join("foo.rb");
        let excluded_file = PathBuf::from("real_dir").join("bar.rb");
        context.touch(&included);
        context.touch(&excluded_file);

        // Create a symlink: link -> real_dir
        std::os::unix::fs::symlink(context.absolute_path_to("real_dir"), context.absolute_path_to("link")).unwrap();

        // Excluding the real directory while requesting to index the symlink should properly exclude the link
        let mut excluded = HashSet::new();
        excluded.insert(context.absolute_path_to("real_dir"));

        let (files, errors) = collect_document_paths_with_exclusions(&context, &["included", "link"], &excluded);

        assert!(errors.is_empty());
        assert_eq!(files, [included.to_str().unwrap().to_string()]);
    }
}
