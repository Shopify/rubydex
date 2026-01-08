use super::normalize_indentation;
use std::fs;
use std::path::{Path, PathBuf};
use tempfile::TempDir;

#[derive(Debug)]
pub struct Context {
    _root: TempDir,
    absolute_path: PathBuf,
}

/// Executes a closure with a new temporary context, ensuring cleanup afterwards.
///
/// # Examples
///
/// ```
/// use saturn::test_utils::with_context;
///
/// with_context(|context| {
///     context.touch("foo.rb");
/// });
/// ```
pub fn with_context<F, R>(f: F) -> R
where
    F: FnOnce(&Context) -> R,
{
    let context = Context::new();
    f(&context)
}

impl Context {
    /// Creates a new test context in a temporary directory
    ///
    /// # Panics
    ///
    /// Panics if the temp directory cannot be created.
    #[must_use]
    pub fn new() -> Self {
        let root = tempfile::tempdir().expect("failed to create temp dir");
        let absolute_path = fs::canonicalize(root.path()).unwrap();
        Self {
            _root: root,
            absolute_path,
        }
    }

    /// Returns the absolute path to the temp directory as a `PathBuf`
    ///
    /// # Panics
    ///
    /// Panics if the path cannot be canonicalized.
    #[must_use]
    pub fn absolute_path(&self) -> PathBuf {
        self.absolute_path.clone()
    }

    /// Returns the absolute path to the relative path
    ///
    /// # Panics
    ///
    /// Panics if the path cannot be canonicalized.
    #[must_use]
    pub fn absolute_path_to(&self, relative: &str) -> PathBuf {
        self.absolute_path.join(relative)
    }

    /// Returns the path of `absolute` relative to the context root.
    ///
    /// # Panics
    ///
    /// Panics if the provided path cannot be canonicalized or is not under the root.
    #[must_use]
    pub fn relative_path_to<P: AsRef<Path>>(&self, absolute: P) -> PathBuf {
        absolute
            .as_ref()
            .strip_prefix(self.absolute_path())
            .unwrap()
            .to_path_buf()
    }

    /// Create a directory (and parents) relative to the root
    ///
    /// # Panics
    ///
    /// Panics if the directory cannot be created.
    pub fn mkdir<P: AsRef<Path>>(&self, relative: P) {
        let dir = self.absolute_path().join(relative);
        fs::create_dir_all(dir).unwrap();
    }

    /// Touch a file relative to the root, creating parent directories as needed
    ///
    /// # Panics
    ///
    /// Panics if the file cannot be created.
    pub fn touch<P: AsRef<Path>>(&self, relative: P) {
        self.write(relative, "");
    }

    /// Read a file relative to the root
    ///
    /// # Panics
    ///
    /// Panics if the file cannot be read.
    #[must_use]
    pub fn read<P: AsRef<Path>>(&self, relative: P) -> String {
        fs::read_to_string(self.absolute_path().join(relative)).unwrap()
    }

    /// Write a file relative to the root, creating parent directories as needed
    ///
    /// # Panics
    ///
    /// Panics if the file cannot be created.
    pub fn write<P: AsRef<Path>>(&self, relative: P, content: &str) {
        let path = self.absolute_path().join(relative);
        if let Some(parent) = path.parent() {
            fs::create_dir_all(parent).unwrap();
        }
        let content = normalize_indentation(content);
        fs::write(path, content).unwrap();
    }
}

impl Default for Context {
    fn default() -> Self {
        Self::new()
    }
}

// no local normalize_indentation; shared via super::normalize_indentation

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn creates_and_cleans_up_temp_dir() {
        let context = Context::new();
        let root = context.absolute_path();

        assert!(root.exists());

        drop(context);

        // After drop, the directory should not exist anymore
        assert!(!root.exists());
    }

    #[test]
    fn mkdir_creates_directories() {
        let context = Context::new();

        assert!(!context.absolute_path_to("foo").exists());
        context.mkdir("foo");
        assert!(context.absolute_path_to("foo").exists());

        assert!(!context.absolute_path_to("bar/baz").exists());
        context.mkdir("bar/baz");
        assert!(context.absolute_path_to("bar/baz").exists());
    }

    #[test]
    fn touch_creates_files() {
        let context = Context::new();

        assert!(!context.absolute_path_to("foo/bar.rb").exists());
        context.touch("foo/bar.rb");
        assert!(context.absolute_path_to("foo/bar.rb").exists());

        assert!(!context.absolute_path_to("baz/qux.rb").exists());
        context.touch("baz/qux.rb");
        assert!(context.absolute_path_to("baz/qux.rb").exists());
    }

    #[test]
    fn write_creates_files_with_content() {
        let context = Context::new();

        context.write("foo/bar.rb", "class Foo; end\n");
        assert_eq!(context.read("foo/bar.rb"), "class Foo; end\n");

        context.write("baz/qux.rb", "class Baz; end\n");
        assert_eq!(context.read("baz/qux.rb"), "class Baz; end\n");
    }

    #[test]
    fn write_creates_files_with_content_and_normalizes_indentation() {
        let context = Context::new();

        context.write("foo/bar.rb", {
            "
            class Foo
              def bar
                puts 'baz'
              end
            end
            "
        });

        assert_eq!(
            context.read("foo/bar.rb"),
            normalize_indentation({
                "
                class Foo
                  def bar
                    puts 'baz'
                  end
                end
                "
            }),
        );
    }

    #[test]
    fn with_context_creates_and_cleans_up_temp_dir() {
        let root = with_context(|context| {
            let root = context.absolute_path();
            assert!(root.exists());
            context.touch("foo.rb");
            root
        });

        assert!(!root.exists());
    }
}
