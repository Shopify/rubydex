use std::fs;
use std::io;
use std::path::{Path, PathBuf};

/// Resolves `path` into the spelling rubydex knows a path by: absolute, with symlinks expanded and, on Windows, spelled
/// plainly instead of verbatim.
///
/// # Errors
///
/// Returns an error if the path cannot be canonicalized, as for one that does not exist.
pub(crate) fn resolved(path: &Path) -> io::Result<PathBuf> {
    Ok(simplified(fs::canonicalize(path)?))
}

/// Only Windows spells a path verbatim, so everywhere else there is nothing to simplify.
#[cfg(not(windows))]
pub(crate) fn simplified(path: PathBuf) -> PathBuf {
    path
}

/// Since we canonicalize paths, we must simplify Windows paths in order to use them correctly with glob patterns, like
/// stripping the `\\?\` prefix.
#[cfg(windows)]
pub(crate) fn simplified(path: PathBuf) -> PathBuf {
    use std::path::Component;
    use std::path::Prefix;

    let mut components = path.components();
    let Some(Component::Prefix(prefix)) = components.next() else {
        return path;
    };

    // Only a verbatim drive or share has a plain equivalent: `\\?\D:\dir` is `D:\dir`, and `\\?\UNC\server\share` is
    // the share `\\server\share`. Any other verbatim prefix names a device, such as `\\?\pipe\name`, which has none.
    let root = match prefix.kind() {
        Prefix::VerbatimDisk(drive) => format!("{}:\\", char::from(drive)),
        Prefix::VerbatimUNC(server, share) => {
            format!(r"\\{}\{}\", server.to_string_lossy(), share.to_string_lossy())
        }
        _ => return path,
    };

    // The root already carries the separator that follows it, so the one spelled as its own component is dropped rather
    // than pushed, which would otherwise reset the path back to the root it was just given.
    let mut simplified = PathBuf::from(root);
    simplified.extend(components.filter(|component| !matches!(component, Component::RootDir)));
    simplified
}

#[cfg(all(test, windows))]
mod tests {
    use super::*;

    #[test]
    fn simplified_rewrites_a_verbatim_path_into_its_plain_spelling() {
        assert_eq!(
            simplified(PathBuf::from(r"\\?\D:\a\project")),
            PathBuf::from(r"D:\a\project")
        );
    }

    #[test]
    fn simplified_keeps_a_verbatim_share_rooted_on_that_share() {
        assert_eq!(
            simplified(PathBuf::from(r"\\?\UNC\server\share\project")),
            PathBuf::from(r"\\server\share\project")
        );
    }

    #[test]
    fn simplified_leaves_a_path_with_no_plain_equivalent_alone() {
        // A device path is not addressable without the prefix, and a plain path has nothing to simplify.
        for path in [r"\\?\pipe\rubydex", r"D:\a\project", r"\\server\share\project"] {
            assert_eq!(simplified(PathBuf::from(path)), PathBuf::from(path));
        }
    }
}
