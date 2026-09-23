use crate::diagnostic_api::DiagnosticSeverity;
use crate::graph_api::{GraphPointer, with_graph};
use crate::utils;
use libc::{c_char, c_void};
use rubydex::config::{Config, Rule};
use rubydex::errors::Errors;
use std::ffi::CString;
use std::path::Path;
use std::ptr;

/// An opaque pointer to a loaded configuration
pub type ConfigPointer = *mut c_void;

/// The result of loading a configuration file, carrying either a parsed configuration or an error message.
#[repr(C)]
pub struct CConfigResult {
    /// Non-null on success: a heap-allocated parsed configuration. Free with `rdx_config_free`.
    pub config: ConfigPointer,
    /// Non-null on error; null on success. Caller must free with `free_c_string`.
    pub error: *const c_char,
}

impl CConfigResult {
    fn success(config: Config) -> Self {
        Self {
            config: Box::into_raw(Box::new(config)).cast::<c_void>(),
            error: ptr::null(),
        }
    }

    fn error(message: &str) -> Self {
        // Parse errors quote raw file content, which may contain NUL bytes that a C string cannot carry.
        let message = message.replace('\0', "\u{FFFD}");

        Self {
            config: ptr::null_mut(),
            error: utils::cstring_raw(&message),
        }
    }
}

/// Loads the configuration of the workspace rooted at `workspace_path`, which is where its `rubydex.toml` is expected
/// to be. A workspace without a configuration file produces an empty configuration.
///
/// # Safety
///
/// - `workspace_path` must be a valid, null-terminated string.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn rdx_config_load(workspace_path: *const c_char) -> CConfigResult {
    if workspace_path.is_null() {
        return CConfigResult::error("workspace path is required");
    }

    let Ok(workspace_path) = (unsafe { utils::convert_char_ptr_to_string(workspace_path) }) else {
        return CConfigResult::error("workspace path is not valid UTF-8");
    };

    match Config::load(Path::new(&workspace_path)) {
        Ok(config) => CConfigResult::success(config),
        Err(Errors::ConfigError(message) | Errors::FileError(message)) => CConfigResult::error(&message),
    }
}

/// Returns the root directory of the workspace the configuration was loaded for, as a C string. Caller must free with
/// `free_c_string`.
///
/// # Safety
///
/// - `config` must be a valid `ConfigPointer` previously returned by `rdx_config_load`.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn rdx_config_workspace_path(config: ConfigPointer) -> *const c_char {
    let config = unsafe { &*config.cast::<Config>() };

    CString::new(utils::interop_path(config.workspace_path()))
        .map_or(ptr::null(), |c_string| c_string.into_raw().cast_const())
}

/// Frees a configuration through its pointer. Does nothing when given NULL.
///
/// # Safety
///
/// - `config` must either be NULL or a valid `ConfigPointer` previously returned by `rdx_config_load` and must not
///   be used afterwards. Any string pointer borrowed from it becomes invalid.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn rdx_config_free(config: ConfigPointer) {
    if config.is_null() {
        return;
    }

    unsafe {
        let _ = Box::from_raw(config.cast::<Config>());
    }
}

/// Length-delimited string bytes exposed while building Ruby configuration values. Ownership is determined by the
/// containing array or rule.
#[repr(C)]
#[derive(Debug)]
pub struct CConfigString {
    pub data: *const c_char,
    pub length: usize,
}

/// C-compatible array that owns both its string bytes and the string descriptors.
#[repr(C)]
pub struct CConfigStringArray {
    pub items: *mut CConfigString,
    pub len: usize,
}

impl CConfigStringArray {
    fn new(strings: &[Box<str>]) -> Self {
        if strings.is_empty() {
            return Self {
                items: ptr::null_mut(),
                len: 0,
            };
        }

        let items = strings
            .iter()
            .map(|string| {
                let bytes = string.as_bytes().to_vec().into_boxed_slice();

                CConfigString {
                    length: bytes.len(),
                    data: Box::into_raw(bytes).cast::<c_char>().cast_const(),
                }
            })
            .collect::<Box<[CConfigString]>>();

        Self {
            len: items.len(),
            items: Box::into_raw(items).cast::<CConfigString>(),
        }
    }
}

/// Returns an owned snapshot of the dead-code report's exclusion patterns. Free it with `rdx_config_string_array_free`.
///
/// # Safety
///
/// - `config` must be a valid `ConfigPointer` previously returned by `rdx_config_load`.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn rdx_config_dead_code_exclude_patterns(config: ConfigPointer) -> CConfigStringArray {
    let config = unsafe { &*config.cast::<Config>() };
    CConfigStringArray::new(config.dead_code().exclude_patterns())
}

/// Returns an owned snapshot of the exclusion patterns from the graph's loaded dead-code configuration. Free it with
/// `rdx_config_string_array_free`. The snapshot remains valid if the graph's configuration changes.
///
/// # Safety
///
/// - `graph` must be a valid `GraphPointer` previously returned by `rdx_graph_new`.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn rdx_graph_dead_code_exclude_patterns(graph: GraphPointer) -> CConfigStringArray {
    with_graph(graph, |graph| {
        CConfigStringArray::new(graph.config().dead_code().exclude_patterns())
    })
}

/// Frees both the string bytes and descriptors of an owned configuration string array.
///
/// # Safety
///
/// - `strings` must have been returned by `rdx_config_dead_code_exclude_patterns` or
///   `rdx_graph_dead_code_exclude_patterns`, and must not be used afterwards.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn rdx_config_string_array_free(strings: CConfigStringArray) {
    if strings.items.is_null() {
        return;
    }

    unsafe {
        let strings = Box::from_raw(ptr::slice_from_raw_parts_mut(strings.items, strings.len));

        for string in &*strings {
            let bytes = ptr::slice_from_raw_parts_mut(string.data.cast_mut().cast::<u8>(), string.length);
            let _ = Box::from_raw(bytes);
        }
    }
}

/// C-compatible struct representing a single configured linter rule.
#[repr(C)]
#[derive(Debug)]
pub struct CLinterRule {
    pub name: *const c_char,
    pub name_length: usize,
    pub enabled: bool,
    pub exclude_patterns: *const CConfigString,
    pub exclude_patterns_length: usize,
    pub severity: *const DiagnosticSeverity,
}

impl From<&Rule> for CLinterRule {
    fn from(rule: &Rule) -> Self {
        let exclude_patterns = rule
            .exclude_patterns()
            .iter()
            .map(|pattern| CConfigString {
                data: pattern.as_ptr().cast::<c_char>(),
                length: pattern.len(),
            })
            .collect::<Box<[CConfigString]>>();
        let exclude_patterns_length = exclude_patterns.len();
        let exclude_patterns = if exclude_patterns.is_empty() {
            ptr::null()
        } else {
            Box::into_raw(exclude_patterns).cast::<CConfigString>().cast_const()
        };
        let severity = rule.severity().map_or(ptr::null(), |severity| {
            Box::into_raw(Box::new(DiagnosticSeverity::from(*severity))).cast_const()
        });

        Self {
            name: rule.name().as_ptr().cast::<c_char>(),
            name_length: rule.name().len(),
            enabled: rule.enabled(),
            exclude_patterns,
            exclude_patterns_length,
            severity,
        }
    }
}

/// C-compatible array of configured linter rules.
#[repr(C)]
pub struct CLinterRuleArray {
    pub items: *mut CLinterRule,
    pub len: usize,
}

/// Returns the configured linter rules as an array. Caller must free it with `rdx_config_linter_rules_free`, while the
/// configuration is still alive, since the rule names are borrowed from it.
///
/// # Safety
///
/// - `config` must be a valid `ConfigPointer` previously returned by `rdx_config_load`.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn rdx_config_linter_rules(config: ConfigPointer) -> CLinterRuleArray {
    let config = unsafe { &*config.cast::<Config>() };
    let rules = config.linter().rules();

    if rules.is_empty() {
        return CLinterRuleArray {
            items: ptr::null_mut(),
            len: 0,
        };
    }

    let items = rules.iter().map(CLinterRule::from).collect::<Box<[CLinterRule]>>();

    CLinterRuleArray {
        len: items.len(),
        items: Box::into_raw(items).cast::<CLinterRule>(),
    }
}

/// Frees an array previously returned by `rdx_config_linter_rules`. The rule names it borrowed are left alone, since
/// they belong to the configuration.
///
/// # Safety
///
/// - `rules` must have been returned by `rdx_config_linter_rules` and must not be used afterwards.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn rdx_config_linter_rules_free(rules: CLinterRuleArray) {
    if rules.items.is_null() {
        return;
    }

    unsafe {
        let rules = Box::from_raw(ptr::slice_from_raw_parts_mut(rules.items, rules.len));

        for rule in &*rules {
            if !rule.exclude_patterns.is_null() {
                let exclude_patterns =
                    ptr::slice_from_raw_parts_mut(rule.exclude_patterns.cast_mut(), rule.exclude_patterns_length);
                let _ = Box::from_raw(exclude_patterns);
            }

            if !rule.severity.is_null() {
                let _ = Box::from_raw(rule.severity.cast_mut());
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn config_string_array_owns_length_delimited_strings() {
        let patterns = [Box::from("app/generated/**"), Box::from("a\0b"), Box::from("")];
        let strings = CConfigStringArray::new(&patterns);

        assert_eq!(strings.len, patterns.len());
        let entries = unsafe { std::slice::from_raw_parts(strings.items, strings.len) };
        for (entry, pattern) in entries.iter().zip(&patterns) {
            assert_eq!(entry.length, pattern.len());
            if !pattern.is_empty() {
                assert_ne!(entry.data.cast::<u8>(), pattern.as_ptr());
            }
        }

        drop(patterns);

        for (entry, expected) in entries.iter().zip(["app/generated/**", "a\0b", ""]) {
            let bytes = unsafe { std::slice::from_raw_parts(entry.data.cast::<u8>(), entry.length) };
            assert_eq!(bytes, expected.as_bytes());
        }

        unsafe { rdx_config_string_array_free(strings) };
    }

    #[test]
    fn config_string_array_accepts_empty_patterns() {
        let strings = CConfigStringArray::new(&[]);

        assert_eq!(strings.len, 0);
        assert!(strings.items.is_null());

        unsafe { rdx_config_string_array_free(strings) };
    }
}
