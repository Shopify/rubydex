use crate::utils;
use libc::{c_char, c_void};
use rubydex::config::Config;
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

    CString::new(config.workspace_path().to_string_lossy().as_ref())
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
