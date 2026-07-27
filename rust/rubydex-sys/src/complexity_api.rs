//! C API for the complexity report.

use crate::utils;
use libc::{c_char, size_t};
use rubydex::complexity::{self, Report};
use std::ffi::CString;
use std::ptr;

/// The result of a complexity analysis or diff, carrying either the formatted output or an error
/// message. Exactly one of `output`/`error` is non-null. The caller must free the non-null pointer
/// with `free_c_string`.
#[repr(C)]
pub struct CComplexityResult {
    /// Non-null on success; null on error. Caller must free with `free_c_string`.
    pub output: *const c_char,
    /// Non-null on error; null on success. Caller must free with `free_c_string`.
    pub error: *const c_char,
    /// Non-null when the analysis succeeded with non-fatal warnings (e.g. unreadable files);
    /// null otherwise. Caller must free with `free_c_string`.
    pub warnings: *const c_char,
}

impl CComplexityResult {
    fn success(output: &str, warnings: &str) -> Self {
        let output_ptr = match CString::new(output) {
            Ok(c_string) => c_string.into_raw().cast_const(),
            Err(_) => return Self::error("complexity output contained an interior NUL byte"),
        };
        let warnings_ptr = if warnings.is_empty() {
            ptr::null()
        } else {
            CString::new(warnings).map_or(ptr::null(), |s| s.into_raw().cast_const())
        };
        Self {
            output: output_ptr,
            error: ptr::null(),
            warnings: warnings_ptr,
        }
    }

    fn error(message: &str) -> Self {
        Self {
            output: ptr::null(),
            error: CString::new(message).map_or(ptr::null(), |s| s.into_raw().cast_const()),
            warnings: ptr::null(),
        }
    }
}

/// Runs the complexity analysis pass over the given paths and returns the formatted report.
///
/// `format` must be `"text"` or `"json"`. `top` is the maximum number of entries in text output
/// (`0` prints all); it is ignored for JSON. `methods_only` skips code outside methods; `details`
/// collects the per-construct breakdown (shown in text and included in JSON); `group` groups
/// text output by class with subtotals (ignored for JSON).
/// Per-file non-fatal errors are returned via the `warnings` field (newline-joined) for the
/// caller to surface; a fatal config error is reported via the `error` field.
///
/// # Safety
///
/// - `paths` must point to `count` valid, null-terminated UTF-8 C strings.
/// - `format` must be a valid, null-terminated UTF-8 C string.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn rdx_complexity_analyze(
    paths: *const *const c_char,
    count: size_t,
    format: *const c_char,
    top: size_t,
    methods_only: bool,
    details: bool,
    group: bool,
) -> CComplexityResult {
    let Ok(format_str) = (unsafe { utils::convert_char_ptr_to_string(format) }) else {
        return CComplexityResult::error("format is not valid UTF-8");
    };

    let Ok(path_vec) = (unsafe { utils::convert_double_pointer_to_vec(paths, count) }) else {
        return CComplexityResult::error("one of the paths is not valid UTF-8");
    };

    let (report, errors) = match complexity::analyze(path_vec, methods_only, details) {
        Ok(result) => result,
        Err(error) => return CComplexityResult::error(&error.to_string()),
    };
    let warnings = if errors.is_empty() {
        String::new()
    } else {
        errors
            .iter()
            .map(std::string::ToString::to_string)
            .collect::<Vec<_>>()
            .join("\n")
    };

    let output = match format_str.as_str() {
        "text" => report.render_text(top, details, group),
        "json" => report.to_json(),
        other => {
            return CComplexityResult::error(&format!(
                "unknown complexity format `{other}` (expected `text` or `json`)"
            ));
        }
    };

    CComplexityResult::success(&output, &warnings)
}

/// Diffs two complexity reports (given as JSON strings) and returns the formatted diff.
///
/// `format` must be `"text"` or `"json"`. `top` caps each text section (`0` prints all); it is
/// ignored for JSON.
///
/// # Safety
///
/// - `baseline_json` and `current_json` must be valid, null-terminated UTF-8 C strings.
/// - `format` must be a valid, null-terminated UTF-8 C string.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn rdx_complexity_diff(
    baseline_json: *const c_char,
    current_json: *const c_char,
    format: *const c_char,
    top: size_t,
) -> CComplexityResult {
    let Ok(format_str) = (unsafe { utils::convert_char_ptr_to_string(format) }) else {
        return CComplexityResult::error("format is not valid UTF-8");
    };

    let Ok(baseline_str) = (unsafe { utils::convert_char_ptr_to_string(baseline_json) }) else {
        return CComplexityResult::error("baseline_json is not valid UTF-8");
    };

    let Ok(current_str) = (unsafe { utils::convert_char_ptr_to_string(current_json) }) else {
        return CComplexityResult::error("current_json is not valid UTF-8");
    };

    let baseline = match Report::from_json(&baseline_str) {
        Ok(report) => report,
        Err(error) => return CComplexityResult::error(&format!("invalid baseline report: {error}")),
    };

    let current = match Report::from_json(&current_str) {
        Ok(report) => report,
        Err(error) => return CComplexityResult::error(&format!("invalid current report: {error}")),
    };

    let diff = match Report::diff(&baseline, &current) {
        Ok(diff) => diff,
        Err(error) => return CComplexityResult::error(&error.to_string()),
    };

    let output = match format_str.as_str() {
        "text" => diff.render_text(top),
        "json" => diff.to_json(),
        other => {
            return CComplexityResult::error(&format!(
                "unknown complexity format `{other}` (expected `text` or `json`)"
            ));
        }
    };

    CComplexityResult::success(&output, "")
}
