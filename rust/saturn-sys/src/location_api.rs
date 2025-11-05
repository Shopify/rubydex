//! Location-related C API and structs

use libc::c_char;
use saturn::offset::Offset;
use saturn::source_location::{SourceLocationConverter, UTF8SourceLocationConverter};
use std::ffi::CString;
use std::fs;
use url::Url;

/// C-compatible struct representing a definition location with offsets and line/column positions.
#[repr(C)]
#[derive(Debug, Clone)]
pub struct Location {
    pub uri: *const c_char,
    pub start_line: u32,
    pub end_line: u32,
    pub start_column: u32,
    pub end_column: u32,
}

#[must_use]
fn file_path_from_uri(uri: &str) -> Option<String> {
    Url::parse(uri)
        .ok()?
        .to_file_path()
        .ok()
        .map(|p| p.to_string_lossy().into_owned())
}

/// Helper to create a location for a given URI and byte-offset range.
/// Allocates and returns a pointer to `Location`. Caller must free with `sat_location_free`.
///
/// # Panics
///
/// - If the URI cannot be converted to a file path.
/// - If the file cannot be read.
/// - If the offset cannot be converted to a position.
#[must_use]
pub(crate) fn create_location_for_uri_and_offset(uri: &str, start: u32, end: u32) -> *mut Location {
    let path_str = file_path_from_uri(uri).unwrap_or_else(|| panic!("Failed to convert URI to file path: {uri}"));

    let source = fs::read_to_string(&path_str).unwrap_or_else(|_| panic!("Failed to read file at path {path_str}"));
    let converter = UTF8SourceLocationConverter::new(&source);
    let offset = Offset::new(start, end);
    let (start_line, start_column, end_line, end_column) = match converter.offset_to_position(&offset) {
        Some((start_pos, end_pos)) => (start_pos.line(), start_pos.column(), end_pos.line(), end_pos.column()),
        None => (0, 0, 0, 0),
    };

    let loc = Location {
        uri: CString::new(uri).unwrap().into_raw().cast_const(),
        start_line: start_line + 1,
        end_line: end_line + 1,
        start_column: start_column + 1,
        end_column: end_column + 1,
    };

    Box::into_raw(Box::new(loc))
}

/// Frees a `Location` struct and its owned inner strings.
///
/// # Safety
///
/// - `ptr` must be a valid pointer previously returned by `create_location_for_uri_and_offset`.
/// - `ptr` must not be used after being freed.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn sat_location_free(ptr: *mut Location) {
    if ptr.is_null() {
        return;
    }
    unsafe {
        // Take ownership of the box so we can free inner allocations first
        let boxed = Box::from_raw(ptr);

        if !boxed.uri.is_null() {
            let _ = CString::from_raw(boxed.uri.cast_mut());
        }

        // Box drops here, freeing the struct memory
    }
}
