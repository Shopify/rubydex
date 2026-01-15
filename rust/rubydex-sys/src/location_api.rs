//! Location-related C API and structs

use libc::c_char;
use rubydex::model::document::Document;
use rubydex::model::graph::Graph;
use rubydex::offset::Offset;
use std::ffi::CString;

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

/// Helper to create a location for a given URI and byte-offset range.
/// Allocates and returns a pointer to `Location`. Caller must free with `rdx_location_free`.
///
/// # Panics
///
/// - If the URI cannot be converted to a file path.
/// - If the file cannot be read.
/// - If the offset cannot be converted to a position.
#[must_use]
pub(crate) fn create_location_for_uri_and_offset(graph: &Graph, document: &Document, offset: &Offset) -> *mut Location {
    let line_index = document.line_index();
    let start_pos = line_index.line_col(offset.start().into());
    let end_pos = line_index.line_col(offset.end().into());

    let loc = if let Some(wide_encoding) = graph.encoding().to_wide() {
        let wide_start_pos = line_index.to_wide(wide_encoding, start_pos).unwrap();
        let wide_end_pos = line_index.to_wide(wide_encoding, end_pos).unwrap();

        Location {
            uri: CString::new(document.uri()).unwrap().into_raw().cast_const(),
            start_line: wide_start_pos.line + 1,
            end_line: wide_end_pos.line + 1,
            start_column: wide_start_pos.col + 1,
            end_column: wide_end_pos.col + 1,
        }
    } else {
        Location {
            uri: CString::new(document.uri()).unwrap().into_raw().cast_const(),
            start_line: start_pos.line + 1,
            end_line: end_pos.line + 1,
            start_column: start_pos.col + 1,
            end_column: end_pos.col + 1,
        }
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
pub unsafe extern "C" fn rdx_location_free(ptr: *mut Location) {
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
