//! Location-related C API and structs

use libc::c_char;
use saturn::location;
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

/// Converts a `Location` struct to a pointer to a FFI `Location` struct.
///
/// # Panics
///
/// - If the URI cannot be converted to a C string.
#[must_use]
pub fn location_to_ffi(location: &location::Location) -> *mut Location {
    let ffi_location = Location {
        uri: CString::new(location.uri()).unwrap().into_raw().cast_const(),
        start_line: location.line_start(),
        end_line: location.line_end(),
        start_column: location.column_start(),
        end_column: location.column_end(),
    };

    Box::into_raw(Box::new(ffi_location))
}

/// Frees a `Location` struct and its owned inner strings.
///
/// # Safety
///
/// - `ptr` must be a valid pointer previously returned by `location_to_ffi`.
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
