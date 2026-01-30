use libc::{c_char, size_t};
use std::ffi::{CStr, CString};
use std::slice;
use std::str::Utf8Error;

/// Converts a C array of strings into a Vec<String>
///
/// # Safety
///
/// This function is unsafe because it attempts to instantiate a Vec<String> from a raw char** pointer
///
/// # Errors
///
/// This function errors if any of the strings inside the array contain invalid UTF-8 data
pub unsafe fn convert_double_pointer_to_vec(data: *const *const c_char, len: size_t) -> Result<Vec<String>, Utf8Error> {
    unsafe {
        slice::from_raw_parts(data, len)
            .iter()
            .map(|arg| CStr::from_ptr(*arg).to_str().map(ToString::to_string))
            .collect()
    }
}

/// # Safety
///
/// This function is unsafe because it dereferences the char pointer, which needs to be valid for the duration of the
/// function
///
/// # Errors
///
/// This function errors if any of the strings inside the array contain invalid UTF-8 data
pub unsafe fn convert_char_ptr_to_string(data: *const c_char) -> Result<String, Utf8Error> {
    unsafe { CStr::from_ptr(data).to_str().map(ToString::to_string) }
}

/// Frees a `CString` allocated on the Rust side
#[unsafe(no_mangle)]
pub extern "C" fn free_c_string(ptr: *const c_char) {
    unsafe {
        let _ = CString::from_raw(ptr.cast_mut());
    }
}

/// Frees a boxed u32 allocated on the Rust side
#[unsafe(no_mangle)]
pub extern "C" fn free_u32(ptr: *const u32) {
    unsafe {
        let _ = Box::from_raw(ptr.cast_mut());
    }
}
