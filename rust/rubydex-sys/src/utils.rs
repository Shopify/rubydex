use libc::{c_char, size_t};
use std::ffi::{CStr, CString};
use std::path::Path;
use std::ptr;
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

/// C-compatible owned UTF-8 string with an explicit length. Unlike a C string, it is not NUL-terminated and may
/// contain NUL bytes, which source code can put into names, comments and diagnostic messages. A null `data` means
/// there is no string. Dropping it frees the bytes
#[repr(C)]
pub struct OwnedString {
    pub data: *const c_char,
    pub length: usize,
}

impl OwnedString {
    #[must_use]
    pub fn new(value: &str) -> Self {
        let bytes: Box<[u8]> = value.as_bytes().into();
        Self {
            length: bytes.len(),
            data: Box::into_raw(bytes).cast::<c_char>().cast_const(),
        }
    }

    #[must_use]
    pub fn null() -> Self {
        Self {
            data: ptr::null(),
            length: 0,
        }
    }
}

impl Drop for OwnedString {
    fn drop(&mut self) {
        if !self.data.is_null() {
            let bytes = ptr::slice_from_raw_parts_mut(self.data.cast_mut().cast::<u8>(), self.length);
            // SAFETY: a non-null `data` always comes from `Box::into_raw` in `OwnedString::new`, with `length` bytes
            let _ = unsafe { Box::from_raw(bytes) };
        }
    }
}

/// Frees an `OwnedString` allocated on the Rust side
///
/// # Safety
///
/// - `string` must have been returned by a rubydex function and must not be used after being freed
#[unsafe(no_mangle)]
pub unsafe extern "C" fn free_owned_string(string: OwnedString) {
    drop(string);
}

/// Frees a boxed u64 allocated on the Rust side
#[unsafe(no_mangle)]
pub extern "C" fn free_u64(ptr: *const u64) {
    unsafe {
        let _ = Box::from_raw(ptr.cast_mut());
    }
}

/// Frees an array of C strings allocated by Rust.
///
/// # Safety
/// - `ptr` must be a pointer to a boxed slice of C strings previously allocated by this crate.
/// - `count` must be the length of the array.
/// - `ptr` must not be used after being freed.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn free_c_string_array(ptr: *const *const c_char, count: usize) {
    if ptr.is_null() {
        return;
    }

    let slice = unsafe { Box::from_raw(std::ptr::slice_from_raw_parts_mut(ptr.cast_mut(), count)) };
    let _: Vec<_> = slice
        .iter()
        .filter(|p| !p.is_null())
        .map(|arg| unsafe { CString::from_raw((*arg).cast_mut()) })
        .collect();
}

/// Converts a Rust `&str` to an owned C string (`*const c_char`), suitable for returning across the
/// FFI boundary. The caller is responsible for freeing it with `free_c_string`.
///
/// # Panics
///
/// Panics if `value` contains an interior null byte, which should never occur in rubydex data.
#[must_use]
pub fn cstring_raw(value: &str) -> *const c_char {
    CString::new(value).unwrap().into_raw().cast_const()
}

/// Rust uses backslashes as separators on Windows, but Ruby prefers forward slashes everywhere. We need to make sure
/// we're maintaining the right separators at the boundary.
#[must_use]
pub fn interop_path(path: &Path) -> String {
    path.to_string_lossy().replace(std::path::MAIN_SEPARATOR, "/")
}

#[cfg(test)]
mod tests {
    use super::*;

    #[cfg(unix)]
    #[test]
    fn interop_path_leaves_a_unix_path_untouched() {
        // A backslash is an ordinary character in a Unix file name, so rewriting it would name a different file.
        assert_eq!(interop_path(Path::new(r"/tmp/we\ird")), r"/tmp/we\ird");
    }

    #[cfg(windows)]
    #[test]
    fn interop_path_separates_a_windows_path_with_forward_slashes() {
        assert_eq!(interop_path(Path::new(r"D:\a\_temp\project")), "D:/a/_temp/project");
    }
}
