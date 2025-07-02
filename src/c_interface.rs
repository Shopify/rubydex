use crate::Repository;
use libc::{c_char, c_void, size_t};
use std::ffi::CStr;
use std::slice::from_raw_parts;
use std::str::Utf8Error;

// Converts a void pointer from C into a mutable Rust reference without causing Rust to free the pointer once the object
// goes out of scope. Always use this macro when reinstantiating a Rust object back from the void pointer
macro_rules! pointer_to_ref {
    ($pointer:expr, $type:ty) => {
        unsafe { &mut *($pointer as *mut $type) }
    };
}

unsafe fn convert_double_pointer_to_vec(data: &mut &mut c_char, len: size_t) -> Result<Vec<String>, Utf8Error> {
    unsafe {
        from_raw_parts(data, len)
            .iter()
            .map(|arg| CStr::from_ptr(*arg).to_str().map(ToString::to_string))
            .collect()
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn index_all_c(pointer: *mut c_void, file_paths: &mut &mut c_char, count: usize) {
    let file_paths: Vec<String> = unsafe { convert_double_pointer_to_vec(file_paths, count).unwrap() };
    let repository = pointer_to_ref!(pointer, Repository);
    repository.index_all(file_paths);
}

#[unsafe(no_mangle)]
pub extern "C" fn new_repo() -> *mut c_void {
    Box::into_raw(Box::new(Repository::new())) as *mut c_void
}
