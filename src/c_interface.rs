use std::ffi::{c_char, CString};

use crate::internal::index_all;

#[no_mangle]
pub extern "C" fn index_all_c(file_paths: *const *const c_char, count: usize) {
    if file_paths.is_null() {
        return;
    }

    let mut rust_file_paths = Vec::with_capacity(count);

    unsafe {
        for i in 0..count {
            let file_path_ptr = *file_paths.add(i);
            if file_path_ptr.is_null() {
                continue;
            }

            let c_str = std::ffi::CStr::from_ptr(file_path_ptr);
            match c_str.to_str() {
                Ok(s) => rust_file_paths.push(s.to_string()),
                Err(_) => continue, // Skip invalid UTF-8 strings
            }
        }
    }

    index_all(rust_file_paths);
}
