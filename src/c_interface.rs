use crate::internal::index_all;
use libc::{c_char, size_t};
use std::str::Utf8Error;
use std::slice::from_raw_parts;
use std::ffi::CStr;

unsafe fn convert_double_pointer_to_vec(
    data: &mut &mut c_char,
    len: size_t,
) -> Result<Vec<String>, Utf8Error> {
    from_raw_parts(data, len)
        .iter()
        .map(|arg| CStr::from_ptr(*arg).to_str().map(ToString::to_string))
        .collect()
}

#[no_mangle]
pub extern "C" fn index_all_c(file_paths: &mut &mut c_char, count: usize) {
    let file_paths: Vec<String> = unsafe {
        convert_double_pointer_to_vec(file_paths, count).unwrap()
     };
     index_all(file_paths);
}