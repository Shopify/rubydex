use crate::{Repository, index_in_parallel};
use libc::{c_char, c_void, size_t};
use std::ffi::CStr;
use std::mem::forget;
use std::slice::from_raw_parts;
use std::str::Utf8Error;
use std::sync::{Arc, Mutex};

unsafe fn convert_double_pointer_to_vec(data: &mut &mut c_char, len: size_t) -> Result<Vec<String>, Utf8Error> {
    unsafe {
        from_raw_parts(data, len)
            .iter()
            .map(|arg| CStr::from_ptr(*arg).to_str().map(ToString::to_string))
            .collect()
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn new_repo() -> *mut c_void {
    let repo_arc = Arc::new(Mutex::new(Repository::new()));
    Arc::into_raw(repo_arc) as *mut c_void
}

#[unsafe(no_mangle)]
pub extern "C" fn free_repo(pointer: *mut c_void) {
    unsafe {
        let _ = Arc::from_raw(pointer as *mut Mutex<Repository>);
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn index_all_c(pointer: *mut c_void, file_paths: &mut &mut c_char, count: usize) {
    let file_paths: Vec<String> = unsafe { convert_double_pointer_to_vec(file_paths, count).unwrap() };
    let file_queue = Arc::new(Mutex::new(file_paths));

    let repository = unsafe { Arc::from_raw(pointer as *mut Mutex<Repository>) };
    index_in_parallel(&repository, &file_queue);
    forget(repository);
}

#[unsafe(no_mangle)]
pub extern "C" fn repo_size(pointer: *mut c_void) -> usize {
    let repository = unsafe { Arc::from_raw(pointer as *mut Mutex<Repository>) };
    let size = repository.lock().unwrap().size();
    forget(repository);
    size
}
