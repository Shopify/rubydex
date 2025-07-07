use crate::declaration::{Declaration, Location};
use crate::{Repository, index_in_parallel};
use libc::{c_char, c_void, size_t};
use std::ffi::CStr;
use std::slice::from_raw_parts;
use std::str::Utf8Error;
use std::sync::{Arc, Mutex};

type RepositoryPointer = *mut c_void;

unsafe fn convert_double_pointer_to_vec(data: &mut &mut c_char, len: size_t) -> Result<Vec<String>, Utf8Error> {
    unsafe {
        from_raw_parts(data, len)
            .iter()
            .map(|arg| CStr::from_ptr(*arg).to_str().map(ToString::to_string))
            .collect()
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn idx_declaration_free(pointer: *const Declaration) {
    unsafe {
        let _ = Box::from_raw(pointer as *mut Declaration);
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn idx_repository_new() -> RepositoryPointer {
    let repo_arc = Arc::new(Mutex::new(Repository::new()));
    Arc::into_raw(repo_arc) as RepositoryPointer
}

#[unsafe(no_mangle)]
pub extern "C" fn idx_repository_free(pointer: RepositoryPointer) {
    unsafe {
        Arc::from_raw(pointer as *mut Mutex<Repository>);
    }
}

fn retain_repository(pointer: RepositoryPointer) -> Arc<Mutex<Repository>> {
    unsafe {
        Arc::increment_strong_count(pointer);
        Arc::from_raw(pointer as *mut Mutex<Repository>)
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn idx_index_all_c(pointer: RepositoryPointer, file_paths: &mut &mut c_char, count: usize) {
    let file_paths: Vec<String> = unsafe { convert_double_pointer_to_vec(file_paths, count).unwrap() };
    let file_queue = Arc::new(Mutex::new(file_paths));
    let repository = retain_repository(pointer);
    index_in_parallel(&repository, &file_queue);
}

#[unsafe(no_mangle)]
pub extern "C" fn idx_repository_size(pointer: RepositoryPointer) -> usize {
    let repository = retain_repository(pointer);
    repository.lock().unwrap().size()
}

#[unsafe(no_mangle)]
pub extern "C" fn idx_repository_memsize(pointer: *mut c_void) -> usize {
    let repository = retain_repository(pointer);
    std::mem::size_of_val(&repository.lock().unwrap())
}

#[unsafe(no_mangle)]
pub extern "C" fn idx_repository_add_class_declaration(
    pointer: RepositoryPointer,
    name: &mut c_char,
    start_offset: u32,
    end_offset: u32,
) {
    let repository_arc = retain_repository(pointer);

    let string_name = match unsafe { CStr::from_ptr(name).to_str().map(ToString::to_string) } {
        Ok(name) => name,
        Err(_) => {
            eprintln!("Invalid UTF-8 string for class name");
            return;
        }
    };

    let mut repository = repository_arc.lock().unwrap();
    let location = Location::new(start_offset, end_offset);
    repository.add_class(string_name, location);
}

/// # Safety
/// This function assumes that the name and repository pointer are valid.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn idx_repository_get(pointer: RepositoryPointer, name: *const c_char) -> *const Declaration {
    let repository_arc = retain_repository(pointer);
    let string_name = match unsafe { CStr::from_ptr(name).to_str().map(ToString::to_string) } {
        Ok(name) => name,
        Err(_) => {
            eprintln!("Invalid UTF-8 string for class name");
            return std::ptr::null();
        }
    };

    let repository = repository_arc.lock().unwrap();
    match repository.get(&string_name) {
        Some(declaration) => Box::into_raw(Box::new(declaration.clone())) as *const Declaration,
        None => std::ptr::null(),
    }
}
