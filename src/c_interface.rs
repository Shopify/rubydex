use crate::declaration::{Declaration, Location};
use crate::{Repository, index_in_parallel};
use libc::{c_char, c_void, size_t};
use std::ffi::CStr;
use std::slice::from_raw_parts;
use std::str::Utf8Error;
use std::sync::{Arc, Mutex};

// This file contains the Rust to C interface that we provide to the Ruby VM so that we can bind Ruby methods and
// objects to Rust implementations. The only time code should be added here is if we're exposing something to Ruby.  All
// public functions that we expose to C should be placed in here so that we have clear separation between the Rust
// crate's API and the C interface used by the Ruby VM.
//
// # Important implementation notes:
//
// ## Returning values to C
//
// The C interface cannot return references to Rust objects because Rust may decide to move, drop or otherwise
// reorganize them. The only exception is returning Arcs. When returning an Arc, we need to ensure that their strong
// count is correctly incremented and that they are dropped when expected. See `retain_repository` and
// `idx_repository_free`.
//
// The other option is returning boxed owned values that we return into pointers with `Box::into_raw`. In these cases,
// data is cloned on demand so that the Ruby layer doesn't keep a reference to Rust data.
//
// ## Freeing memory
//
// Memory allocated by Rust has to be freed by Rust since it can change which allocator is used. Do not try to free Rust
// allocated data from C directly.
//
// ## Naming conventions
//
// C has no support for namespaces, modules or classes. To improve IDE completion and readability, we prefix every
// public C interface function with `idx_` to identify this project + the entity that the function relates to. For
// example, `idx_repository_new` creates a new repository. That way, when typing `idx_repo` we can easily see all
// functions related to repositories and so on.

type RepositoryPointer = *mut c_void;
type DeclarationPointer = *const Declaration;

/// Converts an array of C strings (const **char) to a Vec<String>
unsafe fn convert_double_pointer_to_vec(data: &mut &mut c_char, len: size_t) -> Result<Vec<String>, Utf8Error> {
    unsafe {
        from_raw_parts(data, len)
            .iter()
            .map(|arg| CStr::from_ptr(*arg).to_str().map(ToString::to_string))
            .collect()
    }
}

/// Frees a pointer to a Declaration object
#[unsafe(no_mangle)]
pub extern "C" fn idx_declaration_free(pointer: DeclarationPointer) {
    unsafe {
        let _ = Box::from_raw(pointer as *mut Declaration);
    }
}

/// Creates a new Repository object inside an Arc<Mutex<>> and returns a pointer to it. This function is used to create
/// the data that will back the Ruby objects representing repositories
#[unsafe(no_mangle)]
pub extern "C" fn idx_repository_new() -> RepositoryPointer {
    let repo_arc = Arc::new(Mutex::new(Repository::new()));
    Arc::into_raw(repo_arc) as RepositoryPointer
}

/// Frees a pointer to a Repository object
#[unsafe(no_mangle)]
pub extern "C" fn idx_repository_free(pointer: RepositoryPointer) {
    unsafe {
        Arc::from_raw(pointer as *mut Mutex<Repository>);
    }
}

/// Retains a pointer to a Repository object, incrementing its Arc reference count. This function should always be used
/// when transforming a void pointer back into a Repository object, to ensure that the reference count is updated as
/// expected
fn retain_repository(pointer: RepositoryPointer) -> Arc<Mutex<Repository>> {
    unsafe {
        Arc::increment_strong_count(pointer);
        Arc::from_raw(pointer as *mut Mutex<Repository>)
    }
}

/// Indexes all given file paths in parallel using the provided repository pointer
#[unsafe(no_mangle)]
pub extern "C" fn idx_index_all_c(pointer: RepositoryPointer, file_paths: &mut &mut c_char, count: usize) {
    let file_paths: Vec<String> = unsafe { convert_double_pointer_to_vec(file_paths, count).unwrap() };
    let file_queue = Arc::new(Mutex::new(file_paths));
    let repository = retain_repository(pointer);
    index_in_parallel(&repository, &file_queue);
}

/// Returns the total number of declarations in the repository
#[unsafe(no_mangle)]
pub extern "C" fn idx_repository_size(pointer: RepositoryPointer) -> usize {
    let repository = retain_repository(pointer);
    repository.lock().unwrap().size()
}

/// Allows adding a class declaration to the repository
///
/// This function has to ensure that all data coming from Ruby is cloned, otherwise we may keep references to data owned
/// by the VM, which could be garbage collected at any time.
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

/// Retrieve an entry from the repository by its name
///
/// This function always clones the data.
///
/// # Safety
/// This function assumes that the name and repository pointer are valid.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn idx_repository_get(pointer: RepositoryPointer, name: *const c_char) -> DeclarationPointer {
    let repository_arc = retain_repository(pointer);
    let string_name = match unsafe { CStr::from_ptr(name).to_str().map(ToString::to_string) } {
        Ok(name) => name,
        Err(_) => {
            eprintln!("Invalid UTF-8 string for class name");
            return std::ptr::null();
        }
    };

    let repository = repository_arc.lock().unwrap();
    repository.get(&string_name).map_or_else(std::ptr::null, |declaration| {
        Box::into_raw(Box::new(declaration.clone())) as DeclarationPointer
    })
}
