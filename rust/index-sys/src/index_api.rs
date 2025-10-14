//! This file provides the C API for the Graph object

use crate::conversions;
use index::indexing;
use index::model::graph::Graph;
use libc::{c_char, c_void};
use std::ffi::CString;
use std::{mem, ptr};

pub type GraphPointer = *mut c_void;

/// Creates a new graph within a mutex. This is meant to be used when creating new Graph objects in Ruby
#[unsafe(no_mangle)]
pub extern "C" fn idx_graph_new() -> GraphPointer {
    Box::into_raw(Box::new(Graph::new())) as GraphPointer
}

/// Frees a Graph through its pointer
#[unsafe(no_mangle)]
pub extern "C" fn idx_graph_free(pointer: GraphPointer) {
    unsafe {
        let _ = Box::from_raw(pointer.cast::<Graph>());
    }
}

fn with_graph<F, T>(pointer: GraphPointer, action: F) -> T
where
    F: FnOnce(&mut Graph) -> T,
{
    let mut graph = unsafe { Box::from_raw(pointer.cast::<Graph>()) };
    let result = action(&mut graph);
    mem::forget(graph);
    result
}

/// Indexes all given file paths in parallel using the provided Graph pointer
///
/// # Panics
///
/// Will panic if the given array of C string file paths cannot be converted to a Vec<String>
///
/// # Safety
///
/// This function is unsafe because it dereferences raw pointers coming from C. The caller has to ensure that the Ruby
/// VM will not free the pointers related to the string array while they are in use by Rust
#[unsafe(no_mangle)]
pub unsafe extern "C" fn idx_index_all_c(
    pointer: GraphPointer,
    file_paths: *const *const c_char,
    count: usize,
) -> *const c_char {
    let file_paths: Vec<String> = unsafe { conversions::convert_double_pointer_to_vec(file_paths, count).unwrap() };
    let mut all_errors = Vec::new();

    let (documents, document_errors) = indexing::collect_documents(file_paths);
    all_errors.extend(document_errors);

    with_graph(pointer, |graph| {
        if let Err(errors) = indexing::index_in_parallel(graph, documents) {
            all_errors.extend(errors.0);
        }

        if all_errors.is_empty() {
            return ptr::null();
        }

        let concatenated_errors = all_errors.into_iter().map(|e| e.to_string()).collect::<Vec<_>>();
        CString::new(concatenated_errors.join("\n"))
            .unwrap()
            .into_raw()
            .cast_const()
    })
}

/// # Safety
///
/// This function assumes that both the graph and the db path pointers are valid
///
/// # Panics
///
/// This function will panic in case of a dead lock on the graph mutex
#[unsafe(no_mangle)]
pub unsafe extern "C" fn idx_graph_set_configuration(pointer: GraphPointer, db_path: *const c_char) -> bool {
    with_graph(pointer, |graph| {
        match unsafe { conversions::convert_char_ptr_to_string(db_path) } {
            Ok(path) => graph.set_configuration(path).is_ok(),
            Err(e) => {
                eprintln!("Failed to convert db_path to String: {e}");
                false
            }
        }
    })
}

// TODO

/// # Safety
/// # Panics
#[unsafe(no_mangle)]
pub unsafe extern "C" fn idx_graph_declarations(pointer: GraphPointer) -> *const *const c_char {
    with_graph(pointer, |graph| {
        // Convert to raw C string pointers and leak them; pair with free function
        let mut ptrs: Vec<*const c_char> = graph
            .declarations()
            .values()
            .map(|declaration| CString::new(declaration.name()).unwrap().into_raw().cast_const())
            .collect();
        ptrs.push(ptr::null());
        Box::into_raw(ptrs.into_boxed_slice()) as *const *const c_char
    })
}

/// Returns a NULL-terminated array of C strings describing each definition for the given declaration name.
/// Each string has the format: "uri:start:end:kind".
///
/// # Safety
/// This function assumes both pointers are valid and the declaration points to a valid, null-terminated C string.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn idx_graph_definitions_for(
    pointer: GraphPointer,
    declaration: *const c_char,
) -> *const *const c_char {
    with_graph(pointer, |graph| {
        let name = if let Ok(s) = unsafe { conversions::convert_char_ptr_to_string(declaration) } {
            s
        } else {
            // Return an empty, NULL-terminated array
            let ptrs: Vec<*const c_char> = vec![ptr::null()];
            return Box::into_raw(ptrs.into_boxed_slice()) as *const *const c_char;
        };

        let defs = graph.get(&name).unwrap_or_default();

        let mut ptrs: Vec<*const c_char> = Vec::with_capacity(defs.len() + 1);
        for def in defs {
            let uri = graph.documents().get(def.uri_id()).map_or("", |d| d.uri());
            let s = format!("{}:{}:{}:{}", uri, def.start(), def.end(), def.kind());
            ptrs.push(CString::new(s).unwrap().into_raw().cast_const());
        }

        ptrs.push(ptr::null());
        Box::into_raw(ptrs.into_boxed_slice()) as *const *const c_char
    })
}

/// # Safety
/// # Panics
#[unsafe(no_mangle)]
pub unsafe extern "C" fn free_c_string_array(ptr: *const *const c_char) {
    if ptr.is_null() {
        return;
    }

    // Reconstruct the boxed slice length by walking until NULL terminator
    let mut len = 0usize;
    loop {
        let p = *ptr.add(len);
        if p.is_null() {
            break;
        }
        len += 1;
    }

    // Reconstruct the boxed slice of pointers to drop it
    let slice = std::slice::from_raw_parts(ptr, len + 1); // include NULL
    // Free each string
    for &s_ptr in &slice[..len] {
        let _ = CString::from_raw(s_ptr.cast_mut());
    }
    // Now free the pointers array itself
    let _ = Box::from_raw(slice.as_ptr() as *mut *const c_char);
}
