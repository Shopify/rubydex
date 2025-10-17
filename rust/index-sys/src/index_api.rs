//! This file provides the C API for the Graph object

use crate::conversions;
use index::indexing;
use index::model::graph::Graph;
use index::model::ids::{DefinitionId, NameId};
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

/// Returns the list of declaration IDs as a heap-allocated i64 array and writes its length to `out_len`.
/// Caller must free with `free_i64_array`.
///
/// # Safety
///
/// Assumes pointer and `out_len` are valid.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn idx_graph_declaration_ids(pointer: GraphPointer, out_len: *mut usize) -> *const i64 {
    with_graph(pointer, |graph| {
        let ids: Vec<i64> = graph.declarations().keys().map(|name_id| **name_id).collect();
        unsafe { *out_len = ids.len() };
        Box::into_raw(ids.into_boxed_slice()) as *const i64
    })
}

/// Looks up a declaration by its fully qualified name and returns its id as i64, or 0 if not found
///
/// # Safety
///
/// Assumes pointer and name are valid; name must be a valid C string
#[unsafe(no_mangle)]
pub unsafe extern "C" fn idx_graph_resolve_name(pointer: GraphPointer, name: *const c_char) -> i64 {
    with_graph(pointer, |graph| {
        let Ok(name_str) = (unsafe { conversions::convert_char_ptr_to_string(name) }) else {
            return 0;
        };

        let name_id = NameId::from(name_str.as_str());
        if graph.declarations().contains_key(&name_id) {
            *name_id
        } else {
            0
        }
    })
}

/// Returns the UTF-8 name string for a declaration id.
/// Caller must free with `free_c_string`.
///
/// # Safety
///
/// Assumes pointer is valid.
///
/// # Panics
///
/// This function will panic if the name pointer is invalid.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn idx_declaration_name(pointer: GraphPointer, name_id: i64) -> *const c_char {
    with_graph(pointer, |graph| {
        let name_id = NameId::new(name_id);
        if let Some(decl) = graph.declarations().get(&name_id) {
            CString::new(decl.name()).unwrap().into_raw().cast_const()
        } else {
            ptr::null()
        }
    })
}

/// Returns the list of definition IDs for a declaration id.
/// Caller must free with `free_i64_array`.
///
/// # Safety
/// Assumes pointer and `out_len` are valid.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn idx_declaration_definition_ids(
    pointer: GraphPointer,
    name_id: i64,
    out_len: *mut usize,
) -> *const i64 {
    with_graph(pointer, |graph| {
        let name_id = NameId::new(name_id);
        let ids: Vec<i64> = if let Some(decl) = graph.declarations().get(&name_id) {
            decl.definitions().iter().map(|def_id| **def_id).collect()
        } else {
            Vec::new()
        };

        unsafe { *out_len = ids.len() };
        Box::into_raw(ids.into_boxed_slice()) as *const i64
    })
}

/// Returns the kind string for a definition id. Caller must free with `free_c_string`.
///
/// # Safety
///
/// Assumes pointer is valid.
///
/// # Panics
///
/// This function will panic if the definition pointer is invalid.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn idx_definition_kind(pointer: GraphPointer, def_id: i64) -> *const c_char {
    with_graph(pointer, |graph| {
        let def_id = DefinitionId::new(def_id);
        if let Some(def) = graph.definitions().get(&def_id) {
            CString::new(def.kind()).unwrap().into_raw().cast_const()
        } else {
            ptr::null()
        }
    })
}

/// Frees a heap-allocated i64 array created on the Rust side.
///
/// # Safety
///
/// The `ptr` must be a pointer returned by this crate (e.g., `idx_graph_declaration_ids` or
/// `idx_graph_definition_ids_for`) and `len` must be the exact length that was written to the
/// corresponding out parameter. Passing an incorrect pointer or length is undefined behavior.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn free_i64_array(ptr: *const i64, len: usize) {
    if ptr.is_null() {
        return;
    }
    let slice = unsafe { std::slice::from_raw_parts_mut(ptr.cast_mut(), len) };
    let _ = unsafe { Box::from_raw(std::ptr::from_mut::<[i64]>(slice)) };
}
