//! This file provides the C API for the Graph object

use crate::conversions;
use index::indexing;
use index::model::graph::Graph;
use libc::{c_char, c_void};
use std::ffi::CString;
use std::ptr;
use std::sync::{Arc, Mutex};

pub type GraphPointer = *mut c_void;

/// Creates a new graph within a mutex. This is meant to be used when creating new Graph objects in Ruby
#[unsafe(no_mangle)]
pub extern "C" fn idx_graph_new() -> GraphPointer {
    let repo_arc = Arc::new(Mutex::new(Graph::new()));
    Arc::into_raw(repo_arc) as GraphPointer
}

/// Frees a Graph through its pointer
#[unsafe(no_mangle)]
pub extern "C" fn idx_graph_free(pointer: GraphPointer) {
    unsafe {
        Arc::from_raw(pointer.cast::<Mutex<Graph>>());
    }
}

/// Retains a pointer to a Graph object, incrementing its Arc reference count. This function should always be used when
/// transforming a void pointer back into a Graph object, to ensure that the reference count is updated as expected
fn retain_graph(pointer: GraphPointer) -> Arc<Mutex<Graph>> {
    unsafe {
        Arc::increment_strong_count(pointer);
        Arc::from_raw(pointer.cast::<Mutex<Graph>>())
    }
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
    let graph = retain_graph(pointer);
    let mut all_errors = Vec::new();
    let (documents, document_errors) = indexing::collect_documents_in_parallel(file_paths);
    all_errors.extend(document_errors);

    if let Err(errors) = indexing::index_in_parallel(&graph, &documents) {
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
}

/// # Safety
///
/// This function assumes that both the graph and the db path pointers are valid
///
/// # Panics
///
/// This function will panic in case of a dead lock on the graph mutex
#[unsafe(no_mangle)]
pub unsafe extern "C" fn idx_graph_set_configuration(pointer: GraphPointer, db_path: *const c_char) {
    let graph = retain_graph(pointer);

    match unsafe { conversions::convert_char_ptr_to_string(db_path) } {
        Ok(path) => {
            graph.lock().unwrap().set_configuration(path);
        }
        Err(e) => {
            eprintln!("Failed to convert db_path to String: {e}");
        }
    }
}
