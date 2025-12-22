//! This file provides the C API for the Graph object

use crate::reference_api::{ReferenceKind, ReferencesIter};
use crate::utils;
use libc::{c_char, c_void};
use saturn::model::graph::Graph;
use saturn::model::ids::DeclarationId;
use saturn::{indexing, listing, resolution};
use std::ffi::CString;
use std::{mem, ptr};

pub type GraphPointer = *mut c_void;

/// Creates a new graph within a mutex. This is meant to be used when creating new Graph objects in Ruby
#[unsafe(no_mangle)]
pub extern "C" fn sat_graph_new() -> GraphPointer {
    Box::into_raw(Box::new(Graph::new())) as GraphPointer
}

/// Frees a Graph through its pointer
#[unsafe(no_mangle)]
pub extern "C" fn sat_graph_free(pointer: GraphPointer) {
    unsafe {
        let _ = Box::from_raw(pointer.cast::<Graph>());
    }
}

pub fn with_graph<F, T>(pointer: GraphPointer, action: F) -> T
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
pub unsafe extern "C" fn sat_index_all(
    pointer: GraphPointer,
    file_paths: *const *const c_char,
    count: usize,
) -> *const c_char {
    let file_paths: Vec<String> = unsafe { utils::convert_double_pointer_to_vec(file_paths, count).unwrap() };
    let (file_paths, errors) = listing::collect_file_paths(file_paths);

    if !errors.is_empty() {
        let error_messages = errors
            .iter()
            .map(std::string::ToString::to_string)
            .collect::<Vec<_>>()
            .join("\n");

        return CString::new(error_messages).unwrap().into_raw().cast_const();
    }

    with_graph(pointer, |graph| {
        if let Err(errors) = indexing::index_in_parallel(graph, file_paths) {
            return CString::new(errors.to_string()).unwrap().into_raw().cast_const();
        }

        ptr::null()
    })
}

/// Runs the resolver to compute declarations, ownership and related structures
#[unsafe(no_mangle)]
pub extern "C" fn sat_graph_resolve(pointer: GraphPointer) {
    with_graph(pointer, |graph| {
        resolution::resolve_all(graph);
    });
}

/// An iterator over declaration IDs
///
/// We snapshot the IDs at iterator creation so if the graph is modified, the iterator will not see the changes
#[derive(Debug)]
pub struct DeclarationsIter {
    /// The snapshot of declaration IDs
    ids: Box<[i64]>,
    /// The current index of the iterator
    index: usize,
}

/// Creates a new iterator over declaration IDs by snapshotting the current set of IDs.
///
/// # Safety
///
/// - `pointer` must be a valid `GraphPointer` previously returned by this crate.
/// - The returned pointer must be freed with `sat_graph_declarations_iter_free`.
///
/// # Panics
///
/// Will panic if acquiring a read lock on the graph's declarations fails
#[unsafe(no_mangle)]
pub unsafe extern "C" fn sat_graph_declarations_iter_new(pointer: GraphPointer) -> *mut DeclarationsIter {
    // Snapshot the IDs at iterator creation to avoid borrowing across FFI calls
    let ids = with_graph(pointer, |graph| {
        let read_lock = graph.declarations().read().unwrap();
        read_lock
            .keys()
            .map(|name_id| **name_id)
            .collect::<Vec<i64>>()
            .into_boxed_slice()
    });

    Box::into_raw(Box::new(DeclarationsIter { ids, index: 0 }))
}

/// Returns the total number of IDs in the iterator snapshot.
///
/// # Safety
///
/// - `iter` must be a valid pointer previously returned by `sat_graph_declarations_iter_new`.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn sat_graph_declarations_iter_len(iter: *const DeclarationsIter) -> usize {
    if iter.is_null() {
        return 0;
    }

    unsafe { (&*iter).ids.len() }
}

/// Advances the iterator and writes the next ID into `out_id`.
/// Returns `true` if an ID was written, or `false` if the iterator is exhausted or inputs are invalid.
///
/// # Safety
///
/// - `iter` must be a valid pointer previously returned by `sat_graph_declarations_iter_new`.
/// - `out_id` must be a valid, writable pointer.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn sat_graph_declarations_iter_next(iter: *mut DeclarationsIter, out_id: *mut i64) -> bool {
    if iter.is_null() || out_id.is_null() {
        return false;
    }

    let it = unsafe { &mut *iter };
    if it.index >= it.ids.len() {
        return false;
    }

    let id = it.ids[it.index];
    it.index += 1;
    unsafe { *out_id = id };

    true
}

/// Frees an iterator created by `sat_graph_declarations_iter_new`.
///
/// # Safety
///
/// - `iter` must be a pointer previously returned by `sat_graph_declarations_iter_new`.
/// - `iter` must not be used after being freed.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn sat_graph_declarations_iter_free(iter: *mut DeclarationsIter) {
    if iter.is_null() {
        return;
    }

    unsafe {
        let _ = Box::from_raw(iter);
    }
}

/// An iterator over document (URI) IDs
///
/// We snapshot the IDs at iterator creation so if the graph is modified, the iterator will not see the changes
#[derive(Debug)]
pub struct DocumentsIter {
    /// The snapshot of document (URI) IDs
    ids: Box<[i64]>,
    /// The current index of the iterator
    index: usize,
}

/// Creates a new iterator over document (URI) IDs by snapshotting the current set of IDs.
///
/// # Safety
///
/// - `pointer` must be a valid `GraphPointer` previously returned by this crate.
/// - The returned pointer must be freed with `sat_graph_documents_iter_free`.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn sat_graph_documents_iter_new(pointer: GraphPointer) -> *mut DocumentsIter {
    // Snapshot the IDs at iterator creation to avoid borrowing across FFI calls
    let ids = with_graph(pointer, |graph| {
        graph
            .documents()
            .keys()
            .map(|uri_id| **uri_id)
            .collect::<Vec<i64>>()
            .into_boxed_slice()
    });

    Box::into_raw(Box::new(DocumentsIter { ids, index: 0 }))
}

/// Returns the total number of IDs in the iterator snapshot.
///
/// # Safety
///
/// - `iter` must be a valid pointer previously returned by `sat_graph_documents_iter_new`.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn sat_graph_documents_iter_len(iter: *const DocumentsIter) -> usize {
    if iter.is_null() {
        return 0;
    }

    unsafe { (&*iter).ids.len() }
}

/// Advances the iterator and writes the next ID into `out_id`.
/// Returns `true` if an ID was written, or `false` if the iterator is exhausted or inputs are invalid.
///
/// # Safety
///
/// - `iter` must be a valid pointer previously returned by `sat_graph_documents_iter_new`.
/// - `out_id` must be a valid, writable pointer.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn sat_graph_documents_iter_next(iter: *mut DocumentsIter, out_id: *mut i64) -> bool {
    if iter.is_null() || out_id.is_null() {
        return false;
    }

    let it = unsafe { &mut *iter };
    if it.index >= it.ids.len() {
        return false;
    }

    let id = it.ids[it.index];
    it.index += 1;
    unsafe { *out_id = id };

    true
}

/// Frees an iterator created by `sat_graph_documents_iter_new`.
///
/// # Safety
///
/// - `iter` must be a pointer previously returned by `sat_graph_documents_iter_new`.
/// - `iter` must not be used after being freed.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn sat_graph_documents_iter_free(iter: *mut DocumentsIter) {
    if iter.is_null() {
        return;
    }

    unsafe {
        let _ = Box::from_raw(iter);
    }
}

/// Attempts to resolve a declaration from a fully-qualified name string.
/// Returns a pointer to the internal ID if it exists, or NULL if it does not.
///
/// # Safety
/// - `pointer` must be a valid `GraphPointer`
/// - `name` must be a valid, null-terminated UTF-8 string
/// - `out_id` must be a valid, writable pointer
///
/// # Panics
///
/// Will panic if acquiring a read lock on the graph's declarations fails
#[unsafe(no_mangle)]
pub unsafe extern "C" fn sat_graph_get_declaration(pointer: GraphPointer, name: *const c_char) -> *const i64 {
    let Ok(name_str) = (unsafe { utils::convert_char_ptr_to_string(name) }) else {
        return ptr::null();
    };

    with_graph(pointer, |graph| {
        // TODO: We should perform name resolution instead of accessing the graph with the canonical ID
        let decl_id = DeclarationId::from(name_str.as_str());
        let read_lock = graph.declarations().read().unwrap();
        if read_lock.contains_key(&decl_id) {
            Box::into_raw(Box::new(*decl_id)).cast_const()
        } else {
            ptr::null()
        }
    })
}

/// Creates a new iterator over constant references by snapshotting the current set of (id, kind) pairs.
///
/// # Safety
/// - `pointer` must be a valid `GraphPointer` previously returned by this crate.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn sat_graph_constant_references_iter_new(pointer: GraphPointer) -> *mut ReferencesIter {
    with_graph(pointer, |graph| {
        let refs: Vec<(i64, ReferenceKind)> = graph
            .constant_references()
            .keys()
            .map(|id| (**id, ReferenceKind::Constant))
            .collect();

        ReferencesIter::new(refs.into_boxed_slice())
    })
}

/// Creates a new iterator over method references by snapshotting the current set of (id, kind) pairs.
///
/// # Safety
/// - `pointer` must be a valid `GraphPointer` previously returned by this crate.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn sat_graph_method_references_iter_new(pointer: GraphPointer) -> *mut ReferencesIter {
    with_graph(pointer, |graph| {
        let refs: Vec<(i64, ReferenceKind)> = graph
            .method_references()
            .keys()
            .map(|id| (**id, ReferenceKind::Method))
            .collect();

        ReferencesIter::new(refs.into_boxed_slice())
    })
}
