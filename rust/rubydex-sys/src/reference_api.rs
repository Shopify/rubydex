//! C API for exposing references through ID handles (like definitions)

use std::ffi::CString;

use crate::graph_api::{GraphPointer, with_graph};
use crate::location_api::{Location, create_location_for_uri_and_offset};
use libc::c_char;
use rubydex::model::ids::ReferenceId;

/// Kind of reference for FFI dispatch
#[repr(C)]
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum ReferenceKind {
    Constant = 0,
    Method = 1,
}

/// Shared iterator over reference (id, kind) pairs
#[derive(Debug)]
pub struct ReferencesIter {
    pub entries: Box<[(i64, ReferenceKind)]>,
    pub index: usize,
}

impl ReferencesIter {
    #[must_use]
    pub fn new(entries: Box<[(i64, ReferenceKind)]>) -> *mut ReferencesIter {
        Box::into_raw(Box::new(ReferencesIter { entries, index: 0 }))
    }
}

/// Returns the total number of entries in the iterator snapshot.
///
/// # Safety
/// - `iter` must be a valid pointer previously returned by `ReferencesIter::new`.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn sat_references_iter_len(iter: *const ReferencesIter) -> usize {
    if iter.is_null() {
        return 0;
    }
    unsafe { (&*iter).entries.len() }
}

/// Advances the iterator and writes the next entry into the provided out params.
/// Returns `true` if an entry was written, or `false` if the iterator is exhausted or inputs are invalid.
///
/// # Safety
/// - `iter` must be a valid pointer previously returned by `ReferencesIter::new`.
/// - `out_id` and `out_kind` must be valid, writable pointers.
///
/// # Panics
/// - If the iterator is exhausted or inputs are invalid.
/// - If the name, URI, start, or end pointers are invalid.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn sat_references_iter_next(
    iter: *mut ReferencesIter,
    out_id: *mut i64,
    out_kind: *mut ReferenceKind,
) -> bool {
    if iter.is_null() || out_id.is_null() || out_kind.is_null() {
        return false;
    }

    let it = unsafe { &mut *iter };
    if it.index >= it.entries.len() {
        return false;
    }

    let (id, kind) = it.entries[it.index];
    it.index += 1;
    unsafe {
        *out_id = id;
        *out_kind = kind;
    }
    true
}

/// Frees an iterator created by `ReferencesIter::new`.
///
/// # Safety
/// - `iter` must be a pointer previously returned by `ReferencesIter::new`.
/// - `iter` must not be used after being freed.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn sat_references_iter_free(iter: *mut ReferencesIter) {
    if iter.is_null() {
        return;
    }
    unsafe {
        let _ = Box::from_raw(iter);
    }
}

/// Returns the UTF-8 name string for a constant reference id.
/// Caller must free with `free_c_string`.
///
/// # Safety
///
/// Assumes pointer is valid.
///
/// # Panics
///
/// This function will panic if the reference cannot be found.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn sat_constant_reference_name(pointer: GraphPointer, reference_id: i64) -> *const c_char {
    with_graph(pointer, |graph| {
        let ref_id = ReferenceId::new(reference_id);
        let reference = graph.constant_references().get(&ref_id).expect("Reference not found");
        let name = graph.names().get(reference.name_id()).expect("Name ID should exist");

        let name_string = graph.strings().get(name.str()).expect("String ID should exist").clone();
        CString::new(name_string).unwrap().into_raw().cast_const()
    })
}

/// Returns the UTF-8 name string for a method reference id.
/// Caller must free with `free_c_string`.
///
/// # Safety
///
/// Assumes pointer is valid.
///
/// # Panics
///
/// This function will panic if the reference cannot be found.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn sat_method_reference_name(pointer: GraphPointer, reference_id: i64) -> *const c_char {
    with_graph(pointer, |graph| {
        let ref_id = ReferenceId::new(reference_id);
        let reference = graph.method_references().get(&ref_id).expect("Reference not found");
        let name = graph
            .strings()
            .get(reference.str())
            .expect("Name ID should exist")
            .clone();
        CString::new(name).unwrap().into_raw().cast_const()
    })
}

/// Returns a newly allocated `Location` for the given constant reference id.
/// Caller must free the returned pointer with `sat_location_free`.
///
/// # Safety
///
/// - `pointer` must be a valid pointer previously returned by `sat_graph_new`.
/// - `reference_id` must be a valid reference id.
///
/// # Panics
///
/// This function will panic if a reference or document cannot be found.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn sat_constant_reference_location(pointer: GraphPointer, reference_id: i64) -> *mut Location {
    with_graph(pointer, |graph| {
        let ref_id = ReferenceId::new(reference_id);
        let reference = graph.constant_references().get(&ref_id).expect("Reference not found");
        let uri = graph
            .documents()
            .get(&reference.uri_id())
            .expect("Document should exist")
            .uri()
            .to_string();
        create_location_for_uri_and_offset(&uri, reference.offset())
    })
}

/// Returns a newly allocated `Location` for the given method reference id.
/// Caller must free the returned pointer with `sat_location_free`.
///
/// # Safety
///
/// - `pointer` must be a valid pointer previously returned by `sat_graph_new`.
/// - `reference_id` must be a valid reference id.
///
/// # Panics
///
/// This function will panic if a reference or document cannot be found.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn sat_method_reference_location(pointer: GraphPointer, reference_id: i64) -> *mut Location {
    with_graph(pointer, |graph| {
        let ref_id = ReferenceId::new(reference_id);
        let reference = graph.method_references().get(&ref_id).expect("Reference not found");
        let uri = graph
            .documents()
            .get(&reference.uri_id())
            .expect("Document should exist")
            .uri()
            .to_string();
        create_location_for_uri_and_offset(&uri, reference.offset())
    })
}
