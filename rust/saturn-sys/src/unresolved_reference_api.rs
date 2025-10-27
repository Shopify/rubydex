//! C API for exposing unresolved references through ID handles (like definitions)

use std::ffi::CString;

use crate::graph_api::{GraphPointer, with_graph};
use crate::location_api::{Location, create_location_for_uri_and_offset};
use libc::c_char;
use saturn::model::ids::ReferenceId;
use saturn::model::references::UnresolvedReference;

/// Kind of unresolved reference for FFI dispatch
#[repr(C)]
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum UnresolvedReferenceKind {
    Constant = 0,
    Method = 1,
}

/// Shared iterator over unresolved reference (id, kind) pairs
#[derive(Debug)]
pub struct UnresolvedReferencesIter {
    pub entries: Box<[(i64, UnresolvedReferenceKind)]>,
    pub index: usize,
}

impl UnresolvedReferencesIter {
    #[must_use]
    pub fn new(entries: Box<[(i64, UnresolvedReferenceKind)]>) -> *mut UnresolvedReferencesIter {
        Box::into_raw(Box::new(UnresolvedReferencesIter { entries, index: 0 }))
    }
}

/// Returns the total number of entries in the iterator snapshot.
///
/// # Safety
/// - `iter` must be a valid pointer previously returned by `UnresolvedReferencesIter::new`.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn sat_unresolved_references_iter_len(iter: *const UnresolvedReferencesIter) -> usize {
    if iter.is_null() {
        return 0;
    }
    unsafe { (&*iter).entries.len() }
}

/// Advances the iterator and writes the next entry into the provided out params.
/// Returns `true` if an entry was written, or `false` if the iterator is exhausted or inputs are invalid.
///
/// # Safety
/// - `iter` must be a valid pointer previously returned by `UnresolvedReferencesIter::new`.
/// - `out_id` and `out_kind` must be valid, writable pointers.
///
/// # Panics
/// - If the iterator is exhausted or inputs are invalid.
/// - If the name, URI, start, or end pointers are invalid.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn sat_unresolved_references_iter_next(
    iter: *mut UnresolvedReferencesIter,
    out_id: *mut i64,
    out_kind: *mut UnresolvedReferenceKind,
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

/// Frees an iterator created by `UnresolvedReferencesIter::new`.
///
/// # Safety
/// - `iter` must be a pointer previously returned by `UnresolvedReferencesIter::new`.
/// - `iter` must not be used after being freed.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn sat_unresolved_references_iter_free(iter: *mut UnresolvedReferencesIter) {
    if iter.is_null() {
        return;
    }
    unsafe {
        let _ = Box::from_raw(iter);
    }
}

/// Returns the UTF-8 name string for a reference id.
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
pub unsafe extern "C" fn sat_unresolved_reference_name(pointer: GraphPointer, reference_id: i64) -> *const c_char {
    with_graph(pointer, |graph| {
        let ref_id = ReferenceId::new(reference_id);
        let name = match graph.unresolved_references().get(&ref_id).expect("Reference not found") {
            UnresolvedReference::Constant(c) => graph.names().get(c.name_id()).cloned().unwrap_or_default(),
            UnresolvedReference::Method(m) => graph.names().get(m.name_id()).cloned().unwrap_or_default(),
        };
        CString::new(name).unwrap().into_raw().cast_const()
    })
}

/// Returns a newly allocated `Location` for the given reference id.
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
pub unsafe extern "C" fn sat_unresolved_reference_location(pointer: GraphPointer, reference_id: i64) -> *mut Location {
    with_graph(pointer, |graph| {
        let ref_id = ReferenceId::new(reference_id);
        match graph.unresolved_references().get(&ref_id).expect("Reference not found") {
            UnresolvedReference::Constant(c) => {
                let uri = graph
                    .documents()
                    .get(&c.uri_id())
                    .map(|d| d.uri().to_string())
                    .unwrap_or_default();
                create_location_for_uri_and_offset(&uri, c.offset().start(), c.offset().end())
            }
            UnresolvedReference::Method(m) => {
                let uri = graph
                    .documents()
                    .get(&m.uri_id())
                    .map(|d| d.uri().to_string())
                    .unwrap_or_default();
                create_location_for_uri_and_offset(&uri, m.offset().start(), m.offset().end())
            }
        }
    })
}
