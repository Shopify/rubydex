//! C API for exposing references through ID handles (like definitions)

use std::ffi::CString;

use crate::graph_api::{GraphPointer, with_graph};
use crate::location_api::{Location, create_location_for_uri_and_offset};
use libc::c_char;
use rubydex::model::ids::{ConstantReferenceId, MethodReferenceId};

/// Kind of reference for FFI dispatch
#[repr(C)]
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum ReferenceKind {
    Constant = 0,
    Method = 1,
}

#[repr(C)]
#[derive(Debug, Clone, Copy)]
pub struct CReference {
    id: u64,
    kind: ReferenceKind,
}

impl CReference {
    #[must_use]
    pub fn new(id: u64, kind: ReferenceKind) -> Self {
        Self { id, kind }
    }
}

#[derive(Debug)]
pub struct ReferencesIter {
    entries: Box<[CReference]>,
    index: usize,
}

iterator!(ReferencesIter, entries: CReference);

/// # Safety
/// `iter` must be a valid pointer previously returned by `ReferencesIter::new`.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn rdx_references_iter_len(iter: *const ReferencesIter) -> usize {
    unsafe { ReferencesIter::len(iter) }
}

/// # Safety
/// - `iter` must be a valid pointer previously returned by `ReferencesIter::new`.
/// - `out` must be a valid, writable pointer.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn rdx_references_iter_next(iter: *mut ReferencesIter, out: *mut CReference) -> bool {
    unsafe { ReferencesIter::next(iter, out) }
}

/// # Safety
/// - `iter` must be a pointer previously returned by `ReferencesIter::new`.
/// - `iter` must not be used after being freed.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn rdx_references_iter_free(iter: *mut ReferencesIter) {
    unsafe { ReferencesIter::free(iter) }
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
pub unsafe extern "C" fn rdx_constant_reference_name(pointer: GraphPointer, reference_id: u64) -> *const c_char {
    with_graph(pointer, |graph| {
        let ref_id = ConstantReferenceId::new(reference_id);
        let reference = graph.constant_references().get(&ref_id).expect("Reference not found");
        let name = graph.names().get(reference.name_id()).expect("Name ID should exist");

        let name_string = graph
            .strings()
            .get(name.str())
            .expect("String ID should exist")
            .to_string();
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
pub unsafe extern "C" fn rdx_method_reference_name(pointer: GraphPointer, reference_id: u64) -> *const c_char {
    with_graph(pointer, |graph| {
        let ref_id = MethodReferenceId::new(reference_id);
        let reference = graph.method_references().get(&ref_id).expect("Reference not found");
        let name = graph
            .strings()
            .get(reference.str())
            .expect("Name ID should exist")
            .to_string();
        CString::new(name).unwrap().into_raw().cast_const()
    })
}

/// Returns a newly allocated `Location` for the given constant reference id.
/// Caller must free the returned pointer with `rdx_location_free`.
///
/// # Safety
///
/// - `pointer` must be a valid pointer previously returned by `rdx_graph_new`.
/// - `reference_id` must be a valid reference id.
///
/// # Panics
///
/// This function will panic if a reference or document cannot be found.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn rdx_constant_reference_location(pointer: GraphPointer, reference_id: u64) -> *mut Location {
    with_graph(pointer, |graph| {
        let ref_id = ConstantReferenceId::new(reference_id);
        let reference = graph.constant_references().get(&ref_id).expect("Reference not found");
        let document = graph
            .documents()
            .get(&reference.uri_id())
            .expect("Document should exist");

        create_location_for_uri_and_offset(graph, document, reference.offset())
    })
}

/// Returns a newly allocated `Location` for the given method reference id.
/// Caller must free the returned pointer with `rdx_location_free`.
///
/// # Safety
///
/// - `pointer` must be a valid pointer previously returned by `rdx_graph_new`.
/// - `reference_id` must be a valid reference id.
///
/// # Panics
///
/// This function will panic if a reference or document cannot be found.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn rdx_method_reference_location(pointer: GraphPointer, reference_id: u64) -> *mut Location {
    with_graph(pointer, |graph| {
        let ref_id = MethodReferenceId::new(reference_id);
        let reference = graph.method_references().get(&ref_id).expect("Reference not found");
        let document = graph
            .documents()
            .get(&reference.uri_id())
            .expect("Document should exist");

        create_location_for_uri_and_offset(graph, document, reference.offset())
    })
}
