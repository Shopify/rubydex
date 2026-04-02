//! C API for exposing references through ID handles (like definitions)

use std::ffi::CString;
use std::ptr;

use crate::declaration_api::CDeclaration;
use crate::graph_api::{GraphPointer, with_graph};
use crate::location_api::{Location, create_location_for_uri_and_offset};
use libc::c_char;
use rubydex::model::graph::Graph;
use rubydex::model::ids::{ConstantReferenceId, MethodReferenceId};
use rubydex::model::name::NameRef;

#[repr(C)]
#[derive(Debug, Clone, Copy)]
pub struct CConstantReference {
    pub id: u64,
    pub declaration_id: u64,
}

impl CConstantReference {
    /// Build a `CConstantReference` from a graph and reference ID. Sets `declaration_id` to 0 when the reference is
    /// unresolved.
    ///
    /// # Panics
    ///
    /// This function will panic if there's inconsistent data in the graph
    #[must_use]
    pub fn from_id(graph: &Graph, ref_id: ConstantReferenceId) -> Self {
        let reference = graph
            .constant_references()
            .get(&ref_id)
            .expect("Constant reference not found");

        let name_ref = graph.names().get(reference.name_id()).expect("Name ID should exist");

        let declaration_id = match name_ref {
            NameRef::Resolved(resolved) => **resolved.declaration_id(),
            NameRef::Unresolved(_) => 0,
        };

        Self {
            id: *ref_id,
            declaration_id,
        }
    }
}

#[derive(Debug)]
pub struct ConstantReferencesIter {
    entries: Box<[CConstantReference]>,
    index: usize,
}

iterator!(ConstantReferencesIter, entries: CConstantReference);

/// # Safety
/// `iter` must be a valid pointer previously returned by `ConstantReferencesIter::new`.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn rdx_constant_references_iter_len(iter: *const ConstantReferencesIter) -> usize {
    unsafe { ConstantReferencesIter::len(iter) }
}

/// # Safety
/// - `iter` must be a valid pointer previously returned by `ConstantReferencesIter::new`.
/// - `out` must be a valid, writable pointer.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn rdx_constant_references_iter_next(
    iter: *mut ConstantReferencesIter,
    out: *mut CConstantReference,
) -> bool {
    unsafe { ConstantReferencesIter::next(iter, out) }
}

/// # Safety
/// - `iter` must be a pointer previously returned by `ConstantReferencesIter::new`.
/// - `iter` must not be used after being freed.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn rdx_constant_references_iter_free(iter: *mut ConstantReferencesIter) {
    unsafe { ConstantReferencesIter::free(iter) }
}

#[repr(C)]
#[derive(Debug, Clone, Copy)]
pub struct CMethodReference {
    pub id: u64,
}

#[derive(Debug)]
pub struct MethodReferencesIter {
    entries: Box<[CMethodReference]>,
    index: usize,
}

iterator!(MethodReferencesIter, entries: CMethodReference);

/// # Safety
/// `iter` must be a valid pointer previously returned by `MethodReferencesIter::new`.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn rdx_method_references_iter_len(iter: *const MethodReferencesIter) -> usize {
    unsafe { MethodReferencesIter::len(iter) }
}

/// # Safety
/// - `iter` must be a valid pointer previously returned by `MethodReferencesIter::new`.
/// - `out` must be a valid, writable pointer.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn rdx_method_references_iter_next(
    iter: *mut MethodReferencesIter,
    out: *mut CMethodReference,
) -> bool {
    unsafe { MethodReferencesIter::next(iter, out) }
}

/// # Safety
/// - `iter` must be a pointer previously returned by `MethodReferencesIter::new`.
/// - `iter` must not be used after being freed.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn rdx_method_references_iter_free(iter: *mut MethodReferencesIter) {
    unsafe { MethodReferencesIter::free(iter) }
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

/// Returns the declaration that the given resolved constant reference points to. Returns NULL if the reference is
/// unresolved. Caller must free with `free_c_declaration`.
///
/// # Safety
///
/// Assumes pointer is valid.
///
/// # Panics
///
/// This function will panic if the reference cannot be found.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn rdx_resolved_constant_reference_declaration(
    pointer: GraphPointer,
    reference_id: u64,
) -> *const CDeclaration {
    with_graph(pointer, |graph| {
        let ref_id = ConstantReferenceId::new(reference_id);
        let reference = graph.constant_references().get(&ref_id).expect("Reference not found");
        let name_ref = graph.names().get(reference.name_id()).expect("Name ID should exist");

        match name_ref {
            NameRef::Resolved(resolved) => {
                let decl_id = *resolved.declaration_id();
                let decl = graph.declarations().get(&decl_id).expect("Declaration not found");
                Box::into_raw(Box::new(CDeclaration::from_declaration(decl_id, decl))).cast_const()
            }
            NameRef::Unresolved(_) => ptr::null(),
        }
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

/// Frees a `CConstantReference` previously returned by an FFI function.
///
/// # Safety
/// - `ptr` must be a valid pointer previously returned by an FFI function that allocates a `CConstantReference`, or
///   NULL.
/// - `ptr` must not be used after being freed.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn free_c_constant_reference(ptr: *const CConstantReference) {
    if !ptr.is_null() {
        unsafe {
            let _ = Box::from_raw(ptr.cast_mut());
        }
    }
}
