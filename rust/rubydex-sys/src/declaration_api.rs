//! This file provides the C API for the Graph object

use libc::c_char;
use std::ffi::CString;
use std::ptr;

use crate::definition_api::{DefinitionKind, DefinitionsIter, rdx_definitions_iter_new_from_ids};
use crate::graph_api::{GraphPointer, with_graph};
use rubydex::model::declaration::Ancestor;
use rubydex::model::ids::DeclarationId;

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
pub unsafe extern "C" fn rdx_declaration_name(pointer: GraphPointer, name_id: i64) -> *const c_char {
    with_graph(pointer, |graph| {
        let name_id = DeclarationId::new(name_id);
        if let Some(decl) = graph.declarations().get(&name_id) {
            CString::new(decl.name()).unwrap().into_raw().cast_const()
        } else {
            ptr::null()
        }
    })
}

/// Returns the UTF-8 unqualified name string for a declaration id.
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
pub unsafe extern "C" fn rdx_declaration_unqualified_name(pointer: GraphPointer, name_id: i64) -> *const c_char {
    with_graph(pointer, |graph| {
        let name_id = DeclarationId::new(name_id);
        if let Some(decl) = graph.declarations().get(&name_id) {
            CString::new(decl.unqualified_name()).unwrap().into_raw().cast_const()
        } else {
            ptr::null()
        }
    })
}

/// An iterator over definition IDs and kinds for a given declaration
///
/// We snapshot the IDs at iterator creation so if the graph is modified, the iterator will not see the changes
// Use shared DefinitionsIter directly in signatures
/// Creates a new iterator over definition IDs for a given declaration by snapshotting the current set of IDs.
///
/// # Safety
///
/// - `pointer` must be a valid `GraphPointer` previously returned by this crate.
/// - The returned pointer must be freed with `rdx_declaration_definitions_iter_free`.
///
/// # Panics
///
/// This function will panic if acquiring a read lock fails
#[unsafe(no_mangle)]
pub unsafe extern "C" fn rdx_declaration_definitions_iter_new(
    pointer: GraphPointer,
    decl_id: i64,
) -> *mut DefinitionsIter {
    // Snapshot the IDs and kinds at iterator creation to avoid borrowing across FFI calls
    with_graph(pointer, |graph| {
        let decl_id = DeclarationId::new(decl_id);
        if let Some(decl) = graph.declarations().get(&decl_id) {
            rdx_definitions_iter_new_from_ids(graph, decl.definitions())
        } else {
            DefinitionsIter::new(Vec::<(i64, DefinitionKind)>::new().into_boxed_slice())
        }
    })
}

/// Iterator over member declaration IDs for a namespace
pub struct MembersIter {
    ids: Box<[i64]>,
    index: usize,
}

/// Creates a new iterator over member declaration IDs for a given namespace declaration.
///
/// # Safety
///
/// - `pointer` must be a valid `GraphPointer`
/// - The returned pointer must be freed with `rdx_members_iter_free`
#[unsafe(no_mangle)]
pub unsafe extern "C" fn rdx_declaration_members_iter_new(
    pointer: GraphPointer,
    decl_id: i64,
) -> *mut MembersIter {
    with_graph(pointer, |graph| {
        let decl_id = DeclarationId::new(decl_id);
        let members: Vec<i64> = if let Some(decl) = graph.declarations().get(&decl_id) {
            if let Some(namespace) = decl.as_namespace() {
                namespace.members().values().map(|id| **id).collect()
            } else {
                Vec::new()
            }
        } else {
            Vec::new()
        };
        Box::into_raw(Box::new(MembersIter {
            ids: members.into_boxed_slice(),
            index: 0,
        }))
    })
}

/// Advances the iterator and writes the next ID into `out_id`.
/// Returns `true` if an ID was written, `false` if exhausted.
///
/// # Safety
/// - `iter` must be valid pointer from `rdx_declaration_members_iter_new`
/// - `out_id` must be a valid, writable pointer
#[unsafe(no_mangle)]
pub unsafe extern "C" fn rdx_members_iter_next(iter: *mut MembersIter, out_id: *mut i64) -> bool {
    if iter.is_null() || out_id.is_null() {
        return false;
    }
    let it = unsafe { &mut *iter };
    if it.index >= it.ids.len() {
        return false;
    }
    unsafe { *out_id = it.ids[it.index] };
    it.index += 1;
    true
}

/// Returns the total number of members in the iterator.
///
/// # Safety
/// - `iter` must be valid pointer from `rdx_declaration_members_iter_new`
#[unsafe(no_mangle)]
pub unsafe extern "C" fn rdx_members_iter_len(iter: *const MembersIter) -> usize {
    if iter.is_null() {
        0
    } else {
        unsafe { (&(*iter).ids).len() }
    }
}

/// Frees an iterator created by `rdx_declaration_members_iter_new`.
///
/// # Safety
/// - `iter` must be a pointer from `rdx_declaration_members_iter_new`
/// - `iter` must not be used after being freed
#[unsafe(no_mangle)]
pub unsafe extern "C" fn rdx_members_iter_free(iter: *mut MembersIter) {
    if !iter.is_null() {
        unsafe {
            let _ = Box::from_raw(iter);
        }
    }
}

/// Iterator over ancestor declaration IDs for a namespace
pub struct AncestorsIter {
    ids: Box<[i64]>,
    index: usize,
}

/// Creates a new iterator over ancestor declaration IDs for a given namespace declaration.
/// Only returns Complete ancestors (resolved declaration IDs), skipping Partial ones.
/// This computes ancestors on-demand if they haven't been computed yet.
///
/// # Safety
///
/// - `pointer` must be a valid `GraphPointer`
/// - The returned pointer must be freed with `rdx_ancestors_iter_free`
#[unsafe(no_mangle)]
pub unsafe extern "C" fn rdx_declaration_ancestors_iter_new(
    pointer: GraphPointer,
    decl_id: i64,
) -> *mut AncestorsIter {
    with_graph(pointer, |graph| {
        let decl_id = DeclarationId::new(decl_id);
        // Get ancestors from the declaration (cached after resolution)
        let ancestors: Vec<i64> = if let Some(decl) = graph.declarations().get(&decl_id) {
            if let Some(namespace) = decl.as_namespace() {
                namespace
                    .ancestors()
                    .iter()
                    .filter_map(|ancestor| {
                        if let Ancestor::Complete(id) = ancestor {
                            Some(**id)
                        } else {
                            None
                        }
                    })
                    .collect()
            } else {
                Vec::new()
            }
        } else {
            Vec::new()
        };
        Box::into_raw(Box::new(AncestorsIter {
            ids: ancestors.into_boxed_slice(),
            index: 0,
        }))
    })
}

/// Advances the iterator and writes the next ID into `out_id`.
/// Returns `true` if an ID was written, `false` if exhausted.
///
/// # Safety
/// - `iter` must be valid pointer from `rdx_declaration_ancestors_iter_new`
/// - `out_id` must be a valid, writable pointer
#[unsafe(no_mangle)]
pub unsafe extern "C" fn rdx_ancestors_iter_next(iter: *mut AncestorsIter, out_id: *mut i64) -> bool {
    if iter.is_null() || out_id.is_null() {
        return false;
    }
    let it = unsafe { &mut *iter };
    if it.index >= it.ids.len() {
        return false;
    }
    unsafe { *out_id = it.ids[it.index] };
    it.index += 1;
    true
}

/// Returns the total number of ancestors in the iterator.
///
/// # Safety
/// - `iter` must be valid pointer from `rdx_declaration_ancestors_iter_new`
#[unsafe(no_mangle)]
pub unsafe extern "C" fn rdx_ancestors_iter_len(iter: *const AncestorsIter) -> usize {
    if iter.is_null() {
        0
    } else {
        unsafe { (&(*iter).ids).len() }
    }
}

/// Frees an iterator created by `rdx_declaration_ancestors_iter_new`.
///
/// # Safety
/// - `iter` must be a pointer from `rdx_declaration_ancestors_iter_new`
/// - `iter` must not be used after being freed
#[unsafe(no_mangle)]
pub unsafe extern "C" fn rdx_ancestors_iter_free(iter: *mut AncestorsIter) {
    if !iter.is_null() {
        unsafe {
            let _ = Box::from_raw(iter);
        }
    }
}
