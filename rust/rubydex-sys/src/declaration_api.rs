//! This file provides the C API for the Graph object

use libc::c_char;
use rubydex::model::declaration::Declaration;
use std::ffi::CString;
use std::ptr;

use crate::definition_api::{DefinitionKind, DefinitionsIter, rdx_definitions_iter_new_from_ids};
use crate::graph_api::{GraphPointer, with_graph};
use crate::utils;
use rubydex::model::ids::{DeclarationId, StringId};

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
pub unsafe extern "C" fn rdx_declaration_name(pointer: GraphPointer, name_id: u32) -> *const c_char {
    with_graph(pointer, |graph| {
        let name_id = DeclarationId::new(name_id);
        if let Some(decl) = graph.declarations().get(&name_id) {
            CString::new(decl.name()).unwrap().into_raw().cast_const()
        } else {
            ptr::null()
        }
    })
}

/// Returns the declaration ID for a member from a declaration.
/// Returns NULL if the member is not found.
///
/// # Safety
/// - `member` must be a valid, null-terminated UTF-8 string
#[unsafe(no_mangle)]
pub unsafe extern "C" fn rdx_declaration_member(
    pointer: GraphPointer,
    name_id: u32,
    member: *const c_char,
) -> *const u32 {
    let Ok(member_str) = (unsafe { utils::convert_char_ptr_to_string(member) }) else {
        return ptr::null();
    };

    with_graph(pointer, |graph| {
        let name_id = DeclarationId::new(name_id);
        if let Some(Declaration::Namespace(decl)) = graph.declarations().get(&name_id) {
            let member_id = StringId::from(member_str.as_str());

            if let Some(member_decl_id) = decl.member(&member_id) {
                return Box::into_raw(Box::new(**member_decl_id)).cast_const();
            }
        }

        ptr::null()
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
pub unsafe extern "C" fn rdx_declaration_unqualified_name(pointer: GraphPointer, name_id: u32) -> *const c_char {
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
#[unsafe(no_mangle)]
pub unsafe extern "C" fn rdx_declaration_definitions_iter_new(
    pointer: GraphPointer,
    decl_id: u32,
) -> *mut DefinitionsIter {
    // Snapshot the IDs and kinds at iterator creation to avoid borrowing across FFI calls
    with_graph(pointer, |graph| {
        let decl_id = DeclarationId::new(decl_id);
        if let Some(decl) = graph.declarations().get(&decl_id) {
            rdx_definitions_iter_new_from_ids(graph, decl.definitions())
        } else {
            DefinitionsIter::new(Vec::<(u32, DefinitionKind)>::new().into_boxed_slice())
        }
    })
}

/// Returns the declaration for the singleton class of the declaration
///
/// # Safety
///
/// Assumes pointer is valid
///
/// # Panics
///
/// Will panic if invoked on a non-existing or non-namespace declaration
#[unsafe(no_mangle)]
pub unsafe extern "C" fn rdx_declaration_singleton_class(pointer: GraphPointer, decl_id: u32) -> *const u32 {
    with_graph(pointer, |graph| {
        let declaration = graph
            .declarations()
            .get(&DeclarationId::new(decl_id))
            .unwrap()
            .as_namespace()
            .unwrap();

        if let Some(singleton_id) = declaration.singleton_class() {
            Box::into_raw(Box::new(**singleton_id)).cast_const()
        } else {
            ptr::null()
        }
    })
}

/// Returns the owner of the declaration (attached object in the case of singleton classes)
///
/// # Safety
///
/// Assumes pointer is valid
///
/// # Panics
///
/// Will panic if invoked on a non-existing or non-namespace declaration
#[unsafe(no_mangle)]
pub unsafe extern "C" fn rdx_declaration_owner(pointer: GraphPointer, decl_id: u32) -> *const u32 {
    with_graph(pointer, |graph| {
        let declaration = graph.declarations().get(&DeclarationId::new(decl_id)).unwrap();
        Box::into_raw(Box::new(**declaration.owner_id())).cast_const()
    })
}
