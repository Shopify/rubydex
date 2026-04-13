//! This file provides the C API for method signature accessors

use crate::graph_api::{GraphPointer, with_graph};
use crate::location_api::{Location, create_location_for_uri_and_offset};
use libc::c_char;
use rubydex::model::definitions::{Definition, MethodDefinition, Parameter};
use rubydex::model::graph::Graph;
use rubydex::model::ids::DefinitionId;
use std::ffi::CString;
use std::ptr;

/// C-compatible enum representing the kind of a parameter.
#[repr(C)]
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum ParameterKind {
    RequiredPositional = 0,
    OptionalPositional = 1,
    RestPositional = 2,
    Post = 3,
    RequiredKeyword = 4,
    OptionalKeyword = 5,
    RestKeyword = 6,
    Forward = 7,
    Block = 8,
}

fn map_parameter_kind(param: &Parameter) -> ParameterKind {
    match param {
        Parameter::RequiredPositional(_) => ParameterKind::RequiredPositional,
        Parameter::Post(_) => ParameterKind::Post,
        Parameter::OptionalPositional(_) => ParameterKind::OptionalPositional,
        Parameter::RestPositional(_) => ParameterKind::RestPositional,
        Parameter::RequiredKeyword(_) => ParameterKind::RequiredKeyword,
        Parameter::OptionalKeyword(_) => ParameterKind::OptionalKeyword,
        Parameter::RestKeyword(_) => ParameterKind::RestKeyword,
        Parameter::Block(_) => ParameterKind::Block,
        Parameter::Forward(_) => ParameterKind::Forward,
    }
}

/// C-compatible struct representing a single parameter with its name, kind, and location.
#[repr(C)]
pub struct ParameterEntry {
    pub name: *const c_char,
    pub kind: ParameterKind,
    pub location: *mut Location,
}

/// C-compatible struct representing a single method signature (a list of parameters).
#[repr(C)]
pub struct SignatureEntry {
    pub definition_id: u64,
    pub parameters: *mut ParameterEntry,
    pub parameters_len: usize,
}

/// C-compatible array of signatures.
#[repr(C)]
pub struct SignatureArray {
    pub items: *mut SignatureEntry,
    pub len: usize,
}

/// Returns a newly allocated array of signatures for the given method definition id.
/// Caller must free the returned pointer with `rdx_definition_signatures_free`.
///
/// # Safety
/// - `pointer` must be a valid pointer previously returned by `rdx_graph_new`.
/// - `definition_id` must be a valid definition id.
///
/// # Panics
/// Panics if `definition_id` does not exist or is not a `MethodDefinition`.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn rdx_definition_signatures(pointer: GraphPointer, definition_id: u64) -> *mut SignatureArray {
    with_graph(pointer, |graph| {
        let def_id = DefinitionId::new(definition_id);
        let Definition::Method(method_def) = graph.definitions().get(&def_id).expect("definition should exist") else {
            panic!("expected a method definition");
        };

        let mut sig_entries: Vec<SignatureEntry> = Vec::new();
        collect_method_signatures(graph, method_def, definition_id, &mut sig_entries);

        let mut boxed = sig_entries.into_boxed_slice();
        let len = boxed.len();
        let items_ptr = boxed.as_mut_ptr();
        std::mem::forget(boxed);

        Box::into_raw(Box::new(SignatureArray { items: items_ptr, len }))
    })
}

/// Helper: build signature entries from a `MethodDefinition` and append them to the output vector.
fn collect_method_signatures(
    graph: &Graph,
    method_def: &MethodDefinition,
    definition_id: u64,
    out: &mut Vec<SignatureEntry>,
) {
    let uri_id = *method_def.uri_id();
    let document = graph.documents().get(&uri_id).expect("document should exist");

    for sig in method_def.signatures().as_slice() {
        let mut param_entries = sig
            .iter()
            .map(|param| {
                let param_struct = param.inner();
                let name = graph
                    .strings()
                    .get(param_struct.str())
                    .expect("parameter name string should exist");
                let name_str = CString::new(name.as_str()).unwrap().into_raw().cast_const();

                ParameterEntry {
                    name: name_str,
                    kind: map_parameter_kind(param),
                    location: create_location_for_uri_and_offset(graph, document, param_struct.offset()),
                }
            })
            .collect::<Vec<_>>()
            .into_boxed_slice();

        let parameters_len = param_entries.len();
        let parameters_ptr = param_entries.as_mut_ptr();
        std::mem::forget(param_entries);

        out.push(SignatureEntry {
            definition_id,
            parameters: parameters_ptr,
            parameters_len,
        });
    }
}

/// Returns signatures for a `MethodAliasDefinition` by following the alias chain.
/// Always returns a valid `SignatureArray` pointer (possibly with `len == 0`).
/// Errors during alias resolution (unresolved receivers, circular chains, etc.)
/// are silently ignored.
///
/// # Safety
/// - `pointer` must be a valid pointer previously returned by `rdx_graph_new`.
/// - `definition_id` must be a valid definition id.
///
/// # Panics
/// Panics if `definition_id` does not exist or is not a `MethodAliasDefinition`.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn rdx_method_alias_definition_signatures(
    pointer: GraphPointer,
    definition_id: u64,
) -> *mut SignatureArray {
    with_graph(pointer, |graph| {
        let def_id = DefinitionId::new(definition_id);

        let resolved = rubydex::query::deep_dealias_method(graph, def_id);

        let mut sig_entries: Vec<SignatureEntry> = Vec::new();
        for id in &resolved {
            if let Some(Definition::Method(method_def)) = graph.definitions().get(id) {
                collect_method_signatures(graph, method_def, id.get(), &mut sig_entries);
            }
        }

        let mut boxed = sig_entries.into_boxed_slice();
        let len = boxed.len();
        let items_ptr = boxed.as_mut_ptr();
        std::mem::forget(boxed);

        Box::into_raw(Box::new(SignatureArray { items: items_ptr, len }))
    })
}

/// Frees a `SignatureArray` previously returned by `rdx_definition_signatures`.
///
/// # Safety
/// - `ptr` must be a valid pointer previously returned by `rdx_definition_signatures`.
/// - `ptr` must not be used after being freed.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn rdx_definition_signatures_free(ptr: *mut SignatureArray) {
    if ptr.is_null() {
        return;
    }

    // Take ownership of the SignatureArray
    let arr = unsafe { Box::from_raw(ptr) };

    if !arr.items.is_null() && arr.len > 0 {
        // Reconstruct the boxed slice so we can drop it after freeing inner allocations
        let slice_ptr = ptr::slice_from_raw_parts_mut(arr.items, arr.len);
        let mut sig_slice: Box<[SignatureEntry]> = unsafe { Box::from_raw(slice_ptr) };

        for sig_entry in &mut sig_slice {
            if !sig_entry.parameters.is_null() && sig_entry.parameters_len > 0 {
                let param_slice_ptr = ptr::slice_from_raw_parts_mut(sig_entry.parameters, sig_entry.parameters_len);
                let mut param_slice: Box<[ParameterEntry]> = unsafe { Box::from_raw(param_slice_ptr) };

                for param_entry in &mut param_slice {
                    if !param_entry.name.is_null() {
                        let _ = unsafe { CString::from_raw(param_entry.name.cast_mut()) };
                    }
                    if !param_entry.location.is_null() {
                        unsafe { crate::location_api::rdx_location_free(param_entry.location) };
                        param_entry.location = ptr::null_mut();
                    }
                }
                // param_slice is dropped here, freeing the parameters buffer
            }
        }
        // sig_slice is dropped here, freeing the signatures buffer
    }
    // arr is dropped here
}
