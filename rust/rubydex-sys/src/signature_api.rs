//! C API for method signature accessors

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
    pub location: *mut Location,
    pub kind: ParameterKind,
}

/// C-compatible struct representing a single method signature (a list of parameters).
#[repr(C)]
pub struct SignatureEntry {
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

        let sig_entries = collect_method_signatures(graph, method_def);

        let len = sig_entries.len();
        let items_ptr = Box::into_raw(sig_entries.into_boxed_slice()).cast::<SignatureEntry>();

        Box::into_raw(Box::new(SignatureArray { items: items_ptr, len }))
    })
}

/// Helper: build signature entries from a `MethodDefinition`.
fn collect_method_signatures(graph: &Graph, method_def: &MethodDefinition) -> Vec<SignatureEntry> {
    let uri_id = *method_def.uri_id();
    let document = graph.documents().get(&uri_id).expect("document should exist");

    method_def
        .signatures()
        .as_slice()
        .iter()
        .map(|sig| {
            let param_entries: Vec<ParameterEntry> = sig
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
                .collect();

            let parameters_len = param_entries.len();
            let parameters_ptr = Box::into_raw(param_entries.into_boxed_slice()).cast::<ParameterEntry>();

            SignatureEntry {
                parameters: parameters_ptr,
                parameters_len,
            }
        })
        .collect()
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

    let arr = unsafe { Box::from_raw(ptr) };

    if arr.items.is_null() || arr.len == 0 {
        return;
    }

    let slice_ptr = ptr::slice_from_raw_parts_mut(arr.items, arr.len);
    let sig_slice: Box<[SignatureEntry]> = unsafe { Box::from_raw(slice_ptr) };

    for sig_entry in &*sig_slice {
        if sig_entry.parameters.is_null() || sig_entry.parameters_len == 0 {
            continue;
        }

        let param_slice_ptr = ptr::slice_from_raw_parts_mut(sig_entry.parameters, sig_entry.parameters_len);
        let param_slice: Box<[ParameterEntry]> = unsafe { Box::from_raw(param_slice_ptr) };

        for param_entry in &*param_slice {
            if !param_entry.name.is_null() {
                drop(unsafe { CString::from_raw(param_entry.name.cast_mut()) });
            }
            if !param_entry.location.is_null() {
                unsafe { crate::location_api::rdx_location_free(param_entry.location) };
            }
        }
    }
}
