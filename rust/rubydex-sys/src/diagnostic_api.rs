//! Diagnostics-related FFI helpers

use crate::graph_api::{GraphPointer, with_graph};
use crate::location_api::{Location, create_location_for_uri_and_offset};
use libc::c_char;
use std::{ffi::CString, mem, ptr};

/// C-compatible struct representing a diagnostic entry.
#[repr(C)]
pub struct DiagnosticEntry {
    pub rule: *const c_char,
    pub message: *const c_char,
    pub location: *mut Location,
}

/// C-compatible array wrapper for diagnostics.
#[repr(C)]
pub struct DiagnosticArray {
    pub items: *mut DiagnosticEntry,
    pub len: usize,
}

impl DiagnosticArray {
    fn from_vec(mut entries: Vec<DiagnosticEntry>) -> *mut DiagnosticArray {
        let len = entries.len();
        let ptr = entries.as_mut_ptr();
        mem::forget(entries);
        Box::into_raw(Box::new(DiagnosticArray { items: ptr, len }))
    }
}

/// Returns all diagnostics currently recorded in the global graph.
///
/// # Safety
///
/// - `pointer` must be a valid `GraphPointer` previously returned by this crate.
/// - The pointed graph must remain alive for the duration of the call.
///
/// # Panics
///
/// - If a diagnostic references a URI whose file cannot be read to build a location.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn rdx_graph_diagnostics(pointer: GraphPointer) -> *mut DiagnosticArray {
    with_graph(pointer, |graph| {
        let entries = graph
            .diagnostics()
            .iter()
            .map(|diagnostic| {
                let uri = graph.documents().get(diagnostic.uri_id()).unwrap().uri();
                let location = create_location_for_uri_and_offset(uri, diagnostic.offset());

                DiagnosticEntry {
                    rule: CString::new(diagnostic.rule().to_string())
                        .unwrap()
                        .into_raw()
                        .cast_const(),
                    message: CString::new(diagnostic.message()).unwrap().into_raw().cast_const(),
                    location,
                }
            })
            .collect::<Vec<DiagnosticEntry>>();

        DiagnosticArray::from_vec(entries)
    })
}

/// Frees a diagnostic array previously returned by `rdx_graph_diagnostics`.
///
/// # Safety
///
/// - `ptr` must be a valid pointer previously returned by `rdx_graph_diagnostics`.
/// - `ptr` must not be used after being freed.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn rdx_diagnostics_free(ptr: *mut DiagnosticArray) {
    if ptr.is_null() {
        return;
    }

    let array = unsafe { Box::from_raw(ptr) };
    if !array.items.is_null() && array.len > 0 {
        let slice_ptr = ptr::slice_from_raw_parts_mut(array.items, array.len);
        let mut boxed_slice: Box<[DiagnosticEntry]> = unsafe { Box::from_raw(slice_ptr) };

        for entry in &mut *boxed_slice {
            if !entry.rule.is_null() {
                let _ = unsafe { CString::from_raw(entry.rule.cast_mut()) };
            }
            if !entry.message.is_null() {
                let _ = unsafe { CString::from_raw(entry.message.cast_mut()) };
            }
            if !entry.location.is_null() {
                unsafe { crate::location_api::rdx_location_free(entry.location) };
                entry.location = ptr::null_mut();
            }
        }
        // boxed_slice drops here, releasing the buffer
    }
    // array drops here
}
