//! This file provides the C API for Document accessors

use libc::c_char;
use std::ffi::CString;
use std::ptr;

use crate::definition_api::{DefinitionKind, DefinitionsIter, sat_definitions_iter_new_from_ids};
use crate::graph_api::{GraphPointer, with_graph};
use saturn::model::ids::UriId;

/// Returns the UTF-8 URI string for a document id.
/// Caller must free with `free_c_string`.
///
/// # Safety
///
/// Assumes pointer is valid.
///
/// # Panics
///
/// This function will panic if the URI pointer is invalid.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn sat_document_uri(pointer: GraphPointer, uri_id: i64) -> *const c_char {
    with_graph(pointer, |graph| {
        let uri_id = UriId::new(uri_id);
        if let Some(doc) = graph.documents().get(&uri_id) {
            CString::new(doc.uri()).unwrap().into_raw().cast_const()
        } else {
            ptr::null()
        }
    })
}

/// An iterator over definition IDs and kinds for a given document (URI)
///
/// We snapshot the IDs at iterator creation so if the graph is modified, the iterator will not see the changes
// Use shared DefinitionsIter directly in signatures
/// Creates a new iterator over definition IDs for a given document by snapshotting the current set of IDs.
///
/// # Safety
///
/// - `pointer` must be a valid `GraphPointer` previously returned by this crate.
/// - The returned pointer must be freed with `sat_document_definitions_iter_free`.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn sat_document_definitions_iter_new(pointer: GraphPointer, uri_id: i64) -> *mut DefinitionsIter {
    // Snapshot the IDs and kinds at iterator creation to avoid borrowing across FFI calls
    with_graph(pointer, |graph| {
        let uri_id = UriId::new(uri_id);
        if let Some(doc) = graph.documents().get(&uri_id) {
            sat_definitions_iter_new_from_ids(graph, doc.definitions())
        } else {
            DefinitionsIter::new(Vec::<(i64, DefinitionKind)>::new().into_boxed_slice())
        }
    })
}
