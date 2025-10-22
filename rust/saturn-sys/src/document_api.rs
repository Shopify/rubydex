//! This file provides the C API for Document accessors

use libc::c_char;
use std::ffi::CString;
use std::ptr;

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
