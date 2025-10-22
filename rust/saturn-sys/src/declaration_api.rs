//! This file provides the C API for the Graph object

use libc::c_char;
use std::ffi::CString;
use std::ptr;

use crate::graph_api::{GraphPointer, with_graph};
use saturn::model::ids::DeclarationId;

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
pub unsafe extern "C" fn sat_declaration_name(pointer: GraphPointer, name_id: i64) -> *const c_char {
    with_graph(pointer, |graph| {
        let name_id = DeclarationId::new(name_id);
        if let Some(decl) = graph.declarations().get(&name_id) {
            CString::new(decl.name()).unwrap().into_raw().cast_const()
        } else {
            ptr::null()
        }
    })
}
