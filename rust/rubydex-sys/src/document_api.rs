//! This file provides the C API for Document accessors

use libc::c_char;
use std::ffi::CString;
use std::ptr;

use crate::definition_api::{DefinitionsIter, rdx_definitions_iter_new_from_ids};
use crate::graph_api::{GraphPointer, with_graph};
use crate::location_api::{Location, create_location_for_uri_and_offset};
use crate::reference_api::{CMethodReference, MethodReferencesIter};
use rubydex::model::ids::UriId;

#[derive(Debug)]
pub struct DocumentsIter {
    entries: Box<[u64]>,
    index: usize,
}

iterator!(DocumentsIter, entries: u64);

/// C-compatible comment entry using a byte length so comment strings may contain NUL bytes.
#[repr(C)]
#[derive(Debug, Clone, Copy)]
pub struct DocumentCommentEntry {
    pub string: *const c_char,
    pub string_len: usize,
    pub location: *mut Location,
}

impl DocumentCommentEntry {
    fn new(string: &str, location: *mut Location) -> Self {
        let mut bytes = string.as_bytes().to_vec().into_boxed_slice();
        let string_len = bytes.len();
        let string = bytes.as_mut_ptr().cast::<c_char>();
        std::mem::forget(bytes);

        Self {
            string,
            string_len,
            location,
        }
    }
}

/// C-compatible array of comments in one group.
#[repr(C)]
pub struct DocumentCommentArray {
    pub items: *mut DocumentCommentEntry,
    pub len: usize,
}

/// C-compatible array of comment groups not attached to definitions in a document.
#[repr(C)]
pub struct CommentGroupsArray {
    pub items: *mut DocumentCommentArray,
    pub len: usize,
}

/// Returns comment groups not attached to definitions in a Ruby document, or NULL if the document cannot be found.
/// Each group contains comments in source order. Caller must free the result with
/// `rdx_document_comments_free`.
///
/// # Safety
/// - `pointer` must be a valid `GraphPointer` previously returned by `rdx_graph_new`.
/// - `uri_id` must be a valid URI id.
///
#[unsafe(no_mangle)]
pub unsafe extern "C" fn rdx_document_comments(pointer: GraphPointer, uri_id: u64) -> *mut CommentGroupsArray {
    with_graph(pointer, |graph| {
        let uri_id = UriId::new(uri_id);
        let Some(document) = graph.documents().get(&uri_id) else {
            return ptr::null_mut();
        };

        let mut groups = document
            .comments()
            .iter()
            .map(|group| {
                let mut items = group
                    .comments()
                    .iter()
                    .map(|comment| {
                        DocumentCommentEntry::new(
                            comment.string(),
                            create_location_for_uri_and_offset(graph, document, comment.offset()),
                        )
                    })
                    .collect::<Vec<_>>()
                    .into_boxed_slice();

                let len = items.len();
                let items_ptr = items.as_mut_ptr();
                std::mem::forget(items);

                DocumentCommentArray { items: items_ptr, len }
            })
            .collect::<Vec<_>>()
            .into_boxed_slice();

        let len = groups.len();
        let items = groups.as_mut_ptr();
        std::mem::forget(groups);

        Box::into_raw(Box::new(CommentGroupsArray { items, len }))
    })
}

/// Frees a `CommentGroupsArray` previously returned by `rdx_document_comments`.
///
/// # Safety
/// - `ptr` must be a valid pointer returned by `rdx_document_comments`, or NULL.
/// - `ptr` must not be used after being freed.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn rdx_document_comments_free(ptr: *mut CommentGroupsArray) {
    if ptr.is_null() {
        return;
    }

    let groups = unsafe { Box::from_raw(ptr) };
    if !groups.items.is_null() && groups.len > 0 {
        let slice_ptr = ptr::slice_from_raw_parts_mut(groups.items, groups.len);
        let mut groups_slice: Box<[DocumentCommentArray]> = unsafe { Box::from_raw(slice_ptr) };
        for group in &mut groups_slice {
            if group.items.is_null() || group.len == 0 {
                continue;
            }

            let entries_ptr = ptr::slice_from_raw_parts_mut(group.items, group.len);
            let mut entries: Box<[DocumentCommentEntry]> = unsafe { Box::from_raw(entries_ptr) };
            for entry in &mut entries {
                if !entry.string.is_null() {
                    let string_ptr =
                        ptr::slice_from_raw_parts_mut(entry.string.cast_mut().cast::<u8>(), entry.string_len);
                    drop(unsafe { Box::from_raw(string_ptr) });
                    entry.string = ptr::null();
                }
                if !entry.location.is_null() {
                    unsafe { crate::location_api::rdx_location_free(entry.location) };
                    entry.location = ptr::null_mut();
                }
            }
        }
    }
}

/// # Safety
/// `iter` must be a valid pointer previously returned by `DocumentsIter::new`.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn rdx_graph_documents_iter_len(iter: *const DocumentsIter) -> usize {
    unsafe { DocumentsIter::len(iter) }
}

/// # Safety
/// - `iter` must be a valid pointer previously returned by `DocumentsIter::new`.
/// - `out` must be a valid, writable pointer.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn rdx_graph_documents_iter_next(iter: *mut DocumentsIter, out: *mut u64) -> bool {
    unsafe { DocumentsIter::next(iter, out) }
}

/// # Safety
/// - `iter` must be a pointer previously returned by `DocumentsIter::new`.
/// - `iter` must not be used after being freed.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn rdx_graph_documents_iter_free(iter: *mut DocumentsIter) {
    unsafe { DocumentsIter::free(iter) }
}

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
pub unsafe extern "C" fn rdx_document_uri(pointer: GraphPointer, uri_id: u64) -> *const c_char {
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
/// - The returned pointer must be freed with `rdx_document_definitions_iter_free`.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn rdx_document_definitions_iter_new(pointer: GraphPointer, uri_id: u64) -> *mut DefinitionsIter {
    // Snapshot the IDs and kinds at iterator creation to avoid borrowing across FFI calls
    with_graph(pointer, |graph| {
        let uri_id = UriId::new(uri_id);
        if let Some(doc) = graph.documents().get(&uri_id) {
            rdx_definitions_iter_new_from_ids(graph, doc.definitions())
        } else {
            DefinitionsIter::new(Vec::<_>::new().into_boxed_slice())
        }
    })
}

/// Creates a new iterator over method reference IDs for a given document by snapshotting the current set of IDs.
///
/// # Safety
///
/// - `pointer` must be a valid `GraphPointer` previously returned by this crate.
/// - The returned pointer must be freed with `rdx_method_references_iter_free`.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn rdx_document_method_references_iter_new(
    pointer: GraphPointer,
    uri_id: u64,
) -> *mut MethodReferencesIter {
    with_graph(pointer, |graph| {
        let uri_id = UriId::new(uri_id);
        if let Some(doc) = graph.documents().get(&uri_id) {
            let entries: Vec<_> = doc
                .method_references()
                .iter()
                .map(|ref_id| CMethodReference { id: **ref_id })
                .collect();

            MethodReferencesIter::new(entries.into_boxed_slice())
        } else {
            MethodReferencesIter::new(Vec::<_>::new().into_boxed_slice())
        }
    })
}
