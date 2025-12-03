//! This file provides the C API for Definition accessors

use crate::graph_api::{GraphPointer, with_graph};
use crate::location_api::{Location, create_location_for_uri_and_offset};
use libc::c_char;
use saturn::model::definitions::Definition;
use saturn::model::ids::DefinitionId;
use std::ffi::CString;
use std::ptr;

/// C-compatible enum representing the kind of a definition.
#[repr(C)]
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum DefinitionKind {
    Class = 0,
    SingletonClass = 1,
    Module = 2,
    Constant = 3,
    Method = 4,
    AttrAccessor = 5,
    AttrReader = 6,
    AttrWriter = 7,
    GlobalVariable = 8,
    InstanceVariable = 9,
    ClassVariable = 10,
}

pub(crate) fn map_definition_to_kind(defn: &Definition) -> DefinitionKind {
    match defn {
        Definition::Class(_) => DefinitionKind::Class,
        Definition::SingletonClass(_) => DefinitionKind::SingletonClass,
        Definition::Module(_) => DefinitionKind::Module,
        Definition::Constant(_) => DefinitionKind::Constant,
        Definition::Method(_) => DefinitionKind::Method,
        Definition::AttrAccessor(_) => DefinitionKind::AttrAccessor,
        Definition::AttrReader(_) => DefinitionKind::AttrReader,
        Definition::AttrWriter(_) => DefinitionKind::AttrWriter,
        Definition::GlobalVariable(_) => DefinitionKind::GlobalVariable,
        Definition::InstanceVariable(_) => DefinitionKind::InstanceVariable,
        Definition::ClassVariable(_) => DefinitionKind::ClassVariable,
    }
}

/// Returns the enum kind for a definition id (e.g. Class, Module).
///
/// # Safety
///
/// Assumes pointer is valid.
///
/// # Panics
///
/// This function will panic if the definition cannot be found.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn sat_definition_kind(pointer: GraphPointer, definition_id: i64) -> DefinitionKind {
    with_graph(pointer, |graph| {
        let definition_id = DefinitionId::new(definition_id);
        if let Some(defn) = graph.definitions().get(&definition_id) {
            map_definition_to_kind(defn)
        } else {
            panic!("Definition not found: {definition_id:?}");
        }
    })
}

/// Returns the UTF-8 unqualified name string for a definition id.
/// Caller must free with `free_c_string`.
///
/// # Safety
///
/// Assumes pointer is valid.
///
/// # Panics
///
/// This function will panic if the definition cannot be found.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn sat_definition_name(pointer: GraphPointer, definition_id: i64) -> *const c_char {
    with_graph(pointer, |graph| {
        let def_id = DefinitionId::new(definition_id);
        if let Some(defn) = graph.definitions().get(&def_id) {
            let string_id = graph.definition_string_id(defn);

            if let Some(name) = graph.strings().get(&string_id) {
                CString::new(name.as_str()).unwrap().into_raw().cast_const()
            } else {
                ptr::null()
            }
        } else {
            ptr::null()
        }
    })
}

/// Shared iterator over definition (id, kind) pairs
#[derive(Debug)]
pub struct DefinitionsIter {
    pub entries: Box<[(i64, DefinitionKind)]>,
    pub index: usize,
}

impl DefinitionsIter {
    #[must_use]
    pub fn new(entries: Box<[(i64, DefinitionKind)]>) -> *mut DefinitionsIter {
        Box::into_raw(Box::new(DefinitionsIter { entries, index: 0 }))
    }
}

/// Returns the total number of entries in the iterator snapshot.
///
/// # Safety
/// - `iter` must be a valid pointer previously returned by `DefinitionsIter::new`.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn sat_definitions_iter_len(iter: *const DefinitionsIter) -> usize {
    if iter.is_null() {
        return 0;
    }
    unsafe { (&*iter).entries.len() }
}

/// Advances the iterator and writes the next (ID, Kind) into `out_id` and `out_kind`.
/// Returns `true` if a pair was written, or `false` if the iterator is exhausted or inputs are invalid.
///
/// # Safety
/// - `iter` must be a valid pointer previously returned by `DefinitionsIter::new`.
/// - `out_id` and `out_kind` must be valid, writable pointers.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn sat_definitions_iter_next(
    iter: *mut DefinitionsIter,
    out_id: *mut i64,
    out_kind: *mut DefinitionKind,
) -> bool {
    if iter.is_null() || out_id.is_null() || out_kind.is_null() {
        return false;
    }

    let it = unsafe { &mut *iter };
    if it.index >= it.entries.len() {
        return false;
    }

    let (id, kind) = it.entries[it.index];
    it.index += 1;
    unsafe {
        *out_id = id;
        *out_kind = kind;
    }
    true
}

/// Frees an iterator created by `DefinitionsIter::new`.
///
/// # Safety
/// - `iter` must be a pointer previously returned by `DefinitionsIter::new`.
/// - `iter` must not be used after being freed.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn sat_definitions_iter_free(iter: *mut DefinitionsIter) {
    if iter.is_null() {
        return;
    }
    unsafe {
        let _ = Box::from_raw(iter);
    }
}

/// C-compatible struct representing a single comment with its string and location
#[repr(C)]
pub struct CommentEntry {
    pub string: *const c_char,
    pub location: *mut Location,
}

/// C-compatible array of comments
#[repr(C)]
pub struct CommentArray {
    pub items: *mut CommentEntry,
    pub len: usize,
}

/// Returns a newly allocated array of comments (string and location) for the given definition id.
/// Caller must free the returned pointer with `sat_definition_comments_free` and each inner string with `free_c_string` if needed.
///
/// # Safety
/// - `pointer` must be a valid pointer previously returned by `sat_graph_new`.
/// - `definition_id` must be a valid definition id.
///
/// # Panics
/// This function will panic if a definition or document cannot be found.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn sat_definition_comments(pointer: GraphPointer, definition_id: i64) -> *mut CommentArray {
    with_graph(pointer, |graph| {
        let def_id = DefinitionId::new(definition_id);
        let Some(defn) = graph.definitions().get(&def_id) else {
            panic!("Definition not found: {definition_id:?}");
        };

        let uri_id = *defn.uri_id();
        let uri = if let Some(doc) = graph.documents().get(&uri_id) {
            doc.uri().to_string()
        } else {
            panic!("Document not found: {uri_id:?}");
        };

        let mut entries = defn
            .comments()
            .iter()
            .map(|c| CommentEntry {
                string: CString::new(c.string().as_str()).unwrap().into_raw().cast_const(),
                location: create_location_for_uri_and_offset(&uri, c.offset().start(), c.offset().end()),
            })
            .collect::<Vec<CommentEntry>>()
            .into_boxed_slice();

        let len = entries.len();
        let items_ptr = entries.as_mut_ptr();
        std::mem::forget(entries);

        Box::into_raw(Box::new(CommentArray { items: items_ptr, len }))
    })
}

/// Frees a `CommentArray` previously returned by `sat_definition_comments`.
///
/// # Safety
/// - `ptr` must be a valid pointer previously returned by `sat_definition_comments`.
/// - `ptr` must not be used after being freed.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn sat_definition_comments_free(ptr: *mut CommentArray) {
    if ptr.is_null() {
        return;
    }

    // Take ownership of the CommentArray
    let arr = unsafe { Box::from_raw(ptr) };

    if !arr.items.is_null() && arr.len > 0 {
        // Reconstruct the boxed slice so we can drop it after freeing inner allocations
        let slice_ptr = ptr::slice_from_raw_parts_mut(arr.items, arr.len);
        let mut boxed_slice: Box<[CommentEntry]> = unsafe { Box::from_raw(slice_ptr) };

        for item in &mut boxed_slice {
            if !item.string.is_null() {
                // Free the CString allocated for the comment string
                let _ = unsafe { CString::from_raw(item.string.cast_mut()) };
            }
            if !item.location.is_null() {
                unsafe { crate::location_api::sat_location_free(item.location) };
                item.location = ptr::null_mut();
            }
        }

        // boxed_slice is dropped here, freeing the items buffer
    }
    // arr is dropped here
}

/// Returns a newly allocated `Location` for the given definition id.
/// Caller must free the returned pointer with `sat_location_free`.
///
/// # Safety
/// - `pointer` must be a valid pointer previously returned by `sat_graph_new`.
/// - `definition_id` must be a valid definition id.
///
/// # Panics
///
/// This function will panic if a definition or document cannot be found.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn sat_definition_location(pointer: GraphPointer, definition_id: i64) -> *mut Location {
    with_graph(pointer, |graph| {
        let def_id = DefinitionId::new(definition_id);
        let Some(defn) = graph.definitions().get(&def_id) else {
            panic!("Definition not found: {definition_id:?}");
        };

        let uri_id = *defn.uri_id();
        let uri = if let Some(doc) = graph.documents().get(&uri_id) {
            doc.uri().to_string()
        } else {
            panic!("Document not found: {uri_id:?}");
        };

        let offset = defn.offset();
        create_location_for_uri_and_offset(&uri, offset.start(), offset.end())
    })
}

/// Creates a new iterator over definition IDs for a given declaration by snapshotting the current set of IDs.
///
/// # Panics
///
/// This function will panic if a definition cannot be found.
pub(crate) fn sat_definitions_iter_new_from_ids<'a, I>(
    graph: &saturn::model::graph::Graph,
    ids: I,
) -> *mut DefinitionsIter
where
    I: IntoIterator<Item = &'a DefinitionId>,
{
    let entries = ids
        .into_iter()
        .map(|def_id| {
            let id = **def_id;
            let kind = graph
                .definitions()
                .get(&DefinitionId::new(id))
                .map_or_else(|| panic!("Definition not found: {id:?}"), map_definition_to_kind);
            (id, kind)
        })
        .collect::<Vec<(i64, DefinitionKind)>>()
        .into_boxed_slice();

    DefinitionsIter::new(entries)
}
