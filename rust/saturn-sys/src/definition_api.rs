//! This file provides the C API for Definition accessors

use crate::graph_api::{GraphPointer, with_graph};
use crate::location_api::{Location, create_location_for_uri_and_offset};
use saturn::model::definitions::Definition;
use saturn::model::ids::DefinitionId;

/// C-compatible enum representing the kind of a definition.
#[repr(C)]
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum DefinitionKind {
    Class = 0,
    Module = 1,
    Constant = 2,
    Method = 3,
    AttrAccessor = 4,
    AttrReader = 5,
    AttrWriter = 6,
    GlobalVariable = 7,
    InstanceVariable = 8,
    ClassVariable = 9,
}

pub(crate) fn map_definition_to_kind(defn: &Definition) -> DefinitionKind {
    match defn {
        Definition::Class(_) => DefinitionKind::Class,
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

/// Returns a newly allocated `Location` for the given definition id.
/// Caller must free the returned pointer with `sat_definition_location_free`.
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

        create_location_for_uri_and_offset(&uri, defn.start(), defn.end())
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
