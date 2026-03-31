//! This file provides the C API for Definition accessors

use crate::graph_api::{GraphPointer, with_graph};
use crate::location_api::{Location, create_location_for_uri_and_offset};
use libc::c_char;
use rubydex::model::definitions::{Definition, Parameter};
use rubydex::model::ids::DefinitionId;
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
    ConstantAlias = 4,
    ConstantVisibility = 5,
    Method = 6,
    AttrAccessor = 7,
    AttrReader = 8,
    AttrWriter = 9,
    GlobalVariable = 10,
    InstanceVariable = 11,
    ClassVariable = 12,
    MethodAlias = 13,
    GlobalVariableAlias = 14,
}

pub(crate) fn map_definition_to_kind(defn: &Definition) -> DefinitionKind {
    match defn {
        Definition::Class(_) => DefinitionKind::Class,
        Definition::SingletonClass(_) => DefinitionKind::SingletonClass,
        Definition::Module(_) => DefinitionKind::Module,
        Definition::Constant(_) => DefinitionKind::Constant,
        Definition::ConstantAlias(_) => DefinitionKind::ConstantAlias,
        Definition::ConstantVisibility(_) => DefinitionKind::ConstantVisibility,
        Definition::Method(_) => DefinitionKind::Method,
        Definition::AttrAccessor(_) => DefinitionKind::AttrAccessor,
        Definition::AttrReader(_) => DefinitionKind::AttrReader,
        Definition::AttrWriter(_) => DefinitionKind::AttrWriter,
        Definition::GlobalVariable(_) => DefinitionKind::GlobalVariable,
        Definition::InstanceVariable(_) => DefinitionKind::InstanceVariable,
        Definition::ClassVariable(_) => DefinitionKind::ClassVariable,
        Definition::MethodAlias(_) => DefinitionKind::MethodAlias,
        Definition::GlobalVariableAlias(_) => DefinitionKind::GlobalVariableAlias,
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
pub unsafe extern "C" fn rdx_definition_kind(pointer: GraphPointer, definition_id: u64) -> DefinitionKind {
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
pub unsafe extern "C" fn rdx_definition_name(pointer: GraphPointer, definition_id: u64) -> *const c_char {
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
    pub entries: Box<[(u64, DefinitionKind)]>,
    pub index: usize,
}

impl DefinitionsIter {
    #[must_use]
    pub fn new(entries: Box<[(u64, DefinitionKind)]>) -> *mut DefinitionsIter {
        Box::into_raw(Box::new(DefinitionsIter { entries, index: 0 }))
    }
}

/// Returns the total number of entries in the iterator snapshot.
///
/// # Safety
/// - `iter` must be a valid pointer previously returned by `DefinitionsIter::new`.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn rdx_definitions_iter_len(iter: *const DefinitionsIter) -> usize {
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
pub unsafe extern "C" fn rdx_definitions_iter_next(
    iter: *mut DefinitionsIter,
    out_id: *mut u64,
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
pub unsafe extern "C" fn rdx_definitions_iter_free(iter: *mut DefinitionsIter) {
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
/// Caller must free the returned pointer with `rdx_definition_comments_free` and each inner string with `free_c_string` if needed.
///
/// # Safety
/// - `pointer` must be a valid pointer previously returned by `rdx_graph_new`.
/// - `definition_id` must be a valid definition id.
///
/// # Panics
/// This function will panic if a definition or document cannot be found.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn rdx_definition_comments(pointer: GraphPointer, definition_id: u64) -> *mut CommentArray {
    with_graph(pointer, |graph| {
        let def_id = DefinitionId::new(definition_id);
        let Some(defn) = graph.definitions().get(&def_id) else {
            panic!("Definition not found: {definition_id:?}");
        };

        let uri_id = *defn.uri_id();
        let document = graph.documents().get(&uri_id).expect("document should exist");

        let mut entries = defn
            .comments()
            .iter()
            .map(|c| CommentEntry {
                string: CString::new(c.string().as_str()).unwrap().into_raw().cast_const(),
                location: create_location_for_uri_and_offset(graph, document, c.offset()),
            })
            .collect::<Vec<CommentEntry>>()
            .into_boxed_slice();

        let len = entries.len();
        let items_ptr = entries.as_mut_ptr();
        std::mem::forget(entries);

        Box::into_raw(Box::new(CommentArray { items: items_ptr, len }))
    })
}

/// Frees a `CommentArray` previously returned by `rdx_definition_comments`.
///
/// # Safety
/// - `ptr` must be a valid pointer previously returned by `rdx_definition_comments`.
/// - `ptr` must not be used after being freed.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn rdx_definition_comments_free(ptr: *mut CommentArray) {
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
                unsafe { crate::location_api::rdx_location_free(item.location) };
                item.location = ptr::null_mut();
            }
        }

        // boxed_slice is dropped here, freeing the items buffer
    }
    // arr is dropped here
}

/// Returns a newly allocated `Location` for the given definition id.
/// Caller must free the returned pointer with `rdx_location_free`.
///
/// # Safety
/// - `pointer` must be a valid pointer previously returned by `rdx_graph_new`.
/// - `definition_id` must be a valid definition id.
///
/// # Panics
///
/// This function will panic if a definition or document cannot be found.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn rdx_definition_location(pointer: GraphPointer, definition_id: u64) -> *mut Location {
    with_graph(pointer, |graph| {
        let def_id = DefinitionId::new(definition_id);
        let Some(defn) = graph.definitions().get(&def_id) else {
            panic!("Definition not found: {definition_id:?}");
        };

        let document = graph.documents().get(defn.uri_id()).expect("document should exist");
        create_location_for_uri_and_offset(graph, document, defn.offset())
    })
}

/// Creates a new iterator over definition IDs for a given declaration by snapshotting the current set of IDs.
///
/// # Panics
///
/// This function will panic if a definition cannot be found.
pub(crate) fn rdx_definitions_iter_new_from_ids<'a, I>(
    graph: &rubydex::model::graph::Graph,
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
        .collect::<Vec<_>>()
        .into_boxed_slice();

    DefinitionsIter::new(entries)
}

/// Returns true if the definition is deprecated.
///
/// # Safety
/// - `pointer` must be a valid pointer previously returned by `rdx_graph_new`.
/// - `definition_id` must be a valid definition id.
///
/// # Panics
/// This function will panic if a definition cannot be found.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn rdx_definition_is_deprecated(pointer: GraphPointer, definition_id: u64) -> bool {
    with_graph(pointer, |graph| {
        let def_id = DefinitionId::new(definition_id);
        let defn = graph.definitions().get(&def_id).expect("definition not found");
        defn.is_deprecated()
    })
}

/// Returns a newly allocated `Location` for the name portion of a definition id.
/// For class, module, and singleton class definitions, this returns the location of just
/// the name (e.g., "Bar" in `class Foo::Bar`).
/// For other definition types, returns NULL.
/// Caller must free the returned pointer with `rdx_location_free`.
///
/// # Safety
/// - `pointer` must be a valid pointer previously returned by `rdx_graph_new`.
/// - `definition_id` must be a valid definition id.
///
/// # Panics
/// Panics if the definition's document does not exist in the graph.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn rdx_definition_name_location(pointer: GraphPointer, definition_id: u64) -> *mut Location {
    with_graph(pointer, |graph| {
        let def_id = DefinitionId::new(definition_id);
        let Some(defn) = graph.definitions().get(&def_id) else {
            return ptr::null_mut();
        };
        let Some(name_offset) = defn.name_offset() else {
            return ptr::null_mut();
        };
        let document = graph.documents().get(defn.uri_id()).expect("document should exist");
        create_location_for_uri_and_offset(graph, document, name_offset)
    })
}

/// C-compatible enum representing the kind of a parameter.
#[repr(C)]
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum ParameterKind {
    RequiredPositional = 0,
    OptionalPositional = 1,
    Rest = 2,
    RequiredKeyword = 3,
    OptionalKeyword = 4,
    RestKeyword = 5,
    Block = 6,
    Forward = 7,
}

fn map_parameter_kind(param: &Parameter) -> ParameterKind {
    match param {
        Parameter::RequiredPositional(_) | Parameter::Post(_) => ParameterKind::RequiredPositional,
        Parameter::OptionalPositional(_) => ParameterKind::OptionalPositional,
        Parameter::RestPositional(_) => ParameterKind::Rest,
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
/// Returns NULL if the definition is not a method definition.
/// Caller must free the returned pointer with `rdx_definition_signatures_free`.
///
/// # Safety
/// - `pointer` must be a valid pointer previously returned by `rdx_graph_new`.
/// - `definition_id` must be a valid definition id.
///
/// # Panics
/// This function will panic if a definition or document cannot be found.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn rdx_definition_signatures(pointer: GraphPointer, definition_id: u64) -> *mut SignatureArray {
    with_graph(pointer, |graph| {
        let def_id = DefinitionId::new(definition_id);
        let Some(Definition::Method(method_def)) = graph.definitions().get(&def_id) else {
            return ptr::null_mut();
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
    graph: &rubydex::model::graph::Graph,
    method_def: &rubydex::model::definitions::MethodDefinition,
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
