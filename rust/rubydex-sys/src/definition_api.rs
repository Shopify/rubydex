//! This file provides the C API for Definition accessors

use crate::graph_api::{GraphPointer, with_graph};
use crate::location_api::{Location, create_location_for_uri_and_offset};
use crate::reference_api::CConstantReference;
use libc::c_char;
use rubydex::model::definitions::{Definition, Mixin, Parameter};
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
    MethodVisibility = 6,
    Method = 7,
    AttrAccessor = 8,
    AttrReader = 9,
    AttrWriter = 10,
    GlobalVariable = 11,
    InstanceVariable = 12,
    ClassVariable = 13,
    MethodAlias = 14,
    GlobalVariableAlias = 15,
}

#[repr(C)]
#[derive(Debug, Clone, Copy)]
pub struct CDefinition {
    pub id: u64,
    pub kind: DefinitionKind,
}

pub(crate) fn map_definition_to_kind(defn: &Definition) -> DefinitionKind {
    match defn {
        Definition::Class(_) => DefinitionKind::Class,
        Definition::SingletonClass(_) => DefinitionKind::SingletonClass,
        Definition::Module(_) => DefinitionKind::Module,
        Definition::Constant(_) => DefinitionKind::Constant,
        Definition::ConstantAlias(_) => DefinitionKind::ConstantAlias,
        Definition::ConstantVisibility(_) => DefinitionKind::ConstantVisibility,
        Definition::MethodVisibility(_) => DefinitionKind::MethodVisibility,
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
    entries: Box<[CDefinition]>,
    index: usize,
}

iterator!(DefinitionsIter, entries: CDefinition);

/// # Safety
/// `iter` must be a valid pointer previously returned by `DefinitionsIter::new`.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn rdx_definitions_iter_len(iter: *const DefinitionsIter) -> usize {
    unsafe { DefinitionsIter::len(iter) }
}

/// # Safety
/// - `iter` must be a valid pointer previously returned by `DefinitionsIter::new`.
/// - `out` must be a valid, writable pointer.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn rdx_definitions_iter_next(iter: *mut DefinitionsIter, out: *mut CDefinition) -> bool {
    unsafe { DefinitionsIter::next(iter, out) }
}

/// # Safety
/// - `iter` must be a pointer previously returned by `DefinitionsIter::new`.
/// - `iter` must not be used after being freed.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn rdx_definitions_iter_free(iter: *mut DefinitionsIter) {
    unsafe { DefinitionsIter::free(iter) }
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
            CDefinition { id, kind }
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

/// Returns the superclass constant reference for a class definition, or NULL if the class has no superclass. Caller
/// must free with `free_c_constant_reference`.
///
/// # Safety
/// - `pointer` must be a valid pointer previously returned by `rdx_graph_new`.
/// - `definition_id` must be a valid definition id for a class definition.
///
/// # Panics
/// This function will panic if the definition cannot be found or is not a class definition.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn rdx_class_definition_superclass(
    pointer: GraphPointer,
    definition_id: u64,
) -> *const CConstantReference {
    with_graph(pointer, |graph| {
        let def_id = DefinitionId::new(definition_id);
        let defn = graph.definitions().get(&def_id).expect("Definition not found");

        let Definition::Class(class_def) = defn else {
            panic!("Definition is not a class: {definition_id}");
        };

        let Some(ref_id) = class_def.superclass_ref() else {
            return ptr::null();
        };

        Box::into_raw(Box::new(CConstantReference::from_id(graph, *ref_id))).cast_const()
    })
}

/// C-compatible enum representing the kind of a mixin.
#[repr(C)]
#[derive(Debug, Clone, Copy)]
pub enum MixinKind {
    Include = 0,
    Prepend = 1,
    Extend = 2,
}

/// C-compatible struct representing a mixin (kind + constant reference).
#[repr(C)]
#[derive(Debug, Clone, Copy)]
pub struct CMixin {
    pub kind: MixinKind,
    pub constant_reference: CConstantReference,
}

#[derive(Debug)]
pub struct MixinsIter {
    entries: Box<[CMixin]>,
    index: usize,
}

iterator!(MixinsIter, entries: CMixin);

/// # Safety
/// `iter` must be a valid pointer previously returned by `rdx_definition_mixins`.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn rdx_mixins_iter_len(iter: *const MixinsIter) -> usize {
    unsafe { MixinsIter::len(iter) }
}

/// # Safety
/// - `iter` must be a valid pointer previously returned by `rdx_definition_mixins`.
/// - `out` must be a valid, writable pointer.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn rdx_mixins_iter_next(iter: *mut MixinsIter, out: *mut CMixin) -> bool {
    unsafe { MixinsIter::next(iter, out) }
}

/// # Safety
/// - `iter` must be a pointer previously returned by `rdx_definition_mixins`.
/// - `iter` must not be used after being freed.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn rdx_mixins_iter_free(iter: *mut MixinsIter) {
    unsafe { MixinsIter::free(iter) }
}

fn map_mixin_kind(mixin: &Mixin) -> MixinKind {
    match mixin {
        Mixin::Include(_) => MixinKind::Include,
        Mixin::Prepend(_) => MixinKind::Prepend,
        Mixin::Extend(_) => MixinKind::Extend,
    }
}

/// Returns an iterator over the mixins for a definition (class, module, or singleton class).
/// Returns NULL for definition types that do not support mixins.
///
/// # Safety
/// - `pointer` must be a valid pointer previously returned by `rdx_graph_new`.
/// - `definition_id` must be a valid definition id.
///
/// # Panics
/// This function will panic if the definition cannot be found.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn rdx_definition_mixins(pointer: GraphPointer, definition_id: u64) -> *mut MixinsIter {
    with_graph(pointer, |graph| {
        let def_id = DefinitionId::new(definition_id);
        let defn = graph.definitions().get(&def_id).expect("Definition not found");

        let mixins = match defn {
            Definition::Class(class_def) => class_def.mixins(),
            Definition::Module(mod_def) => mod_def.mixins(),
            Definition::SingletonClass(singleton_def) => singleton_def.mixins(),
            _ => return ptr::null_mut(),
        };

        let entries: Vec<CMixin> = mixins
            .iter()
            .map(|mixin| CMixin {
                kind: map_mixin_kind(mixin),
                constant_reference: CConstantReference::from_id(graph, *mixin.constant_reference_id()),
            })
            .collect();

        MixinsIter::new(entries.into_boxed_slice())
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

/// Returns signatures for a `MethodAliasDefinition` by following the alias chain.
/// Returns NULL if the definition is not a method alias or the chain cannot be resolved.
///
/// # Safety
/// - `pointer` must be a valid pointer previously returned by `rdx_graph_new`.
/// - `definition_id` must be a valid definition id.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn rdx_method_alias_definition_signatures(
    pointer: GraphPointer,
    definition_id: u64,
) -> *mut SignatureArray {
    with_graph(pointer, |graph| {
        let def_id = DefinitionId::new(definition_id);
        let Some(Definition::MethodAlias(alias)) = graph.definitions().get(&def_id) else {
            return ptr::null_mut();
        };

        // Follow the alias chain until we find method definitions
        let mut current_results = rubydex::query::dealias_method(graph, alias);
        let mut visited = std::collections::HashSet::new();
        visited.insert(def_id);

        loop {
            let mut sig_entries: Vec<SignatureEntry> = Vec::new();
            let mut next_aliases = Vec::new();

            for result in &current_results {
                match result {
                    rubydex::query::DealiasMethodResult::Method(id) => {
                        if let Some(Definition::Method(method_def)) = graph.definitions().get(id) {
                            collect_method_signatures(graph, method_def, id.get(), &mut sig_entries);
                        }
                    }
                    rubydex::query::DealiasMethodResult::Alias(id) => {
                        if visited.insert(*id)
                            && let Some(Definition::MethodAlias(next_alias)) = graph.definitions().get(id)
                        {
                            next_aliases.extend(rubydex::query::dealias_method(graph, next_alias));
                        }
                    }
                }
            }

            if !sig_entries.is_empty() {
                let mut boxed = sig_entries.into_boxed_slice();
                let len = boxed.len();
                let items_ptr = boxed.as_mut_ptr();
                std::mem::forget(boxed);
                return Box::into_raw(Box::new(SignatureArray { items: items_ptr, len }));
            }

            if next_aliases.is_empty() {
                return ptr::null_mut();
            }

            current_results = next_aliases;
        }
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
