//! This file provides the C API for the Graph object

use crate::reference_api::{ReferenceKind, ReferencesIter};
use crate::utils;
use libc::{c_char, c_void};
use rubydex::model::encoding::Encoding;
use rubydex::model::graph::Graph;
use rubydex::model::ids::DeclarationId;
use rubydex::resolution::Resolver;
use rubydex::{indexing, listing, query};
use std::ffi::CString;
use std::{mem, ptr};

pub type GraphPointer = *mut c_void;

/// Creates a new graph within a mutex. This is meant to be used when creating new Graph objects in Ruby
#[unsafe(no_mangle)]
pub extern "C" fn rdx_graph_new() -> GraphPointer {
    Box::into_raw(Box::new(Graph::new())) as GraphPointer
}

/// Frees a Graph through its pointer
#[unsafe(no_mangle)]
pub extern "C" fn rdx_graph_free(pointer: GraphPointer) {
    unsafe {
        let _ = Box::from_raw(pointer.cast::<Graph>());
    }
}

pub fn with_graph<F, T>(pointer: GraphPointer, action: F) -> T
where
    F: FnOnce(&mut Graph) -> T,
{
    let mut graph = unsafe { Box::from_raw(pointer.cast::<Graph>()) };
    let result = action(&mut graph);
    mem::forget(graph);
    result
}

/// Searches the graph based on query and returns all declarations that match
///
/// # Safety
///
/// Expects both the graph and the query pointers to be valid
#[unsafe(no_mangle)]
pub unsafe extern "C" fn rdx_graph_declarations_search(
    pointer: GraphPointer,
    c_query: *const c_char,
) -> *mut DeclarationsIter {
    let Ok(query) = (unsafe { utils::convert_char_ptr_to_string(c_query) }) else {
        return ptr::null_mut();
    };

    let ids = with_graph(pointer, |graph| {
        query::declaration_search(graph, &query)
            .into_iter()
            .map(|id| *id)
            .collect::<Vec<i64>>()
            .into_boxed_slice()
    });

    Box::into_raw(Box::new(DeclarationsIter { ids, index: 0 }))
}

/// Indexes all given file paths in parallel using the provided Graph pointer
///
/// # Panics
///
/// Will panic if the given array of C string file paths cannot be converted to a Vec<String>
///
/// # Safety
///
/// This function is unsafe because it dereferences raw pointers coming from C. The caller has to ensure that the Ruby
/// VM will not free the pointers related to the string array while they are in use by Rust
#[unsafe(no_mangle)]
pub unsafe extern "C" fn rdx_index_all(
    pointer: GraphPointer,
    file_paths: *const *const c_char,
    count: usize,
) -> *const c_char {
    let file_paths: Vec<String> = unsafe { utils::convert_double_pointer_to_vec(file_paths, count).unwrap() };
    let (file_paths, errors) = listing::collect_file_paths(file_paths);

    if !errors.is_empty() {
        let error_messages = errors
            .iter()
            .map(std::string::ToString::to_string)
            .collect::<Vec<_>>()
            .join("\n");

        return CString::new(error_messages).unwrap().into_raw().cast_const();
    }

    with_graph(pointer, |graph| {
        let errors = indexing::index_files(graph, file_paths);

        if !errors.is_empty() {
            let error_messages = errors
                .iter()
                .map(std::string::ToString::to_string)
                .collect::<Vec<_>>()
                .join("\n");

            return CString::new(error_messages).unwrap().into_raw().cast_const();
        }

        ptr::null()
    })
}

/// Runs the resolver to compute declarations, ownership and related structures
#[unsafe(no_mangle)]
pub extern "C" fn rdx_graph_resolve(pointer: GraphPointer) {
    with_graph(pointer, |graph| {
        let mut resolver = Resolver::new(graph);
        resolver.resolve_all();
    });
}

/// # Safety
///
/// Expects both the graph pointer and encoding string pointer to be valid
#[unsafe(no_mangle)]
pub unsafe extern "C" fn rdx_graph_set_encoding(pointer: GraphPointer, encoding_str: *const c_char) -> bool {
    let Ok(encoding) = (unsafe { utils::convert_char_ptr_to_string(encoding_str) }) else {
        return false;
    };

    let encoding_variant = match encoding.as_str() {
        "utf8" => Encoding::Utf8,
        "utf16" => Encoding::Utf16,
        "utf32" => Encoding::Utf32,
        _ => {
            return false;
        }
    };

    with_graph(pointer, |graph| {
        graph.set_encoding(encoding_variant);
    });

    true
}

/// An iterator over declaration IDs
///
/// We snapshot the IDs at iterator creation so if the graph is modified, the iterator will not see the changes
#[derive(Debug)]
pub struct DeclarationsIter {
    /// The snapshot of declaration IDs
    ids: Box<[i64]>,
    /// The current index of the iterator
    index: usize,
}

/// Creates a new iterator over declaration IDs by snapshotting the current set of IDs.
///
/// # Safety
///
/// - `pointer` must be a valid `GraphPointer` previously returned by this crate.
/// - The returned pointer must be freed with `rdx_graph_declarations_iter_free`.
///
/// # Panics
///
/// Will panic if acquiring a read lock on the graph's declarations fails
#[unsafe(no_mangle)]
pub unsafe extern "C" fn rdx_graph_declarations_iter_new(pointer: GraphPointer) -> *mut DeclarationsIter {
    // Snapshot the IDs at iterator creation to avoid borrowing across FFI calls
    let ids = with_graph(pointer, |graph| {
        graph
            .declarations()
            .keys()
            .map(|name_id| **name_id)
            .collect::<Vec<i64>>()
            .into_boxed_slice()
    });

    Box::into_raw(Box::new(DeclarationsIter { ids, index: 0 }))
}

/// Returns the total number of IDs in the iterator snapshot.
///
/// # Safety
///
/// - `iter` must be a valid pointer previously returned by `rdx_graph_declarations_iter_new`.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn rdx_graph_declarations_iter_len(iter: *const DeclarationsIter) -> usize {
    if iter.is_null() {
        return 0;
    }

    unsafe { (&*iter).ids.len() }
}

/// Advances the iterator and writes the next ID into `out_id`.
/// Returns `true` if an ID was written, or `false` if the iterator is exhausted or inputs are invalid.
///
/// # Safety
///
/// - `iter` must be a valid pointer previously returned by `rdx_graph_declarations_iter_new`.
/// - `out_id` must be a valid, writable pointer.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn rdx_graph_declarations_iter_next(iter: *mut DeclarationsIter, out_id: *mut i64) -> bool {
    if iter.is_null() || out_id.is_null() {
        return false;
    }

    let it = unsafe { &mut *iter };
    if it.index >= it.ids.len() {
        return false;
    }

    let id = it.ids[it.index];
    it.index += 1;
    unsafe { *out_id = id };

    true
}

/// Frees an iterator created by `rdx_graph_declarations_iter_new`.
///
/// # Safety
///
/// - `iter` must be a pointer previously returned by `rdx_graph_declarations_iter_new`.
/// - `iter` must not be used after being freed.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn rdx_graph_declarations_iter_free(iter: *mut DeclarationsIter) {
    if iter.is_null() {
        return;
    }

    unsafe {
        let _ = Box::from_raw(iter);
    }
}

/// An iterator over document (URI) IDs
///
/// We snapshot the IDs at iterator creation so if the graph is modified, the iterator will not see the changes
#[derive(Debug)]
pub struct DocumentsIter {
    /// The snapshot of document (URI) IDs
    ids: Box<[i64]>,
    /// The current index of the iterator
    index: usize,
}

/// Creates a new iterator over document (URI) IDs by snapshotting the current set of IDs.
///
/// # Safety
///
/// - `pointer` must be a valid `GraphPointer` previously returned by this crate.
/// - The returned pointer must be freed with `rdx_graph_documents_iter_free`.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn rdx_graph_documents_iter_new(pointer: GraphPointer) -> *mut DocumentsIter {
    // Snapshot the IDs at iterator creation to avoid borrowing across FFI calls
    let ids = with_graph(pointer, |graph| {
        graph
            .documents()
            .keys()
            .map(|uri_id| **uri_id)
            .collect::<Vec<i64>>()
            .into_boxed_slice()
    });

    Box::into_raw(Box::new(DocumentsIter { ids, index: 0 }))
}

/// Returns the total number of IDs in the iterator snapshot.
///
/// # Safety
///
/// - `iter` must be a valid pointer previously returned by `rdx_graph_documents_iter_new`.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn rdx_graph_documents_iter_len(iter: *const DocumentsIter) -> usize {
    if iter.is_null() {
        return 0;
    }

    unsafe { (&*iter).ids.len() }
}

/// Advances the iterator and writes the next ID into `out_id`.
/// Returns `true` if an ID was written, or `false` if the iterator is exhausted or inputs are invalid.
///
/// # Safety
///
/// - `iter` must be a valid pointer previously returned by `rdx_graph_documents_iter_new`.
/// - `out_id` must be a valid, writable pointer.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn rdx_graph_documents_iter_next(iter: *mut DocumentsIter, out_id: *mut i64) -> bool {
    if iter.is_null() || out_id.is_null() {
        return false;
    }

    let it = unsafe { &mut *iter };
    if it.index >= it.ids.len() {
        return false;
    }

    let id = it.ids[it.index];
    it.index += 1;
    unsafe { *out_id = id };

    true
}

/// Frees an iterator created by `rdx_graph_documents_iter_new`.
///
/// # Safety
///
/// - `iter` must be a pointer previously returned by `rdx_graph_documents_iter_new`.
/// - `iter` must not be used after being freed.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn rdx_graph_documents_iter_free(iter: *mut DocumentsIter) {
    if iter.is_null() {
        return;
    }

    unsafe {
        let _ = Box::from_raw(iter);
    }
}

/// Attempts to resolve a declaration from a fully-qualified name string.
/// Returns a pointer to the internal ID if it exists, or NULL if it does not.
///
/// # Safety
/// - `pointer` must be a valid `GraphPointer`
/// - `name` must be a valid, null-terminated UTF-8 string
/// - `out_id` must be a valid, writable pointer
///
/// # Panics
///
/// Will panic if acquiring a read lock on the graph's declarations fails
#[unsafe(no_mangle)]
pub unsafe extern "C" fn rdx_graph_get_declaration(pointer: GraphPointer, name: *const c_char) -> *const i64 {
    let Ok(name_str) = (unsafe { utils::convert_char_ptr_to_string(name) }) else {
        return ptr::null();
    };

    with_graph(pointer, |graph| {
        // TODO: We should perform name resolution instead of accessing the graph with the canonical ID
        let decl_id = DeclarationId::from(name_str.as_str());
        if graph.declarations().contains_key(&decl_id) {
            Box::into_raw(Box::new(*decl_id)).cast_const()
        } else {
            ptr::null()
        }
    })
}

/// Creates a new iterator over constant references by snapshotting the current set of (id, kind) pairs.
///
/// # Safety
/// - `pointer` must be a valid `GraphPointer` previously returned by this crate.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn rdx_graph_constant_references_iter_new(pointer: GraphPointer) -> *mut ReferencesIter {
    with_graph(pointer, |graph| {
        let refs: Vec<(i64, ReferenceKind)> = graph
            .constant_references()
            .keys()
            .map(|id| (**id, ReferenceKind::Constant))
            .collect();

        ReferencesIter::new(refs.into_boxed_slice())
    })
}

/// Creates a new iterator over method references by snapshotting the current set of (id, kind) pairs.
///
/// # Safety
/// - `pointer` must be a valid `GraphPointer` previously returned by this crate.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn rdx_graph_method_references_iter_new(pointer: GraphPointer) -> *mut ReferencesIter {
    with_graph(pointer, |graph| {
        let refs: Vec<(i64, ReferenceKind)> = graph
            .method_references()
            .keys()
            .map(|id| (**id, ReferenceKind::Method))
            .collect();

        ReferencesIter::new(refs.into_boxed_slice())
    })
}

/// Adds a synthetic method definition to the graph.
///
/// # Safety
///
/// - `pointer` must be a valid `GraphPointer`
/// - `owner_name`, `method_name`, `file_path` must be valid UTF-8 C strings
#[unsafe(no_mangle)]
pub unsafe extern "C" fn rdx_graph_add_method(
    pointer: GraphPointer,
    owner_name: *const c_char,
    method_name: *const c_char,
    file_path: *const c_char,
    line: u32,
    column: u32,
) -> bool {
    let Ok(owner) = (unsafe { utils::convert_char_ptr_to_string(owner_name) }) else {
        return false;
    };
    let Ok(name) = (unsafe { utils::convert_char_ptr_to_string(method_name) }) else {
        return false;
    };
    let Ok(path) = (unsafe { utils::convert_char_ptr_to_string(file_path) }) else {
        return false;
    };

    with_graph(pointer, |graph| {
        graph.add_synthetic_method(&owner, &name, &path, line, column)
    })
}

/// Adds a synthetic class definition to the graph.
///
/// # Safety
///
/// - `pointer` must be a valid `GraphPointer`
/// - `class_name`, `file_path` must be valid UTF-8 C strings
/// - `parent_name` can be null
#[unsafe(no_mangle)]
pub unsafe extern "C" fn rdx_graph_add_class(
    pointer: GraphPointer,
    class_name: *const c_char,
    parent_name: *const c_char, // can be null
    file_path: *const c_char,
    line: u32,
    column: u32,
) -> bool {
    let Ok(name) = (unsafe { utils::convert_char_ptr_to_string(class_name) }) else {
        return false;
    };
    let parent = if parent_name.is_null() {
        None
    } else {
        unsafe { utils::convert_char_ptr_to_string(parent_name).ok() }
    };
    let Ok(path) = (unsafe { utils::convert_char_ptr_to_string(file_path) }) else {
        return false;
    };

    with_graph(pointer, |graph| {
        graph.add_synthetic_class(&name, parent.as_deref(), &path, line, column)
    })
}

/// Adds a synthetic module definition to the graph.
///
/// # Safety
///
/// - `pointer` must be a valid `GraphPointer`
/// - `module_name`, `file_path` must be valid UTF-8 C strings
#[unsafe(no_mangle)]
pub unsafe extern "C" fn rdx_graph_add_module(
    pointer: GraphPointer,
    module_name: *const c_char,
    file_path: *const c_char,
    line: u32,
    column: u32,
) -> bool {
    let Ok(name) = (unsafe { utils::convert_char_ptr_to_string(module_name) }) else {
        return false;
    };
    let Ok(path) = (unsafe { utils::convert_char_ptr_to_string(file_path) }) else {
        return false;
    };

    with_graph(pointer, |graph| {
        graph.add_synthetic_module(&name, &path, line, column)
    })
}

/// Mixin type for FFI: Include, Prepend, or Extend
#[repr(C)]
pub enum MixinType {
    Include = 0,
    Prepend = 1,
    Extend = 2,
}

/// Adds a synthetic mixin relationship to the graph.
///
/// # Safety
///
/// - `pointer` must be a valid `GraphPointer`
/// - `target_name`, `module_name` must be valid UTF-8 C strings
#[unsafe(no_mangle)]
pub unsafe extern "C" fn rdx_graph_add_mixin(
    pointer: GraphPointer,
    target_name: *const c_char,
    module_name: *const c_char,
    mixin_type: MixinType,
) -> bool {
    let Ok(target) = (unsafe { utils::convert_char_ptr_to_string(target_name) }) else {
        return false;
    };
    let Ok(module) = (unsafe { utils::convert_char_ptr_to_string(module_name) }) else {
        return false;
    };

    with_graph(pointer, |graph| {
        graph.add_synthetic_mixin(&target, &module, mixin_type as u8)
    })
}

/// Registers an included hook: when trigger_module is included, extend with extend_module.
///
/// # Safety
///
/// - `pointer` must be a valid `GraphPointer`
/// - `trigger_module`, `extend_module` must be valid UTF-8 C strings
#[unsafe(no_mangle)]
pub unsafe extern "C" fn rdx_graph_register_included_hook(
    pointer: GraphPointer,
    trigger_module: *const c_char,
    extend_module: *const c_char,
) -> bool {
    let Ok(trigger) = (unsafe { utils::convert_char_ptr_to_string(trigger_module) }) else {
        return false;
    };
    let Ok(extend) = (unsafe { utils::convert_char_ptr_to_string(extend_module) }) else {
        return false;
    };

    with_graph(pointer, |graph| {
        graph.register_included_hook(&trigger, &extend);
        true
    })
}

// ============================================================================
// DSL Event API
// ============================================================================

/// Configure which DSL methods to capture during indexing.
/// Must be called before rdx_index_all().
///
/// # Safety
///
/// - `pointer` must be a valid GraphPointer
/// - `methods` must be an array of `count` valid C strings
#[unsafe(no_mangle)]
pub unsafe extern "C" fn rdx_set_dsl_capture_filter(
    pointer: GraphPointer,
    methods: *const *const c_char,
    count: usize,
) {
    let method_names: Vec<String> = (0..count)
        .filter_map(|i| {
            let ptr = unsafe { *methods.add(i) };
            if ptr.is_null() {
                return None;
            }
            unsafe { utils::convert_char_ptr_to_string(ptr) }.ok()
        })
        .collect();

    with_graph(pointer, |graph| {
        graph.set_dsl_capture_filter(method_names);
    });
}

/// Clear the DSL capture filter.
#[unsafe(no_mangle)]
pub extern "C" fn rdx_clear_dsl_capture_filter(pointer: GraphPointer) {
    with_graph(pointer, |graph| {
        graph.clear_dsl_capture_filter();
    });
}

/// Iterator over files with DSL events
pub struct DslFilesIter {
    uri_ids: Box<[i64]>,
    index: usize,
}

/// Creates a new iterator over files that have DSL events.
///
/// # Safety
///
/// - `pointer` must be a valid GraphPointer
#[unsafe(no_mangle)]
pub extern "C" fn rdx_dsl_files_iter_new(pointer: GraphPointer) -> *mut DslFilesIter {
    let uri_ids = with_graph(pointer, |graph| {
        graph
            .dsl_events()
            .keys()
            .map(|id| **id)
            .collect::<Vec<i64>>()
            .into_boxed_slice()
    });

    Box::into_raw(Box::new(DslFilesIter { uri_ids, index: 0 }))
}

/// Advances the DSL files iterator.
/// Returns true if a URI ID was written, false if exhausted.
///
/// # Safety
///
/// - `iter` must be a valid pointer from rdx_dsl_files_iter_new
/// - `out_uri_id` must be a valid writable pointer
#[unsafe(no_mangle)]
pub unsafe extern "C" fn rdx_dsl_files_iter_next(iter: *mut DslFilesIter, out_uri_id: *mut i64) -> bool {
    if iter.is_null() || out_uri_id.is_null() {
        return false;
    }
    let it = unsafe { &mut *iter };
    if it.index >= it.uri_ids.len() {
        return false;
    }
    unsafe { *out_uri_id = it.uri_ids[it.index] };
    it.index += 1;
    true
}

/// Frees a DSL files iterator.
///
/// # Safety
///
/// - `iter` must be a valid pointer from rdx_dsl_files_iter_new
#[unsafe(no_mangle)]
pub unsafe extern "C" fn rdx_dsl_files_iter_free(iter: *mut DslFilesIter) {
    if !iter.is_null() {
        unsafe {
            let _ = Box::from_raw(iter);
        }
    }
}

/// Get the file path for a URI ID.
/// Caller must free the returned string with free_c_string.
///
/// # Safety
///
/// - `pointer` must be a valid GraphPointer
#[unsafe(no_mangle)]
pub extern "C" fn rdx_graph_uri_path(pointer: GraphPointer, uri_id: i64) -> *const c_char {
    with_graph(pointer, |graph| {
        let uri_id = rubydex::model::ids::UriId::new(uri_id);
        if let Some(document) = graph.documents().get(&uri_id) {
            if let Ok(cstr) = CString::new(document.uri()) {
                return cstr.into_raw().cast_const();
            }
        }
        ptr::null()
    })
}

/// Struct to return DSL event data to C
#[repr(C)]
pub struct DslEventData {
    /// Unique ID within this file
    pub id: usize,
    /// Parent event ID (-1 for None/top-level)
    pub parent_id: i64,
    /// Method name (caller must free with free_c_string)
    pub method_name: *const c_char,
    /// Array of argument strings (caller must free each and the array)
    pub arguments: *const *const c_char,
    pub arguments_len: usize,
    /// Array of nesting stack declaration names (fully qualified)
    pub nesting_stack: *const *const c_char,
    pub nesting_len: usize,
    /// Byte offset in file
    pub offset: u32,
    /// Whether this call has a block
    pub has_block: bool,
    /// The receiver text if present (null if no receiver)
    pub receiver: *const c_char,
}

/// Iterator over DSL events for a specific file
pub struct DslEventsIter {
    events: Vec<DslEventData>,
    /// Owned strings to keep alive
    _owned_strings: Vec<CString>,
    /// Owned argument arrays
    _owned_args: Vec<Vec<*const c_char>>,
    /// Owned nesting stack pointer arrays
    _owned_nesting: Vec<Vec<*const c_char>>,
    index: usize,
}

/// Creates a new iterator over DSL events for a specific file.
///
/// # Safety
///
/// - `pointer` must be a valid GraphPointer
#[unsafe(no_mangle)]
pub extern "C" fn rdx_dsl_events_iter_new(pointer: GraphPointer, uri_id: i64) -> *mut DslEventsIter {
    with_graph(pointer, |graph| {
        let uri_id = rubydex::model::ids::UriId::new(uri_id);
        let mut owned_strings = Vec::new();
        let mut owned_args = Vec::new();
        let mut owned_nesting = Vec::new();

        let events = graph
            .dsl_events()
            .get(&uri_id)
            .map(|file_events| {
                file_events
                    .events
                    .iter()
                    .map(|event| {
                        // Convert method name
                        let method_name = graph
                            .strings()
                            .get(&event.method_name)
                            .map(|s| s.as_str())
                            .unwrap_or("");
                        let method_cstr = CString::new(method_name).unwrap_or_default();
                        let method_ptr = method_cstr.as_ptr();
                        owned_strings.push(method_cstr);

                        // Convert arguments
                        let arg_ptrs: Vec<*const c_char> = event
                            .arguments
                            .iter()
                            .filter_map(|arg_id| {
                                let arg_str = graph.strings().get(arg_id)?;
                                let arg_cstr = CString::new(arg_str.as_str()).ok()?;
                                let ptr = arg_cstr.as_ptr();
                                owned_strings.push(arg_cstr);
                                Some(ptr)
                            })
                            .collect();
                        let args_ptr = if arg_ptrs.is_empty() {
                            ptr::null()
                        } else {
                            arg_ptrs.as_ptr()
                        };
                        let args_len = arg_ptrs.len();
                        owned_args.push(arg_ptrs);

                        // Convert nesting stack: look up each definition to get the declaration name
                        // Note: nesting_stack stores DefinitionId values (incorrectly typed as DeclarationId)
                        let nesting_ptrs: Vec<*const c_char> = event
                            .nesting_stack
                            .iter()
                            .filter_map(|stored_id| {
                                // The stored value is actually a DefinitionId, not a DeclarationId
                                let def_id = rubydex::model::ids::DefinitionId::new(**stored_id);
                                // Look up the correct DeclarationId via definitions_to_declarations
                                let decl_id = graph.definitions_to_declarations().get(&def_id)?;
                                let decl = graph.declarations().get(decl_id)?;
                                let name_cstr = CString::new(decl.name()).ok()?;
                                let ptr = name_cstr.as_ptr();
                                owned_strings.push(name_cstr);
                                Some(ptr)
                            })
                            .collect();
                        let nesting_ptr = if nesting_ptrs.is_empty() {
                            ptr::null()
                        } else {
                            nesting_ptrs.as_ptr()
                        };
                        let nesting_len = nesting_ptrs.len();
                        owned_nesting.push(nesting_ptrs);

                        // Convert receiver
                        let receiver_ptr = event
                            .receiver
                            .and_then(|recv_id| {
                                let recv_str = graph.strings().get(&recv_id)?;
                                let recv_cstr = CString::new(recv_str.as_str()).ok()?;
                                let ptr = recv_cstr.as_ptr();
                                owned_strings.push(recv_cstr);
                                Some(ptr)
                            })
                            .unwrap_or(ptr::null());

                        DslEventData {
                            id: event.id,
                            parent_id: event.parent_id.map_or(-1, |id| id as i64),
                            method_name: method_ptr,
                            arguments: args_ptr,
                            arguments_len: args_len,
                            nesting_stack: nesting_ptr,
                            nesting_len,
                            offset: event.offset.start(),
                            has_block: event.has_block,
                            receiver: receiver_ptr,
                        }
                    })
                    .collect()
            })
            .unwrap_or_default();

        Box::into_raw(Box::new(DslEventsIter {
            events,
            _owned_strings: owned_strings,
            _owned_args: owned_args,
            _owned_nesting: owned_nesting,
            index: 0,
        }))
    })
}

/// Returns the number of events in the iterator.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn rdx_dsl_events_iter_len(iter: *const DslEventsIter) -> usize {
    if iter.is_null() {
        return 0;
    }
    unsafe { (*iter).events.len() }
}

/// Gets a pointer to the event at the given index.
/// Returns null if index is out of bounds.
///
/// # Safety
///
/// - `iter` must be a valid pointer from rdx_dsl_events_iter_new
#[unsafe(no_mangle)]
pub unsafe extern "C" fn rdx_dsl_events_iter_get(iter: *const DslEventsIter, index: usize) -> *const DslEventData {
    if iter.is_null() {
        return ptr::null();
    }
    let it = unsafe { &*iter };
    if index >= it.events.len() {
        return ptr::null();
    }
    &it.events[index]
}

/// Advances the iterator and writes the next event data.
/// Returns true if an event was available, false if exhausted.
///
/// # Safety
///
/// - `iter` must be a valid pointer from rdx_dsl_events_iter_new
/// - `out_event` must be a valid writable pointer
#[unsafe(no_mangle)]
pub unsafe extern "C" fn rdx_dsl_events_iter_next(iter: *mut DslEventsIter, out_event: *mut DslEventData) -> bool {
    if iter.is_null() || out_event.is_null() {
        return false;
    }
    let it = unsafe { &mut *iter };
    if it.index >= it.events.len() {
        return false;
    }
    unsafe { ptr::copy_nonoverlapping(&it.events[it.index], out_event, 1) };
    it.index += 1;
    true
}

/// Frees a DSL events iterator.
///
/// # Safety
///
/// - `iter` must be a valid pointer from rdx_dsl_events_iter_new
#[unsafe(no_mangle)]
pub unsafe extern "C" fn rdx_dsl_events_iter_free(iter: *mut DslEventsIter) {
    if !iter.is_null() {
        unsafe {
            let _ = Box::from_raw(iter);
        }
    }
}
