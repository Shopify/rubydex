//! This file provides the C API for the Graph object

use crate::reference_api::{ReferenceKind, ReferencesIter};
use crate::{name_api, utils};
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
    F: FnOnce(&Graph) -> T,
{
    let mut graph = unsafe { Box::from_raw(pointer.cast::<Graph>()) };
    let result = action(&mut graph);
    mem::forget(graph);
    result
}

fn with_mut_graph<F, T>(pointer: GraphPointer, action: F) -> T
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
            .collect::<Vec<u32>>()
            .into_boxed_slice()
    });

    Box::into_raw(Box::new(DeclarationsIter { ids, index: 0 }))
}

/// # Panics
///
/// Will panic if the nesting cannot be transformed into a vector of strings
///
/// # Safety
///
/// Assumes that the `const_name` and `nesting` pointer are valid
#[unsafe(no_mangle)]
pub unsafe extern "C" fn rdx_graph_resolve_constant(
    pointer: GraphPointer,
    const_name: *const c_char,
    nesting: *const *const c_char,
    count: usize,
) -> *const u32 {
    with_mut_graph(pointer, |graph| {
        let nesting: Vec<String> = unsafe { utils::convert_double_pointer_to_vec(nesting, count).unwrap() };
        let const_name: String = unsafe { utils::convert_char_ptr_to_string(const_name).unwrap() };
        let (name_id, names_to_untrack) = name_api::nesting_stack_to_name_id(graph, &const_name, nesting);

        let mut resolver = Resolver::new(graph);

        let resolved_id = match resolver.resolve_constant(name_id) {
            Some(id) => Box::into_raw(Box::new(*id)).cast_const(),
            None => ptr::null(),
        };

        for name_id in names_to_untrack {
            graph.untrack_name(name_id);
        }
        resolved_id
    })
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

    with_mut_graph(pointer, |graph| {
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
    with_mut_graph(pointer, |graph| {
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

    with_mut_graph(pointer, |graph| {
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
    ids: Box<[u32]>,
    /// The current index of the iterator
    index: usize,
}

/// Creates a new iterator over declaration IDs by snapshotting the current set of IDs.
///
/// # Safety
///
/// - `pointer` must be a valid `GraphPointer` previously returned by this crate.
/// - The returned pointer must be freed with `rdx_graph_declarations_iter_free`.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn rdx_graph_declarations_iter_new(pointer: GraphPointer) -> *mut DeclarationsIter {
    // Snapshot the IDs at iterator creation to avoid borrowing across FFI calls
    let ids = with_graph(pointer, |graph| {
        graph
            .declarations()
            .keys()
            .map(|name_id| **name_id)
            .collect::<Vec<u32>>()
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
pub unsafe extern "C" fn rdx_graph_declarations_iter_next(iter: *mut DeclarationsIter, out_id: *mut u32) -> bool {
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
    ids: Box<[u32]>,
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
            .collect::<Vec<u32>>()
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
pub unsafe extern "C" fn rdx_graph_documents_iter_next(iter: *mut DocumentsIter, out_id: *mut u32) -> bool {
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
#[unsafe(no_mangle)]
pub unsafe extern "C" fn rdx_graph_get_declaration(pointer: GraphPointer, name: *const c_char) -> *const u32 {
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
        let refs: Vec<(u32, ReferenceKind)> = graph
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
        let refs: Vec<(u32, ReferenceKind)> = graph
            .method_references()
            .keys()
            .map(|id| (**id, ReferenceKind::Method))
            .collect();

        ReferencesIter::new(refs.into_boxed_slice())
    })
}

#[cfg(test)]
mod tests {
    use rubydex::indexing::ruby_indexer::RubyIndexer;

    use super::*;

    #[test]
    fn names_are_untracked_after_resolving_constant() {
        let mut indexer = RubyIndexer::new(
            "file:///foo.rb".into(),
            "
            class Foo
              BAR = 1
            end
            ",
        );
        indexer.index();

        let mut graph = Graph::new();
        graph.update(indexer.local_graph());
        let mut resolver = Resolver::new(&mut graph);
        resolver.resolve_all();

        assert_eq!(
            1,
            graph
                .names()
                .iter()
                .find_map(|(_, name)| {
                    if graph.strings().get(name.str()).unwrap().as_str() == "BAR" {
                        Some(name)
                    } else {
                        None
                    }
                })
                .unwrap()
                .ref_count()
        );

        let graph_ptr = Box::into_raw(Box::new(graph)) as GraphPointer;

        // Build the nesting array: ["Foo"] since BAR is inside class Foo
        let nesting_strings = [CString::new("Foo").unwrap()];
        let nesting_ptrs: Vec<*const c_char> = nesting_strings.iter().map(|s| s.as_ptr()).collect();

        unsafe {
            let id = rdx_graph_resolve_constant(
                graph_ptr,
                CString::new("BAR").unwrap().as_ptr(),
                nesting_ptrs.as_ptr(),
                nesting_ptrs.len(),
            );
            assert_eq!(*id, *DeclarationId::from("Foo::BAR"));
        };

        let graph = unsafe { Box::from_raw(graph_ptr.cast::<Graph>()) };

        assert_eq!(
            1,
            graph
                .names()
                .iter()
                .find_map(|(_, name)| {
                    if graph.strings().get(name.str()).unwrap().as_str() == "BAR" {
                        Some(name)
                    } else {
                        None
                    }
                })
                .unwrap()
                .ref_count()
        );
    }
}
