//! This file provides the C API for the Graph object

use crate::declaration_api::CDeclaration;
use crate::declaration_api::DeclarationsIter;
use crate::declaration_api::decl_id_from_char_ptr;
use crate::document_api::DocumentsIter;
use crate::reference_api::{CConstantReference, CMethodReference, ConstantReferencesIter, MethodReferencesIter};
use crate::{name_api, utils};
use libc::{c_char, c_void};
use rubydex::indexing::LanguageId;
use rubydex::model::encoding::Encoding;
use rubydex::model::graph::Graph;
use rubydex::model::ids::{DeclarationId, NameId, UriId};
use rubydex::model::keywords;
use rubydex::model::name::NameRef;
use rubydex::model::visibility::Visibility;
use rubydex::query::{CompletionCandidate, CompletionContext, CompletionReceiver};
use rubydex::resolution::Resolver;
use rubydex::{indexing, integrity, listing, query};
use std::ffi::CString;
use std::path::PathBuf;
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

/// Searches the graph using exact substring matching
///
/// # Safety
///
/// Expects both the graph and the query pointers to be valid
#[unsafe(no_mangle)]
pub unsafe extern "C" fn rdx_graph_declarations_search(
    pointer: GraphPointer,
    c_query: *const c_char,
) -> *mut DeclarationsIter {
    {
        let Ok(query) = (unsafe { utils::convert_char_ptr_to_string(c_query) }) else {
            return ptr::null_mut();
        };

        let entries = with_graph(pointer, |graph| {
            query::declaration_search(graph, &query, &query::MatchMode::Exact)
                .into_iter()
                .filter_map(|id| {
                    let decl = graph.declarations().get(&id)?;
                    Some(CDeclaration::from_declaration(id, decl))
                })
                .collect::<Vec<CDeclaration>>()
                .into_boxed_slice()
        });

        DeclarationsIter::new(entries)
    }
}

/// Searches the graph using fuzzy matching
///
/// # Safety
///
/// Expects both the graph and the query pointers to be valid
#[unsafe(no_mangle)]
pub unsafe extern "C" fn rdx_graph_declarations_fuzzy_search(
    pointer: GraphPointer,
    c_query: *const c_char,
) -> *mut DeclarationsIter {
    {
        let Ok(query) = (unsafe { utils::convert_char_ptr_to_string(c_query) }) else {
            return ptr::null_mut();
        };

        let entries = with_graph(pointer, |graph| {
            query::declaration_search(graph, &query, &query::MatchMode::Fuzzy)
                .into_iter()
                .filter_map(|id| {
                    let decl = graph.declarations().get(&id)?;
                    Some(CDeclaration::from_declaration(id, decl))
                })
                .collect::<Vec<CDeclaration>>()
                .into_boxed_slice()
        });

        DeclarationsIter::new(entries)
    }
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
) -> *const CDeclaration {
    with_mut_graph(pointer, |graph| {
        let nesting: Vec<String> = unsafe { utils::convert_double_pointer_to_vec(nesting, count).unwrap() };
        let const_name: String = unsafe { utils::convert_char_ptr_to_string(const_name).unwrap() };

        let Some((name_id, names_to_untrack)) = name_api::nesting_stack_to_name_id(graph, &const_name, nesting) else {
            return ptr::null();
        };

        let mut resolver = Resolver::new(graph);

        let declaration = match resolver.resolve_constant(name_id) {
            Some(id) => {
                let decl = graph.declarations().get(&id).unwrap();
                Box::into_raw(Box::new(CDeclaration::from_declaration(id, decl))).cast_const()
            }
            None => ptr::null(),
        };

        for name_id in names_to_untrack {
            graph.untrack_name(name_id);
        }

        declaration
    })
}

/// Adds paths to exclude from file discovery during indexing.
///
/// # Panics
///
/// Will panic if the given array of C string paths cannot be converted to a `Vec<String>`.
///
/// # Safety
///
/// - `pointer` must be a valid `GraphPointer` previously returned by this crate.
/// - `paths` must be an array of `count` valid, null-terminated UTF-8 strings.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn rdx_graph_exclude_paths(pointer: GraphPointer, paths: *const *const c_char, count: usize) {
    let paths: Vec<String> = unsafe { utils::convert_double_pointer_to_vec(paths, count).unwrap() };
    let path_bufs: Vec<PathBuf> = paths.into_iter().map(PathBuf::from).collect();
    with_mut_graph(pointer, |graph| graph.exclude_paths(path_bufs));
}

/// Returns the currently excluded paths as an array of C strings. Writes the count to `out_count`. Returns NULL if no
/// paths are excluded. Caller must free with `free_c_string_array`.
///
/// # Safety
///
/// - `pointer` must be a valid `GraphPointer` previously returned by this crate.
/// - `out_count` must be a valid, writable pointer.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn rdx_graph_excluded_paths(
    pointer: GraphPointer,
    out_count: *mut usize,
) -> *const *const c_char {
    with_graph(pointer, |graph| {
        let excluded = graph.excluded_paths();

        if excluded.is_empty() {
            unsafe { *out_count = 0 };
            return ptr::null();
        }

        let c_strings: Vec<*const c_char> = excluded
            .iter()
            .filter_map(|path| {
                CString::new(path.to_string_lossy().as_ref())
                    .ok()
                    .map(|c_string| c_string.into_raw().cast_const())
            })
            .collect();

        unsafe { *out_count = c_strings.len() };

        let boxed = c_strings.into_boxed_slice();
        Box::into_raw(boxed).cast::<*const c_char>()
    })
}

/// Indexes all given file paths in parallel using the provided Graph pointer.
/// Returns an array of error message strings and writes the count to `out_error_count`.
/// Returns NULL if there are no errors. Caller must free with `free_c_string_array`.
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
    out_error_count: *mut usize,
) -> *const *const c_char {
    let file_paths: Vec<String> = unsafe { utils::convert_double_pointer_to_vec(file_paths, count).unwrap() };

    with_mut_graph(pointer, |graph| {
        let (file_paths, listing_errors) = listing::collect_file_paths(file_paths, graph.excluded_paths());
        let indexing_errors = indexing::index_files(graph, file_paths);

        let all_errors: Vec<String> = listing_errors
            .into_iter()
            .chain(indexing_errors)
            .map(|e| e.to_string())
            .collect();

        if all_errors.is_empty() {
            unsafe { *out_error_count = 0 };
            return ptr::null();
        }

        let c_strings: Vec<*const c_char> = all_errors
            .into_iter()
            .filter_map(|string| {
                CString::new(string)
                    .ok()
                    .map(|c_string| c_string.into_raw().cast_const())
            })
            .collect();

        unsafe { *out_error_count = c_strings.len() };

        let boxed = c_strings.into_boxed_slice();
        Box::into_raw(boxed).cast::<*const c_char>()
    })
}

/// Returns a pointer to the URI ID of the document identified by `uri`, or NULL if it doesn't exist.
/// Caller must free the returned pointer with `free_u64`.
///
/// # Safety
///
/// Expects both the graph pointer and uri string pointer to be valid
#[unsafe(no_mangle)]
pub unsafe extern "C" fn rdx_graph_get_document(pointer: GraphPointer, uri: *const c_char) -> *const u64 {
    let Ok(uri_str) = (unsafe { utils::convert_char_ptr_to_string(uri) }) else {
        return ptr::null();
    };

    with_graph(pointer, |graph| {
        let uri_id = UriId::from(uri_str.as_str());

        if graph.documents().contains_key(&uri_id) {
            Box::into_raw(Box::new(*uri_id)).cast_const()
        } else {
            ptr::null()
        }
    })
}

/// Deletes a document and all of its definitions from the graph.
/// Returns a pointer to the URI ID if the document was found and removed, or NULL if it didn't exist.
/// Caller must free the returned pointer with `free_u64`.
///
/// # Safety
///
/// Expects both the graph pointer and uri string pointer to be valid
#[unsafe(no_mangle)]
pub unsafe extern "C" fn rdx_graph_delete_document(pointer: GraphPointer, uri: *const c_char) -> *const u64 {
    let Ok(uri_str) = (unsafe { utils::convert_char_ptr_to_string(uri) }) else {
        return ptr::null();
    };

    with_mut_graph(pointer, |graph| match graph.delete_document(&uri_str) {
        Some(uri_id) => Box::into_raw(Box::new(*uri_id)),
        None => ptr::null(),
    })
}

/// Runs the resolver to compute declarations, ownership and related structures
#[unsafe(no_mangle)]
pub extern "C" fn rdx_graph_resolve(pointer: GraphPointer) {
    with_mut_graph(pointer, |graph| {
        let mut resolver = Resolver::new(graph);
        resolver.resolve();
    });
}

/// Checks the integrity of the graph and returns an array of error message strings. Returns NULL if there are no
/// errors. Caller must free with `free_c_string_array`.
///
/// # Safety
///
/// - `pointer` must be a valid `GraphPointer` previously returned by this crate.
/// - `out_error_count` must be a valid, writable pointer.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn rdx_check_integrity(
    pointer: GraphPointer,
    out_error_count: *mut usize,
) -> *const *const c_char {
    with_graph(pointer, |graph| {
        let errors = integrity::check_integrity(graph);

        if errors.is_empty() {
            unsafe { *out_error_count = 0 };
            return ptr::null();
        }

        let c_strings: Vec<*const c_char> = errors
            .into_iter()
            .filter_map(|error| {
                CString::new(error.to_string())
                    .ok()
                    .map(|c_string| c_string.into_raw().cast_const())
            })
            .collect();

        unsafe { *out_error_count = c_strings.len() };

        let boxed = c_strings.into_boxed_slice();
        Box::into_raw(boxed).cast::<*const c_char>()
    })
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

/// Creates a new iterator over declaration IDs by snapshotting the current set of IDs.
///
/// # Safety
///
/// - `pointer` must be a valid `GraphPointer` previously returned by this crate.
/// - The returned pointer must be freed with `rdx_graph_declarations_iter_free`.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn rdx_graph_declarations_iter_new(pointer: GraphPointer) -> *mut DeclarationsIter {
    // Snapshot the declarations at iterator creation to avoid borrowing across FFI calls
    let entries = with_graph(pointer, |graph| {
        graph
            .declarations()
            .iter()
            .map(|(id, decl)| CDeclaration::from_declaration(*id, decl))
            .collect::<Vec<CDeclaration>>()
            .into_boxed_slice()
    });

    DeclarationsIter::new(entries)
}

/// Creates a new iterator over document (URI) IDs by snapshotting the current set of IDs.
///
/// # Safety
///
/// - `pointer` must be a valid `GraphPointer` previously returned by this crate.
/// - The returned pointer must be freed with `rdx_graph_documents_iter_free`.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn rdx_graph_documents_iter_new(pointer: GraphPointer) -> *mut DocumentsIter {
    let entries = with_graph(pointer, |graph| {
        graph
            .documents()
            .keys()
            .map(|uri_id| **uri_id)
            .collect::<Vec<_>>()
            .into_boxed_slice()
    });

    DocumentsIter::new(entries)
}

/// Attempts to resolve a declaration from a fully-qualified name string.
/// Returns a `CDeclaration` pointer if it exists, or NULL if it does not.
///
/// # Safety
/// - `pointer` must be a valid `GraphPointer`
/// - `name` must be a valid, null-terminated UTF-8 string
#[unsafe(no_mangle)]
pub unsafe extern "C" fn rdx_graph_get_declaration(pointer: GraphPointer, name: *const c_char) -> *const CDeclaration {
    let Ok(name_str) = (unsafe { utils::convert_char_ptr_to_string(name) }) else {
        return ptr::null();
    };

    with_graph(pointer, |graph| {
        let decl_id = DeclarationId::from(name_str.as_str());

        if let Some(decl) = graph.declarations().get(&decl_id) {
            Box::into_raw(Box::new(CDeclaration::from_declaration(decl_id, decl))).cast_const()
        } else {
            ptr::null()
        }
    })
}

/// Creates a new iterator over constant references by snapshotting the current set of IDs.
///
/// # Safety
/// - `pointer` must be a valid `GraphPointer` previously returned by this crate.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn rdx_graph_constant_references_iter_new(pointer: GraphPointer) -> *mut ConstantReferencesIter {
    with_graph(pointer, |graph| {
        let refs: Vec<_> = graph
            .constant_references()
            .iter()
            .map(|(id, cref)| {
                let declaration_id = graph
                    .names()
                    .get(cref.name_id())
                    .and_then(|name_ref| match name_ref {
                        NameRef::Resolved(resolved) => Some(**resolved.declaration_id()),
                        NameRef::Unresolved(_) => None,
                    })
                    .unwrap_or(0);

                CConstantReference {
                    id: **id,
                    declaration_id,
                }
            })
            .collect();

        ConstantReferencesIter::new(refs.into_boxed_slice())
    })
}

/// Creates a new iterator over method references by snapshotting the current set of IDs.
///
/// # Safety
/// - `pointer` must be a valid `GraphPointer` previously returned by this crate.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn rdx_graph_method_references_iter_new(pointer: GraphPointer) -> *mut MethodReferencesIter {
    with_graph(pointer, |graph| {
        let refs: Vec<_> = graph
            .method_references()
            .keys()
            .map(|id| CMethodReference { id: **id })
            .collect();

        MethodReferencesIter::new(refs.into_boxed_slice())
    })
}

/// Resolves a require path to its document URI ID.
/// Returns a pointer to the URI ID if found, or NULL if not found.
/// Caller must free with the returned pointer.
///
/// # Safety
/// - `pointer` must be a valid `GraphPointer` previously returned by this crate.
/// - `require_path` must be a valid, null-terminated UTF-8 string.
/// - `load_paths` must be an array of `load_paths_count` valid, null-terminated UTF-8 strings.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn rdx_resolve_require_path(
    pointer: GraphPointer,
    require_path: *const c_char,
    load_paths: *const *const c_char,
    load_paths_count: usize,
) -> *const u64 {
    let Ok(path_str) = (unsafe { utils::convert_char_ptr_to_string(require_path) }) else {
        return ptr::null();
    };

    let Ok(paths_vec) = (unsafe { utils::convert_double_pointer_to_vec(load_paths, load_paths_count) }) else {
        return ptr::null();
    };
    let paths_vec = paths_vec.into_iter().map(PathBuf::from).collect::<Vec<_>>();

    with_graph(pointer, |graph| {
        query::resolve_require_path(graph, &path_str, &paths_vec).map_or(ptr::null(), |id| Box::into_raw(Box::new(*id)))
    })
}

/// Returns all require paths for completion.
/// Returns array of C strings and writes count to `out_count`.
/// Returns null if `load_path` contain invalid UTF-8.
/// Caller must free with `free_c_string_array`.
///
/// # Safety
/// - `pointer` must be a valid `GraphPointer` previously returned by this crate.
/// - `load_path` must be an array of `load_path_count` valid, null-terminated UTF-8 strings.
/// - `out_count` must be a valid, writable pointer.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn rdx_require_paths(
    pointer: GraphPointer,
    load_path: *const *const c_char,
    load_path_count: usize,
    out_count: *mut usize,
) -> *const *const c_char {
    let Ok(paths_vec) = (unsafe { utils::convert_double_pointer_to_vec(load_path, load_path_count) }) else {
        return ptr::null_mut();
    };
    let paths_vec = paths_vec.into_iter().map(PathBuf::from).collect::<Vec<_>>();

    let results = with_graph(pointer, |graph| query::require_paths(graph, &paths_vec));

    let c_strings: Vec<*const c_char> = results
        .into_iter()
        .filter_map(|string| {
            CString::new(string)
                .ok()
                .map(|c_string| c_string.into_raw().cast_const())
        })
        .collect();

    unsafe { *out_count = c_strings.len() };

    let boxed = c_strings.into_boxed_slice();
    Box::into_raw(boxed).cast::<*const c_char>()
}

#[repr(C)]
pub enum IndexSourceResult {
    Success = 0,
    InvalidUri = 1,
    InvalidSource = 2,
    InvalidLanguageId = 3,
    UnsupportedLanguageId = 4,
}

/// Indexes source code from memory using the specified language.  Returns `IndexSourceResult::Success` on success
/// or a specific error variant if string conversion or language lookup fails.
///
/// # Safety
///
/// - `pointer` must be a valid `GraphPointer` previously returned by this crate.
/// - `uri` and `language_id` must be valid, null-terminated UTF-8 strings.
/// - `source` must point to a valid UTF-8 byte buffer of at least `source_len` bytes.
///   It may contain null bytes.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn rdx_index_source(
    pointer: GraphPointer,
    uri: *const c_char,
    source: *const c_char,
    source_len: usize,
    language_id: *const c_char,
) -> IndexSourceResult {
    let Ok(uri_str) = (unsafe { utils::convert_char_ptr_to_string(uri) }) else {
        return IndexSourceResult::InvalidUri;
    };

    let source_bytes = unsafe { std::slice::from_raw_parts(source.cast::<u8>(), source_len) };
    let Ok(source_str) = std::str::from_utf8(source_bytes) else {
        return IndexSourceResult::InvalidSource;
    };

    let Ok(language_id_str) = (unsafe { utils::convert_char_ptr_to_string(language_id) }) else {
        return IndexSourceResult::InvalidLanguageId;
    };

    let Ok(language) = LanguageId::from_language_id(&language_id_str) else {
        return IndexSourceResult::UnsupportedLanguageId;
    };

    with_mut_graph(pointer, |graph| {
        indexing::index_source(graph, &uri_str, source_str, &language);
        IndexSourceResult::Success
    })
}

#[repr(C)]
#[derive(Debug, Clone, Copy)]
pub enum CCompletionCandidateKind {
    Declaration = 0,
    Keyword = 1,
    KeywordParameter = 2,
}

#[repr(C)]
pub struct CCompletionCandidate {
    pub kind: CCompletionCandidateKind,
    /// Only valid when `kind == Declaration`; null otherwise.
    pub declaration: *const CDeclaration,
    pub name: *const c_char,
    pub documentation: *const c_char,
}

#[repr(C)]
pub struct CompletionCandidateArray {
    pub items: *mut CCompletionCandidate,
    pub len: usize,
}

impl CompletionCandidateArray {
    fn from_vec(entries: Vec<CCompletionCandidate>) -> *mut CompletionCandidateArray {
        let mut boxed = entries.into_boxed_slice();
        let len = boxed.len();
        let ptr = boxed.as_mut_ptr();
        mem::forget(boxed);
        Box::into_raw(Box::new(CompletionCandidateArray { items: ptr, len }))
    }
}

/// Frees a completion candidate array previously returned by a completion function.
///
/// # Safety
///
/// - `ptr` must be a valid pointer previously returned by a completion function.
/// - `ptr` must not be used after being freed.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn rdx_completion_candidates_free(ptr: *mut CompletionCandidateArray) {
    if ptr.is_null() {
        return;
    }

    let array = unsafe { Box::from_raw(ptr) };

    if !array.items.is_null() && array.len > 0 {
        let slice_ptr = ptr::slice_from_raw_parts_mut(array.items, array.len);
        let mut boxed_slice: Box<[CCompletionCandidate]> = unsafe { Box::from_raw(slice_ptr) };

        for entry in &mut *boxed_slice {
            if !entry.declaration.is_null() {
                let _ = unsafe { Box::from_raw(entry.declaration.cast_mut()) };
            }
            if !entry.name.is_null() {
                let _ = unsafe { CString::from_raw(entry.name.cast_mut()) };
            }
            if !entry.documentation.is_null() {
                let _ = unsafe { CString::from_raw(entry.documentation.cast_mut()) };
            }
        }
    }
}

/// Converts the nesting stack into a `NameId`.
/// The last element of the nesting stack is treated as the self type; if the stack is empty, `"Object"` is used.
///
/// Returns `Err` if the nesting array contains invalid UTF-8.
///
/// # Safety
///
/// `nesting` must point to `nesting_count` valid, null-terminated UTF-8 strings.
unsafe fn completion_nesting_name_id(
    graph: &mut Graph,
    nesting: *const *const c_char,
    nesting_count: usize,
) -> Option<(NameId, Vec<NameId>)> {
    let mut nesting: Vec<String> = unsafe { utils::convert_double_pointer_to_vec(nesting, nesting_count).ok()? };

    // When serving completion in a bare script, the self (top level) context is Object
    let self_name = if nesting.is_empty() {
        "Object".to_string()
    } else {
        nesting.pop().unwrap()
    };

    name_api::nesting_stack_to_name_id(graph, &self_name, nesting)
}

/// The result of a completion operation, carrying either a candidate array or an error message.
#[repr(C)]
pub struct CompletionResult {
    /// Non-null on success; null on error.
    pub candidates: *mut CompletionCandidateArray,
    /// Non-null on error; null on success. Caller must free with `free_c_string`.
    pub error: *const c_char,
}

impl CompletionResult {
    fn success(candidates: *mut CompletionCandidateArray) -> Self {
        Self {
            candidates,
            error: ptr::null(),
        }
    }

    fn error(message: &str) -> Self {
        Self {
            candidates: ptr::null_mut(),
            error: CString::new(message).map_or(ptr::null(), |s| s.into_raw().cast_const()),
        }
    }
}

/// Runs completion for the given receiver and returns a structured result with candidates or an error message
fn run_and_finalize_completion(
    graph: &mut Graph,
    receiver: CompletionReceiver,
    names_to_untrack: Vec<NameId>,
) -> CompletionResult {
    let candidates = match query::completion_candidates(graph, CompletionContext::new(receiver)) {
        Ok(candidates) => candidates,
        Err(e) => {
            for name_id in names_to_untrack {
                graph.untrack_name(name_id);
            }
            return CompletionResult::error(&e.to_string());
        }
    };

    let entries: Vec<CCompletionCandidate> = candidates
        .into_iter()
        .map(|candidate| match candidate {
            CompletionCandidate::Declaration(id) => {
                let decl = graph
                    .declarations()
                    .get(&id)
                    .expect("completion candidate declaration must exist in graph");
                CCompletionCandidate {
                    kind: CCompletionCandidateKind::Declaration,
                    declaration: Box::into_raw(Box::new(CDeclaration::from_declaration(id, decl))),
                    name: ptr::null(),
                    documentation: ptr::null(),
                }
            }
            CompletionCandidate::Keyword(kw) => CCompletionCandidate {
                kind: CCompletionCandidateKind::Keyword,
                declaration: ptr::null(),
                name: CString::new(kw.name())
                    .expect("keyword name must not contain NUL")
                    .into_raw()
                    .cast_const(),
                documentation: CString::new(kw.documentation())
                    .expect("keyword documentation must not contain NUL")
                    .into_raw()
                    .cast_const(),
            },
            CompletionCandidate::KeywordArgument(str_id) => {
                let name_str = graph
                    .strings()
                    .get(&str_id)
                    .expect("keyword argument string must exist in graph");
                CCompletionCandidate {
                    kind: CCompletionCandidateKind::KeywordParameter,
                    declaration: ptr::null(),
                    name: CString::new(name_str.as_str())
                        .expect("keyword argument name must not contain NUL")
                        .into_raw()
                        .cast_const(),
                    documentation: ptr::null(),
                }
            }
        })
        .collect();

    for name_id in names_to_untrack {
        graph.untrack_name(name_id);
    }

    CompletionResult::success(CompletionCandidateArray::from_vec(entries))
}

/// Returns expression completion candidates.
/// The caller must free candidates with `rdx_completion_candidates_free`
/// and the error string (if non-null) with `free_c_string`.
///
/// # Safety
///
/// - `pointer` must be a valid `GraphPointer` previously returned by this crate.
/// - `nesting` must point to `nesting_count` valid, null-terminated UTF-8 strings.
/// - `self_receiver` must be null or a valid, null-terminated UTF-8 string. When non-null, it
///   overrides the self-type (e.g., `"Foo::<Foo>"` for completion inside `def Foo.bar`), while
///   the lexical nesting still comes from `nesting`.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn rdx_graph_complete_expression(
    pointer: GraphPointer,
    nesting: *const *const c_char,
    nesting_count: usize,
    self_receiver: *const c_char,
) -> CompletionResult {
    with_mut_graph(pointer, |graph| {
        let Some((name_id, names_to_untrack)) = (unsafe { completion_nesting_name_id(graph, nesting, nesting_count) })
        else {
            return CompletionResult::success(ptr::null_mut());
        };

        let self_decl_id = unsafe { decl_id_from_char_ptr(self_receiver) };

        run_and_finalize_completion(
            graph,
            CompletionReceiver::Expression {
                self_decl_id,
                nesting_name_id: name_id,
            },
            names_to_untrack,
        )
    })
}

/// Returns namespace access completion candidates.
/// The caller must free candidates with `rdx_completion_candidates_free`
/// and the error string (if non-null) with `free_c_string`.
///
/// # Safety
///
/// - `pointer` must be a valid `GraphPointer` previously returned by this crate.
/// - `name` must be a valid, null-terminated UTF-8 string (FQN of the namespace).
#[unsafe(no_mangle)]
pub unsafe extern "C" fn rdx_graph_complete_namespace_access(
    pointer: GraphPointer,
    name: *const c_char,
) -> CompletionResult {
    let Ok(name_str) = (unsafe { utils::convert_char_ptr_to_string(name) }) else {
        return CompletionResult::success(ptr::null_mut());
    };

    with_mut_graph(pointer, |graph| {
        run_and_finalize_completion(
            graph,
            CompletionReceiver::NamespaceAccess(DeclarationId::from(name_str.as_str())),
            Vec::new(),
        )
    })
}

/// Returns method call completion candidates.
/// The caller must free candidates with `rdx_completion_candidates_free`
/// and the error string (if non-null) with `free_c_string`.
///
/// # Safety
///
/// - `pointer` must be a valid `GraphPointer` previously returned by this crate.
/// - `name` must be a valid, null-terminated UTF-8 string (FQN of the receiver).
#[unsafe(no_mangle)]
pub unsafe extern "C" fn rdx_graph_complete_method_call(
    pointer: GraphPointer,
    name: *const c_char,
) -> CompletionResult {
    let Ok(name_str) = (unsafe { utils::convert_char_ptr_to_string(name) }) else {
        return CompletionResult::success(ptr::null_mut());
    };

    with_mut_graph(pointer, |graph| {
        run_and_finalize_completion(
            graph,
            CompletionReceiver::MethodCall(DeclarationId::from(name_str.as_str())),
            Vec::new(),
        )
    })
}

/// Returns method argument completion candidates.
/// The caller must free candidates with `rdx_completion_candidates_free`
/// and the error string (if non-null) with `free_c_string`.
///
/// # Safety
///
/// - `pointer` must be a valid `GraphPointer` previously returned by this crate.
/// - `name` must be a valid, null-terminated UTF-8 string (FQN of the method).
/// - `nesting` must point to `nesting_count` valid, null-terminated UTF-8 strings.
/// - `self_receiver` must be null or a valid, null-terminated UTF-8 string. See
///   `rdx_graph_complete_expression` for semantics.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn rdx_graph_complete_method_argument(
    pointer: GraphPointer,
    name: *const c_char,
    nesting: *const *const c_char,
    nesting_count: usize,
    self_receiver: *const c_char,
) -> CompletionResult {
    let Ok(name_str) = (unsafe { utils::convert_char_ptr_to_string(name) }) else {
        return CompletionResult::success(ptr::null_mut());
    };

    with_mut_graph(pointer, |graph| {
        let Some((nesting_name_id, names_to_untrack)) =
            (unsafe { completion_nesting_name_id(graph, nesting, nesting_count) })
        else {
            return CompletionResult::success(ptr::null_mut());
        };

        let self_decl_id = unsafe { decl_id_from_char_ptr(self_receiver) };

        run_and_finalize_completion(
            graph,
            CompletionReceiver::MethodArgument {
                self_decl_id,
                nesting_name_id,
                method_decl_id: DeclarationId::from(name_str.as_str()),
            },
            names_to_untrack,
        )
    })
}

#[repr(C)]
pub struct CKeyword {
    name: *const c_char,
    documentation: *const c_char,
}

/// Looks up a Ruby keyword by its exact name.
/// Returns a heap-allocated `CKeyword` if found, or NULL if the name is not a keyword.
/// Caller must free with `rdx_keyword_free`.
///
/// # Safety
///
/// - `name` must be a valid, null-terminated UTF-8 string.
///
/// # Panics
///
/// Will panic if the keyword's name or documentation contains an internal NUL byte
#[unsafe(no_mangle)]
pub unsafe extern "C" fn rdx_keyword_get(name: *const c_char) -> *const CKeyword {
    let Ok(name_str) = (unsafe { utils::convert_char_ptr_to_string(name) }) else {
        return ptr::null();
    };

    match keywords::get(&name_str) {
        Some(kw) => {
            let c_name = CString::new(kw.name())
                .expect("keyword name must not contain NUL")
                .into_raw()
                .cast_const();

            let c_doc = CString::new(kw.documentation())
                .expect("keyword documentation must not contain NUL")
                .into_raw()
                .cast_const();

            Box::into_raw(Box::new(CKeyword {
                name: c_name,
                documentation: c_doc,
            }))
            .cast_const()
        }
        None => ptr::null(),
    }
}

#[repr(u8)]
#[derive(Debug, Clone, Copy)]
pub enum CVisibility {
    Public = 0,
    Protected = 1,
    Private = 2,
}

/// Returns the visibility of a declaration (method, constant, class, or module) as a heap-allocated
/// `CVisibility`, or NULL when the declaration carries no visibility (e.g. variables, singleton
/// classes, todos). Caller must free the returned pointer with `free_c_visibility`.
///
/// # Safety
///
/// - `pointer` must be a valid `GraphPointer` previously returned by this crate.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn rdx_graph_visibility(pointer: GraphPointer, declaration_id: u64) -> *const CVisibility {
    with_graph(pointer, |graph| {
        let Some(visibility) = graph.visibility(&DeclarationId::new(declaration_id)) else {
            return ptr::null();
        };

        let c_visibility = match visibility {
            Visibility::Public => CVisibility::Public,
            Visibility::Protected => CVisibility::Protected,
            Visibility::Private => CVisibility::Private,
            Visibility::ModuleFunction => {
                unimplemented!("module_function visibility translation is not implemented yet")
            }
        };

        Box::into_raw(Box::new(c_visibility)).cast_const()
    })
}

/// Frees a `CVisibility` previously returned by `rdx_graph_visibility`.
///
/// # Safety
///
/// - `ptr` must be a valid pointer previously returned by `rdx_graph_visibility`.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn free_c_visibility(ptr: *const CVisibility) {
    unsafe {
        let _ = Box::from_raw(ptr.cast_mut());
    }
}

/// Frees a `CKeyword` previously returned by `rdx_keyword_get`.
///
/// # Safety
///
/// - `ptr` must be a valid pointer previously returned by `rdx_keyword_get`, or NULL.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn rdx_keyword_free(ptr: *const CKeyword) {
    if ptr.is_null() {
        return;
    }

    let kw = unsafe { Box::from_raw(ptr.cast_mut()) };

    if !kw.name.is_null() {
        let _ = unsafe { CString::from_raw(kw.name.cast_mut()) };
    }

    if !kw.documentation.is_null() {
        let _ = unsafe { CString::from_raw(kw.documentation.cast_mut()) };
    }
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
        graph.consume_document_changes(indexer.local_graph());
        let mut resolver = Resolver::new(&mut graph);
        resolver.resolve();

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
            let decl = rdx_graph_resolve_constant(
                graph_ptr,
                CString::new("BAR").unwrap().as_ptr(),
                nesting_ptrs.as_ptr(),
                nesting_ptrs.len(),
            );
            assert_eq!((*decl).id(), *DeclarationId::from("Foo::BAR"));
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
