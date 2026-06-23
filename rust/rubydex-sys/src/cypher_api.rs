//! This file provides the C API for Cypher query parsing, schema, and structured execution.
//! It connects to the graph through `graph_api::with_graph`.

use crate::declaration_api::CDeclaration;
use crate::definition_api::map_definition_to_kind;
use crate::graph_api::{GraphPointer, with_graph};
use crate::utils;
use libc::{c_char, c_void};
use rubydex::model::graph::Graph;
use rubydex::query::cypher::schema::NodeRef;
use rubydex::query::cypher::{self, CypherValue, OutputFormat};
use std::ffi::CString;
use std::ptr;

/// The result of running a Cypher query, carrying either the formatted output or an error message.
#[repr(C)]
pub struct CQueryResult {
    /// Non-null on success; null on error. Caller must free with `free_c_string`.
    pub output: *const c_char,
    /// Non-null on error; null on success. Caller must free with `free_c_string`.
    pub error: *const c_char,
}

impl CQueryResult {
    #[must_use]
    pub fn success(output: &str) -> Self {
        match CString::new(output) {
            Ok(c_string) => Self {
                output: c_string.into_raw().cast_const(),
                error: ptr::null(),
            },
            Err(_) => Self::error("query output contained an interior NUL byte"),
        }
    }

    #[must_use]
    pub fn error(message: &str) -> Self {
        Self {
            output: ptr::null(),
            error: utils::cstring_raw(message),
        }
    }
}

/// The result of parsing a Cypher query into an opaque, reusable parsed-query object.
#[repr(C)]
pub struct CParseResult {
    /// Non-null on success: a heap-allocated parsed query. Free with `rdx_cypher_query_free`.
    pub query: *mut c_void,
    /// Non-null on error; null on success. Caller must free with `free_c_string`.
    pub error: *const c_char,
}

/// Parses a Cypher query string into an opaque parsed-query object, without needing a graph.
///
/// On success, `query` is a heap-allocated parsed query that can be executed against a graph with
/// `rdx_query_run` and must eventually be freed with `rdx_cypher_query_free`. On failure, `error`
/// holds the message.
///
/// # Safety
///
/// - `query` must be a valid, null-terminated UTF-8 string.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn rdx_cypher_parse(query: *const c_char) -> CParseResult {
    let Ok(query_str) = (unsafe { utils::convert_char_ptr_to_string(query) }) else {
        return CParseResult {
            query: ptr::null_mut(),
            error: utils::cstring_raw("query is not valid UTF-8"),
        };
    };

    match cypher::parse(&query_str) {
        Ok(parsed) => CParseResult {
            query: Box::into_raw(Box::new(parsed)).cast::<c_void>(),
            error: ptr::null(),
        },
        Err(error) => CParseResult {
            query: ptr::null_mut(),
            error: utils::cstring_raw(&error.to_string()),
        },
    }
}

/// Frees a parsed query previously returned by `rdx_cypher_parse`.
///
/// # Safety
///
/// - `query` must be a pointer returned by `rdx_cypher_parse`, or null. It must not be used after.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn rdx_cypher_query_free(query: *mut c_void) {
    if query.is_null() {
        return;
    }
    let _ = unsafe { Box::from_raw(query.cast::<cypher::Query>()) };
}

/// Returns a description of the queryable Cypher schema (node labels, relationship types, and
/// properties) in the given format (`"table"` or `"json"`). The schema is static and requires no
/// graph. Caller must free the returned pointer with `free_c_string`.
///
/// # Safety
///
/// - `format` must be a valid, null-terminated UTF-8 string.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn rdx_cypher_schema(format: *const c_char) -> *const c_char {
    let format_str = unsafe { utils::convert_char_ptr_to_string(format) }.unwrap_or_else(|_| "table".to_string());
    let output_format = if format_str == "json" {
        OutputFormat::Json
    } else {
        OutputFormat::Table
    };

    utils::cstring_raw(&cypher::schema(output_format))
}

// ---------------------------------------------------------------------------
// Structured result types (object-returning query execution)
// ---------------------------------------------------------------------------

/// Tag for a structured result cell.
#[repr(C)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CCellTag {
    Null = 0,
    Bool = 1,
    Int = 2,
    Str = 3,
    Node = 4,
    List = 5,
    Map = 6,
}

/// Which family of graph node a `Node` cell refers to (selects the Ruby handle class family).
#[repr(C)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CNodeCategory {
    Declaration = 0,
    Definition = 1,
    Document = 2,
}

/// `Node` cell payload: which handle family to build, the kind value, and the entity id.
#[repr(C)]
#[derive(Debug, Clone, Copy)]
pub struct CNode {
    /// Which handle family to build.
    pub category: CNodeCategory,
    /// The `CDeclarationKind`/`DefinitionKind` value (ignored for documents).
    pub kind: u32,
    /// The entity id to build the handle from.
    pub id: u64,
}

/// `List` cell payload: a heap array of nested cells (freed by `rdx_rows_iter_free`).
#[repr(C)]
#[derive(Debug, Clone, Copy)]
pub struct CList {
    pub items: *mut CCell,
    pub len: usize,
}

/// `Map` cell payload: parallel heap arrays of owned key strings and their value cells, in order
/// (both `len` long, freed by `rdx_rows_iter_free`). Stored as separate arrays rather than an
/// entry struct so no `CCell` is embedded by value (keeping the generated header well-ordered).
#[repr(C)]
#[derive(Debug, Clone, Copy)]
pub struct CMap {
    pub keys: *mut *const c_char,
    pub values: *mut CCell,
    pub len: usize,
}

/// Payload of a `CCell`. The active field is selected by the cell's `tag`; reading any other field
/// is undefined. `Null` carries no payload.
#[repr(C)]
#[derive(Clone, Copy)]
pub union CCellPayload {
    /// `Bool`.
    pub bool_val: bool,
    /// `Int`.
    pub int_val: i64,
    /// `Str`: owned C string (freed by `rdx_rows_iter_free`).
    pub str_val: *const c_char,
    /// `Node`.
    pub node: CNode,
    /// `List`.
    pub list: CList,
    /// `Map`.
    pub map: CMap,
}

/// A single structured result value: a `tag` discriminant plus a `payload` union whose active
/// field the tag selects.
#[repr(C)]
pub struct CCell {
    pub tag: CCellTag,
    pub payload: CCellPayload,
}

impl CCell {
    fn new(tag: CCellTag, payload: CCellPayload) -> Self {
        Self { tag, payload }
    }

    fn null() -> Self {
        Self {
            tag: CCellTag::Null,
            payload: CCellPayload { int_val: 0 },
        }
    }
}

/// One row of structured cells.
#[repr(C)]
#[derive(Clone, Copy)]
pub struct CResultRow {
    pub cells: *mut CCell,
    pub len: usize,
}

/// Iterator over structured query result rows. Opaque from the C side — use
/// `rdx_rows_iter_column_count`, `rdx_rows_iter_columns`, `rdx_rows_iter_len`, `rdx_rows_iter_next`,
/// and `rdx_rows_iter_free`.
pub struct CRowsIter {
    columns: Box<[*const c_char]>,
    rows: Box<[CResultRow]>,
    index: usize,
}

/// The result of running a query for structured rows: either a row iterator or an error message.
#[repr(C)]
pub struct CRunRows {
    /// Non-null on success; free with `rdx_rows_iter_free`.
    pub iter: *mut CRowsIter,
    /// Non-null on error; free with `free_c_string`.
    pub error: *const c_char,
}

/// Converts a `CypherValue` into a `CCell`, resolving node identity to a handle-buildable category +
/// kind + id. A node whose id cannot be decoded or found falls back to its display name as a string.
fn build_cell(graph: &Graph, value: &CypherValue) -> CCell {
    match value {
        CypherValue::Null => CCell::null(),
        CypherValue::Bool(b) => CCell::new(CCellTag::Bool, CCellPayload { bool_val: *b }),
        CypherValue::Int(i) => CCell::new(CCellTag::Int, CCellPayload { int_val: *i }),
        CypherValue::Str(s) => CCell::new(
            CCellTag::Str,
            CCellPayload {
                str_val: utils::cstring_raw(s),
            },
        ),
        CypherValue::List(items) => {
            let cells: Vec<CCell> = items.iter().map(|item| build_cell(graph, item)).collect();
            let len = cells.len();
            let items = if cells.is_empty() {
                ptr::null_mut()
            } else {
                Box::into_raw(cells.into_boxed_slice()).cast::<CCell>()
            };
            CCell::new(
                CCellTag::List,
                CCellPayload {
                    list: CList { items, len },
                },
            )
        }
        CypherValue::Map(pairs) => {
            let len = pairs.len();
            let mut keys: Vec<*const c_char> = Vec::with_capacity(len);
            let mut values: Vec<CCell> = Vec::with_capacity(len);
            for (key, val) in pairs {
                keys.push(utils::cstring_raw(key));
                values.push(build_cell(graph, val));
            }
            let (keys, values) = if len == 0 {
                (ptr::null_mut(), ptr::null_mut())
            } else {
                (
                    Box::into_raw(keys.into_boxed_slice()).cast::<*const c_char>(),
                    Box::into_raw(values.into_boxed_slice()).cast::<CCell>(),
                )
            };
            CCell::new(
                CCellTag::Map,
                CCellPayload {
                    map: CMap { keys, values, len },
                },
            )
        }
        CypherValue::Node { id, name, .. } => build_node_cell(graph, id).unwrap_or_else(|| {
            CCell::new(
                CCellTag::Str,
                CCellPayload {
                    str_val: utils::cstring_raw(name),
                },
            )
        }),
    }
}

/// Builds a `Node` cell by decoding the opaque node id and looking up its kind in the graph.
fn build_node_cell(graph: &Graph, encoded_id: &str) -> Option<CCell> {
    match NodeRef::decode(encoded_id)? {
        NodeRef::Declaration(id) => {
            let kind = CDeclaration::kind_from_declaration(graph.declarations().get(&id)?);
            Some(CCell::new(
                CCellTag::Node,
                CCellPayload {
                    node: CNode {
                        category: CNodeCategory::Declaration,
                        kind: kind as u32,
                        id: *id,
                    },
                },
            ))
        }
        NodeRef::Definition(id) => {
            let kind = map_definition_to_kind(graph.definitions().get(&id)?);
            Some(CCell::new(
                CCellTag::Node,
                CCellPayload {
                    node: CNode {
                        category: CNodeCategory::Definition,
                        kind: kind as u32,
                        id: *id,
                    },
                },
            ))
        }
        NodeRef::Document(id) => graph.documents().contains_key(&id).then(|| {
            CCell::new(
                CCellTag::Node,
                CCellPayload {
                    node: CNode {
                        category: CNodeCategory::Document,
                        kind: 0,
                        id: *id,
                    },
                },
            )
        }),
    }
}

/// Runs a previously parsed query and returns a row iterator (column names + typed rows),
/// so callers can build their own value/handle objects instead of a formatted string.
///
/// # Safety
///
/// - `query` must be a valid pointer returned by `rdx_cypher_parse`.
/// - `pointer` must be a valid `GraphPointer` previously returned by this crate.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn rdx_query_run_rows(query: *const c_void, pointer: GraphPointer) -> CRunRows {
    if query.is_null() {
        return CRunRows {
            iter: ptr::null_mut(),
            error: utils::cstring_raw("query is null"),
        };
    }

    let parsed = unsafe { &*query.cast::<cypher::Query>() };

    with_graph(pointer, |graph| {
        let result_set = match cypher::execute(graph, parsed) {
            Ok(result_set) => result_set,
            Err(error) => {
                return CRunRows {
                    iter: ptr::null_mut(),
                    error: utils::cstring_raw(&error.to_string()),
                };
            }
        };

        let columns: Box<[*const c_char]> = result_set.columns.iter().map(|name| utils::cstring_raw(name)).collect();

        let rows: Box<[CResultRow]> = result_set
            .rows
            .iter()
            .map(|row| {
                let cells: Vec<CCell> = row.iter().map(|cell| build_cell(graph, cell)).collect();
                let len = cells.len();
                let cells_ptr = if cells.is_empty() {
                    ptr::null_mut()
                } else {
                    Box::into_raw(cells.into_boxed_slice()).cast::<CCell>()
                };
                CResultRow { cells: cells_ptr, len }
            })
            .collect();

        CRunRows {
            iter: Box::into_raw(Box::new(CRowsIter {
                columns,
                rows,
                index: 0,
            })),
            error: ptr::null(),
        }
    })
}

/// Returns the number of columns in the result set.
///
/// # Safety
///
/// - `iter` must be a valid pointer returned by `rdx_query_run_rows`, or null.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn rdx_rows_iter_column_count(iter: *const CRowsIter) -> usize {
    if iter.is_null() {
        return 0;
    }
    let iter = unsafe { &*iter };
    iter.columns.len()
}

/// Returns a pointer to the array of column name C strings. The array has
/// `rdx_rows_iter_column_count(iter)` entries and is valid for the lifetime of the iterator.
///
/// # Safety
///
/// - `iter` must be a valid pointer returned by `rdx_query_run_rows`, or null.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn rdx_rows_iter_columns(iter: *const CRowsIter) -> *const *const c_char {
    if iter.is_null() {
        return ptr::null();
    }
    let iter = unsafe { &*iter };
    iter.columns.as_ptr()
}

/// Returns the number of rows in the result set.
///
/// # Safety
///
/// - `iter` must be a valid pointer returned by `rdx_query_run_rows`, or null.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn rdx_rows_iter_len(iter: *const CRowsIter) -> usize {
    if iter.is_null() {
        return 0;
    }
    let iter = unsafe { &*iter };
    iter.rows.len()
}

/// Advances the iterator and copies the next row into `out`. Returns `true` if a row was read,
/// `false` if the iterator is exhausted. The copied `CResultRow` is a view into the iterator's
/// owned cells — it remains valid until `rdx_rows_iter_free` is called.
///
/// # Safety
///
/// - `iter` must be a valid pointer returned by `rdx_query_run_rows`, or null.
/// - `out` must be a valid, writable pointer, or null.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn rdx_rows_iter_next(iter: *mut CRowsIter, out: *mut CResultRow) -> bool {
    if iter.is_null() || out.is_null() {
        return false;
    }

    let it = unsafe { &mut *iter };
    if it.index >= it.rows.len() {
        return false;
    }

    let row = it.rows[it.index];
    it.index += 1;
    unsafe {
        *out = row;
    }

    true
}

/// Recursively frees a `CCell`'s owned allocations (its string, or its nested list cells).
unsafe fn free_cell(cell: &CCell) {
    match cell.tag {
        // SAFETY: the tag selects the active union field.
        CCellTag::Str => {
            let str_val = unsafe { cell.payload.str_val };
            if !str_val.is_null() {
                let _ = unsafe { CString::from_raw(str_val.cast_mut()) };
            }
        }
        // SAFETY: the tag selects the active union field.
        CCellTag::List => {
            let list = unsafe { cell.payload.list };
            if !list.items.is_null() && list.len > 0 {
                let slice = unsafe { Box::from_raw(ptr::slice_from_raw_parts_mut(list.items, list.len)) };
                for nested in &slice {
                    unsafe { free_cell(nested) };
                }
            }
        }
        // SAFETY: the tag selects the active union field.
        CCellTag::Map => {
            let map = unsafe { cell.payload.map };
            if map.len > 0 {
                if !map.keys.is_null() {
                    let keys = unsafe { Box::from_raw(ptr::slice_from_raw_parts_mut(map.keys, map.len)) };
                    for key in &keys {
                        if !key.is_null() {
                            let _ = unsafe { CString::from_raw(key.cast_mut()) };
                        }
                    }
                }
                if !map.values.is_null() {
                    let values = unsafe { Box::from_raw(ptr::slice_from_raw_parts_mut(map.values, map.len)) };
                    for nested in &values {
                        unsafe { free_cell(nested) };
                    }
                }
            }
        }
        _ => {}
    }
}

/// Frees a `CRowsIter` previously returned by `rdx_query_run_rows`, including all column strings,
/// row cells, and nested allocations.
///
/// # Safety
///
/// - `iter` must be a pointer returned by `rdx_query_run_rows`, or null. It must not be used after.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn rdx_rows_iter_free(iter: *mut CRowsIter) {
    if iter.is_null() {
        return;
    }

    let it = unsafe { Box::from_raw(iter) };

    // Free column C strings (the boxed slice itself is freed when `it` drops).
    for &col in &it.columns {
        if !col.is_null() {
            let _ = unsafe { CString::from_raw(col.cast_mut()) };
        }
    }

    // Free cells in each row (the boxed slice of CResultRow is freed when `it` drops).
    for row in &it.rows {
        if !row.cells.is_null() && row.len > 0 {
            let cells = unsafe { Box::from_raw(ptr::slice_from_raw_parts_mut(row.cells, row.len)) };
            for cell in &cells {
                unsafe { free_cell(cell) };
            }
        }
    }
}
