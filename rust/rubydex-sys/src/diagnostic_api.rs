//! Diagnostics-related FFI helpers

use crate::graph_api::{GraphPointer, with_graph};
use crate::location_api::{Location, create_location_for_uri_and_offset};
use libc::c_char;
use rubydex::diagnostic::{Rule, Severity};
use std::{ffi::CString, mem, ptr};

/// C-compatible enum representing diagnostic severity levels.
#[repr(C)]
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum DiagnosticSeverity {
    Error = 1,
    Warning = 2,
    Information = 3,
    Hint = 4,
}

impl From<Severity> for DiagnosticSeverity {
    fn from(severity: Severity) -> Self {
        match severity {
            Severity::Error => DiagnosticSeverity::Error,
            Severity::Warning => DiagnosticSeverity::Warning,
            Severity::Information => DiagnosticSeverity::Information,
            Severity::Hint => DiagnosticSeverity::Hint,
        }
    }
}

#[repr(C)]
#[derive(Debug)]
pub struct CRule {
    pub name: *const c_char,
    pub name_length: usize,
    pub default_severity: DiagnosticSeverity,
}

impl From<Rule> for CRule {
    fn from(rule: Rule) -> Self {
        let name = rule.name();

        Self {
            name: name.as_ptr().cast::<c_char>(),
            name_length: name.len(),
            default_severity: DiagnosticSeverity::from(rule.default_severity()),
        }
    }
}

#[repr(C)]
pub struct CRuleArray {
    pub items: *mut CRule,
    pub len: usize,
}

/// Returns every rule the graph can report. Caller must free it with `rdx_rules_free`.
#[unsafe(no_mangle)]
pub extern "C" fn rdx_rules() -> CRuleArray {
    let items = Rule::all().iter().copied().map(CRule::from).collect::<Box<[CRule]>>();

    CRuleArray {
        len: items.len(),
        items: Box::into_raw(items).cast::<CRule>(),
    }
}

/// Frees an array previously returned by `rdx_rules`.
///
/// # Safety
///
/// - `rules` must have been returned by `rdx_rules` and must not be used afterwards.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn rdx_rules_free(rules: CRuleArray) {
    if rules.items.is_null() {
        return;
    }

    unsafe {
        let _ = Box::from_raw(ptr::slice_from_raw_parts_mut(rules.items, rules.len));
    }
}

/// C-compatible struct representing a diagnostic entry.
#[repr(C)]
pub struct DiagnosticEntry {
    pub rule: *const c_char,
    pub message: *const c_char,
    pub location: *mut Location,
    pub severity: DiagnosticSeverity,
}

/// C-compatible array wrapper for diagnostics.
#[repr(C)]
pub struct DiagnosticArray {
    pub items: *mut DiagnosticEntry,
    pub len: usize,
}

impl DiagnosticArray {
    fn from_vec(mut entries: Vec<DiagnosticEntry>) -> *mut DiagnosticArray {
        let len = entries.len();
        let ptr = entries.as_mut_ptr();
        mem::forget(entries);
        Box::into_raw(Box::new(DiagnosticArray { items: ptr, len }))
    }
}

/// Returns all diagnostics currently recorded in the global graph.
///
/// # Safety
///
/// - `pointer` must be a valid `GraphPointer` previously returned by this crate.
/// - The pointed graph must remain alive for the duration of the call.
///
/// # Panics
///
/// - If a diagnostic references a URI whose file cannot be read to build a location.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn rdx_graph_diagnostics(pointer: GraphPointer) -> *mut DiagnosticArray {
    with_graph(pointer, |graph| {
        let entries = graph
            .all_diagnostics()
            .iter()
            .map(|diagnostic| {
                let document = graph.documents().get(diagnostic.uri_id()).unwrap();
                let location = create_location_for_uri_and_offset(graph, document, diagnostic.offset());

                DiagnosticEntry {
                    rule: CString::new(diagnostic.rule().to_string())
                        .unwrap()
                        .into_raw()
                        .cast_const(),
                    message: CString::new(diagnostic.message()).unwrap().into_raw().cast_const(),
                    location,
                    severity: DiagnosticSeverity::from(diagnostic.rule().default_severity()),
                }
            })
            .collect::<Vec<DiagnosticEntry>>();

        DiagnosticArray::from_vec(entries)
    })
}

/// Frees a diagnostic array previously returned by `rdx_graph_diagnostics`.
///
/// # Safety
///
/// - `ptr` must be a valid pointer previously returned by `rdx_graph_diagnostics`.
/// - `ptr` must not be used after being freed.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn rdx_diagnostics_free(ptr: *mut DiagnosticArray) {
    if ptr.is_null() {
        return;
    }

    let array = unsafe { Box::from_raw(ptr) };
    if !array.items.is_null() && array.len > 0 {
        let slice_ptr = ptr::slice_from_raw_parts_mut(array.items, array.len);
        let mut boxed_slice: Box<[DiagnosticEntry]> = unsafe { Box::from_raw(slice_ptr) };

        for entry in &mut *boxed_slice {
            if !entry.rule.is_null() {
                let _ = unsafe { CString::from_raw(entry.rule.cast_mut()) };
            }
            if !entry.message.is_null() {
                let _ = unsafe { CString::from_raw(entry.message.cast_mut()) };
            }
            if !entry.location.is_null() {
                unsafe { crate::location_api::rdx_location_free(entry.location) };
                entry.location = ptr::null_mut();
            }
        }
        // boxed_slice drops here, releasing the buffer
    }
    // array drops here
}
