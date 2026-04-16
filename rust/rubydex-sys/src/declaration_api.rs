//! This file provides the C API for the Graph object

use libc::c_char;
use rubydex::model::declaration::{Ancestor, Declaration, Namespace};
use std::ffi::CString;
use std::ptr;

use crate::definition_api::{DefinitionsIter, rdx_definitions_iter_new_from_ids};
use crate::graph_api::{GraphPointer, with_graph};
use crate::reference_api::{CConstantReference, CMethodReference, ConstantReferencesIter, MethodReferencesIter};
use crate::utils;
use rubydex::model::ids::{DeclarationId, StringId};

#[repr(C)]
#[derive(Debug, Clone, Copy)]
pub enum CDeclarationKind {
    Class = 0,
    Module = 1,
    SingletonClass = 2,
    Constant = 3,
    ConstantAlias = 4,
    Method = 5,
    GlobalVariable = 6,
    InstanceVariable = 7,
    ClassVariable = 8,
    Todo = 9,
}

#[repr(C)]
#[derive(Debug, Clone, Copy)]
pub struct CDeclaration {
    id: u64,
    kind: CDeclarationKind,
}

impl CDeclaration {
    #[must_use]
    pub fn id(&self) -> u64 {
        self.id
    }

    #[must_use]
    pub fn from_declaration(id: DeclarationId, decl: &Declaration) -> Self {
        Self {
            id: *id,
            kind: Self::kind_from_declaration(decl),
        }
    }

    #[must_use]
    pub fn kind_from_declaration(decl: &Declaration) -> CDeclarationKind {
        match decl {
            Declaration::Namespace(Namespace::Class(_)) => CDeclarationKind::Class,
            Declaration::Namespace(Namespace::Module(_)) => CDeclarationKind::Module,
            Declaration::Namespace(Namespace::SingletonClass(_)) => CDeclarationKind::SingletonClass,
            Declaration::Namespace(Namespace::Todo(_)) => CDeclarationKind::Todo,
            Declaration::Constant(_) => CDeclarationKind::Constant,
            Declaration::ConstantAlias(_) => CDeclarationKind::ConstantAlias,
            Declaration::Method(_) => CDeclarationKind::Method,
            Declaration::GlobalVariable(_) => CDeclarationKind::GlobalVariable,
            Declaration::InstanceVariable(_) => CDeclarationKind::InstanceVariable,
            Declaration::ClassVariable(_) => CDeclarationKind::ClassVariable,
        }
    }
}

/// An iterator over declaration IDs
///
/// We snapshot the IDs at iterator creation so if the graph is modified, the iterator will not see the changes
#[derive(Debug)]
pub struct DeclarationsIter {
    /// The snapshot of declarations
    entries: Box<[CDeclaration]>,
    /// The current index of the iterator
    index: usize,
}

iterator!(DeclarationsIter, entries: CDeclaration);

/// # Safety
/// `iter` must be a valid pointer previously returned by `DeclarationsIter::new`.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn rdx_graph_declarations_iter_len(iter: *const DeclarationsIter) -> usize {
    unsafe { DeclarationsIter::len(iter) }
}

/// # Safety
/// - `iter` must be a valid pointer previously returned by `DeclarationsIter::new`.
/// - `out` must be a valid, writable pointer.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn rdx_graph_declarations_iter_next(iter: *mut DeclarationsIter, out: *mut CDeclaration) -> bool {
    unsafe { DeclarationsIter::next(iter, out) }
}

/// # Safety
/// - `iter` must be a pointer previously returned by `DeclarationsIter::new`.
/// - `iter` must not be used after being freed.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn rdx_graph_declarations_iter_free(iter: *mut DeclarationsIter) {
    unsafe { DeclarationsIter::free(iter) }
}

/// Returns the UTF-8 name string for a declaration id.
/// Caller must free with `free_c_string`.
///
/// # Safety
///
/// Assumes pointer is valid.
///
/// # Panics
///
/// This function will panic if the name pointer is invalid.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn rdx_declaration_name(pointer: GraphPointer, name_id: u64) -> *const c_char {
    with_graph(pointer, |graph| {
        let name_id = DeclarationId::new(name_id);
        if let Some(decl) = graph.declarations().get(&name_id) {
            CString::new(decl.name()).unwrap().into_raw().cast_const()
        } else {
            ptr::null()
        }
    })
}

/// Returns the declaration ID for a member from a declaration.
/// Returns NULL if the member is not found.
///
/// # Safety
/// - `member` must be a valid, null-terminated UTF-8 string
///
/// # Panics
///
/// Will panic if there's inconsistent graph data
#[unsafe(no_mangle)]
pub unsafe extern "C" fn rdx_declaration_member(
    pointer: GraphPointer,
    name_id: u64,
    member: *const c_char,
) -> *const CDeclaration {
    let Ok(member_str) = (unsafe { utils::convert_char_ptr_to_string(member) }) else {
        return ptr::null();
    };

    with_graph(pointer, |graph| {
        let name_id = DeclarationId::new(name_id);
        if let Some(Declaration::Namespace(decl)) = graph.declarations().get(&name_id) {
            let member_id = StringId::from(member_str.as_str());

            if let Some(member_decl_id) = decl.member(&member_id) {
                let member_decl = graph.declarations().get(member_decl_id).unwrap();
                return Box::into_raw(Box::new(CDeclaration::from_declaration(*member_decl_id, member_decl)))
                    .cast_const();
            }
        }

        ptr::null()
    })
}

/// Searches for a member in the ancestors of the given declaration
///
/// # Safety
/// - `member` must be a valid, null-terminated UTF-8 string
///
/// # Panics
///
/// Will panic if there's inconsistent graph data
#[unsafe(no_mangle)]
pub unsafe extern "C" fn rdx_declaration_find_member(
    pointer: GraphPointer,
    declaration_id: u64,
    member: *const c_char,
    only_inherited: bool,
) -> *const CDeclaration {
    let Ok(member_str) = (unsafe { utils::convert_char_ptr_to_string(member) }) else {
        return ptr::null();
    };

    with_graph(pointer, |graph| {
        let id = DeclarationId::new(declaration_id);

        let Some(Declaration::Namespace(decl)) = graph.declarations().get(&id) else {
            return ptr::null();
        };

        let member_id = StringId::from(member_str.as_str());
        let mut found_main_namespace = false;

        decl.ancestors()
            .iter()
            .find_map(|ancestor| match ancestor {
                Ancestor::Complete(ancestor_id) => {
                    // When only_inherited, skip self and prepended modules
                    if only_inherited {
                        let is_self = *ancestor_id == id;
                        if is_self {
                            found_main_namespace = true;
                        }
                        if is_self || !found_main_namespace {
                            return None;
                        }
                    }

                    let ancestor_decl = graph.declarations().get(ancestor_id).unwrap().as_namespace().unwrap();

                    if let Some(member_decl_id) = ancestor_decl.member(&member_id) {
                        return Some((member_decl_id, graph.declarations().get(member_decl_id).unwrap()));
                    }

                    None
                }
                Ancestor::Partial(_) => None,
            })
            .map_or(ptr::null(), |(member_decl_id, member_decl)| {
                Box::into_raw(Box::new(CDeclaration::from_declaration(*member_decl_id, member_decl))).cast_const()
            })
    })
}

/// Returns the UTF-8 unqualified name string for a declaration id.
/// Caller must free with `free_c_string`.
///
/// # Safety
///
/// Assumes pointer is valid.
///
/// # Panics
///
/// This function will panic if the name pointer is invalid.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn rdx_declaration_unqualified_name(pointer: GraphPointer, name_id: u64) -> *const c_char {
    with_graph(pointer, |graph| {
        let name_id = DeclarationId::new(name_id);
        if let Some(decl) = graph.declarations().get(&name_id) {
            CString::new(decl.unqualified_name()).unwrap().into_raw().cast_const()
        } else {
            ptr::null()
        }
    })
}

/// An iterator over definition IDs and kinds for a given declaration
///
/// We snapshot the IDs at iterator creation so if the graph is modified, the iterator will not see the changes
// Use shared DefinitionsIter directly in signatures
/// Creates a new iterator over definition IDs for a given declaration by snapshotting the current set of IDs.
///
/// # Safety
///
/// - `pointer` must be a valid `GraphPointer` previously returned by this crate.
/// - The returned pointer must be freed with `rdx_declaration_definitions_iter_free`.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn rdx_declaration_definitions_iter_new(
    pointer: GraphPointer,
    decl_id: u64,
) -> *mut DefinitionsIter {
    // Snapshot the IDs and kinds at iterator creation to avoid borrowing across FFI calls
    with_graph(pointer, |graph| {
        let decl_id = DeclarationId::new(decl_id);
        if let Some(decl) = graph.declarations().get(&decl_id) {
            rdx_definitions_iter_new_from_ids(graph, decl.definitions())
        } else {
            DefinitionsIter::new(Vec::<_>::new().into_boxed_slice())
        }
    })
}

/// Returns the declaration for the singleton class of the declaration
///
/// # Safety
///
/// Assumes pointer is valid
///
/// # Panics
///
/// Will panic if invoked on a non-existing or non-namespace declaration
#[unsafe(no_mangle)]
pub unsafe extern "C" fn rdx_declaration_singleton_class(pointer: GraphPointer, decl_id: u64) -> *const CDeclaration {
    with_graph(pointer, |graph| {
        let declaration = graph
            .declarations()
            .get(&DeclarationId::new(decl_id))
            .unwrap()
            .as_namespace()
            .unwrap();

        if let Some(singleton_id) = declaration.singleton_class() {
            Box::into_raw(Box::new(CDeclaration::from_declaration(
                *singleton_id,
                graph.declarations().get(singleton_id).unwrap(),
            )))
        } else {
            ptr::null()
        }
    })
}

/// Returns the owner of the declaration (attached object in the case of singleton classes)
///
/// # Safety
///
/// Assumes pointer is valid
///
/// # Panics
///
/// Will panic if invoked on a non-existing or non-namespace declaration
#[unsafe(no_mangle)]
pub unsafe extern "C" fn rdx_declaration_owner(pointer: GraphPointer, decl_id: u64) -> *const CDeclaration {
    with_graph(pointer, |graph| {
        let declaration = graph.declarations().get(&DeclarationId::new(decl_id)).unwrap();
        let owner_id = *declaration.owner_id();
        Box::into_raw(Box::new(CDeclaration::from_declaration(
            owner_id,
            graph.declarations().get(&owner_id).unwrap(),
        )))
        .cast_const()
    })
}

/// Frees a `CDeclaration` allocated on the Rust side
///
/// # Safety
///
/// - `ptr` must be a valid pointer previously returned by a function returning `*const CDeclaration`
/// - `ptr` must not be used after being freed
#[unsafe(no_mangle)]
pub unsafe extern "C" fn free_c_declaration(ptr: *const CDeclaration) {
    if ptr.is_null() {
        return;
    }

    unsafe {
        let _ = Box::from_raw(ptr.cast_mut());
    }
}

/// Returns an iterator over the ancestor declarations of a given declaration
///
/// # Safety
///
/// Assumes that the graph and member pointers are valid
///
/// # Panics
///
/// Will panic if there's inconsistent graph data
#[unsafe(no_mangle)]
pub unsafe extern "C" fn rdx_declaration_ancestors(pointer: GraphPointer, decl_id: u64) -> *mut DeclarationsIter {
    let declarations = with_graph(pointer, |graph| {
        let declaration_id = DeclarationId::new(decl_id);

        let Some(Declaration::Namespace(declaration)) = graph.declarations().get(&declaration_id) else {
            return Vec::new();
        };

        declaration
            .ancestors()
            .into_iter()
            .filter_map(|ancestor| match ancestor {
                Ancestor::Complete(id) => Some(CDeclaration::from_declaration(
                    *id,
                    graph.declarations().get(id).unwrap(),
                )),
                Ancestor::Partial(_) => None,
            })
            .collect::<Vec<_>>()
    });

    DeclarationsIter::new(declarations.into_boxed_slice())
}

/// Returns an iterator over the descendant declarations of a given declaration
///
/// # Safety
///
/// Assumes that the graph and member pointers are valid
///
/// # Panics
///
/// Will panic if there's inconsistent graph data
#[unsafe(no_mangle)]
pub unsafe extern "C" fn rdx_declaration_descendants(pointer: GraphPointer, decl_id: u64) -> *mut DeclarationsIter {
    let declarations = with_graph(pointer, |graph| {
        let declaration_id = DeclarationId::new(decl_id);

        if !matches!(
            graph.declarations().get(&declaration_id),
            Some(Declaration::Namespace(_))
        ) {
            return Vec::new();
        }

        graph
            .transitive_descendants(declaration_id)
            .map(|id| CDeclaration::from_declaration(id, graph.declarations().get(&id).unwrap()))
            .collect::<Vec<_>>()
    });

    DeclarationsIter::new(declarations.into_boxed_slice())
}

/// Returns an iterator over the member declarations of a given namespace declaration
///
/// # Safety
///
/// Assumes that the graph pointer is valid
///
/// # Panics
///
/// Will panic if there's inconsistent graph data
#[unsafe(no_mangle)]
pub unsafe extern "C" fn rdx_declaration_members(pointer: GraphPointer, decl_id: u64) -> *mut DeclarationsIter {
    let declarations = with_graph(pointer, |graph| {
        let declaration_id = DeclarationId::new(decl_id);

        let Some(Declaration::Namespace(declaration)) = graph.declarations().get(&declaration_id) else {
            return Vec::new();
        };

        declaration
            .members()
            .values()
            .map(|id| CDeclaration::from_declaration(*id, graph.declarations().get(id).unwrap()))
            .collect::<Vec<_>>()
    });

    DeclarationsIter::new(declarations.into_boxed_slice())
}

/// Creates a new iterator over constant references for a given declaration.
///
/// # Safety
///
/// - `pointer` must be a valid `GraphPointer` previously returned by this crate.
/// - The returned pointer must be freed with `rdx_constant_references_iter_free`.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn rdx_declaration_constant_references_iter_new(
    pointer: GraphPointer,
    declaration_id: u64,
) -> *mut ConstantReferencesIter {
    with_graph(pointer, |graph| {
        let decl_id_typed = DeclarationId::new(declaration_id);

        let Some(decl) = graph.declarations().get(&decl_id_typed) else {
            return ptr::null_mut();
        };
        let Some(constant_references) = decl.constant_references() else {
            return ptr::null_mut();
        };

        let entries: Vec<_> = constant_references
            .iter()
            .map(|ref_id| CConstantReference {
                id: **ref_id,
                declaration_id,
            })
            .collect();

        ConstantReferencesIter::new(entries.into_boxed_slice())
    })
}

/// Creates a new iterator over method references for a given declaration.
///
/// # Safety
///
/// - `pointer` must be a valid `GraphPointer` previously returned by this crate.
/// - The returned pointer must be freed with `rdx_method_references_iter_free`.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn rdx_declaration_method_references_iter_new(
    pointer: GraphPointer,
    decl_id: u64,
) -> *mut MethodReferencesIter {
    with_graph(pointer, |graph| {
        let decl_id = DeclarationId::new(decl_id);
        let Some(Declaration::Method(decl)) = graph.declarations().get(&decl_id) else {
            return ptr::null_mut();
        };

        let entries: Vec<_> = decl
            .references()
            .iter()
            .map(|ref_id| CMethodReference { id: **ref_id })
            .collect();

        MethodReferencesIter::new(entries.into_boxed_slice())
    })
}
