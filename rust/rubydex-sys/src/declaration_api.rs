//! This file provides the C API for the Graph object

use libc::c_char;
use rubydex::model::declaration::{Ancestor, Declaration, Namespace};
use std::ffi::CString;
use std::ptr;

use crate::definition_api::{DefinitionsIter, rdx_definitions_iter_new_from_ids};
use crate::graph_api::{GraphPointer, with_graph};
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
    declarations: Box<[CDeclaration]>,
    /// The current index of the iterator
    index: usize,
}

impl DeclarationsIter {
    #[must_use]
    pub fn new(declarations: Box<[CDeclaration]>) -> Self {
        Self { declarations, index: 0 }
    }

    #[must_use]
    pub fn len(&self) -> usize {
        self.declarations.len()
    }

    #[must_use]
    pub fn is_empty(&self) -> bool {
        self.declarations.is_empty()
    }

    /// # Safety
    ///
    /// Expects `out_id` to be a valid pointer
    pub unsafe fn next(&mut self, out_decl: *mut CDeclaration) -> bool {
        if self.index >= self.declarations.len() {
            return false;
        }

        let next_decl = self.declarations[self.index];
        self.index += 1;
        unsafe { *out_decl = next_decl };

        true
    }
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

    Box::into_raw(Box::new(DeclarationsIter::new(declarations.into_boxed_slice())))
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

        let Some(Declaration::Namespace(declaration)) = graph.declarations().get(&declaration_id) else {
            return Vec::new();
        };

        declaration
            .descendants()
            .iter()
            .map(|id| CDeclaration::from_declaration(*id, graph.declarations().get(id).unwrap()))
            .collect::<Vec<_>>()
    });

    Box::into_raw(Box::new(DeclarationsIter::new(declarations.into_boxed_slice())))
}
