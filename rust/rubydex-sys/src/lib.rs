/// Helper macro to generate all required functions for an iterator. We use iterators to go over any collection of data
/// that exists on the Rust side (e.g.: definitions, documents, declarations, references). The goal is to avoid eager
/// allocation of large collections when possible.
///
/// Note: structs must be defined manually so that the cbindgen can see them. The actual methods are not extern "C" and
/// so they can be expanded fromVthe macro.
///
/// # Example
///
/// ```ignore
/// pub struct FoosIter {
///     entries: Box<[Foo]>,
///     index: usize,
/// }
///
/// iterator!(FoosIter, entries: Foo);
/// ```
macro_rules! iterator {
    ($name:ident, $field:ident : $entry:ty) => {
        impl $name {
            #[must_use]
            pub fn new($field: Box<[$entry]>) -> *mut $name {
                Box::into_raw(Box::new($name { $field, index: 0 }))
            }

            /// # Safety
            /// `iter` must be a valid pointer returned by `new`, or null.
            pub unsafe fn len(iter: *const Self) -> usize {
                if iter.is_null() {
                    return 0;
                }
                unsafe { (&*iter).$field.len() }
            }

            /// # Safety
            /// - `iter` must be a valid pointer returned by `new`, or null.
            /// - `out` must be a valid, writable pointer, or null.
            pub unsafe fn next(iter: *mut Self, out: *mut $entry) -> bool {
                if iter.is_null() || out.is_null() {
                    return false;
                }

                let it = unsafe { &mut *iter };
                if it.index >= it.$field.len() {
                    return false;
                }

                let entry = it.$field[it.index];
                it.index += 1;
                unsafe {
                    *out = entry;
                }

                true
            }

            /// # Safety
            /// `iter` must be a pointer returned by `new` (or null). Must not be used after.
            pub unsafe fn free(iter: *mut Self) {
                if iter.is_null() {
                    return;
                }
                unsafe {
                    let _ = Box::from_raw(iter);
                }
            }
        }
    };
}

pub mod declaration_api;
pub mod definition_api;
pub mod diagnostic_api;
pub mod document_api;
pub mod graph_api;
pub mod location_api;
pub mod name_api;
pub mod reference_api;
pub mod utils;
