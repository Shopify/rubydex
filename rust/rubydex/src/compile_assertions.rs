/// Asserts the memory size of a struct at compile time in bytes
#[macro_export]
macro_rules! assert_mem_size {
    ($struct:ident, $size:expr) => {
        // Check struct name on this assert! error
        const _: () = assert!(
            std::mem::size_of::<$struct>() == $size,
            concat!("Incorrect size for `", stringify!($struct), "`")
        );
        // Check actual size on this array error
        const _: [(); $size] = [(); std::mem::size_of::<$struct>()];
    };
}

/// Asserts at compile time that a type is `Send + Sync`.
///
/// The `Graph` is shared across Ractors/threads behind a `RwLock`, so it must stay
/// `Send + Sync`. This assertion fails the build if a field that breaks either trait
/// is ever added.
#[macro_export]
macro_rules! assert_send_sync {
    ($struct:ident) => {
        const _: fn() = || {
            fn assert_send_sync<T: ?Sized + Send + Sync>() {}
            assert_send_sync::<$struct>();
        };
    };
}
