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
