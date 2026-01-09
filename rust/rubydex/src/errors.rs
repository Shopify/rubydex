macro_rules! errors {
    (
        $( $variant:ident );* $(;)?
    ) => {
        #[derive(Debug, PartialEq, Eq)]
        pub enum Errors {
            $(
                $variant(String),
            )*
        }

        impl std::fmt::Display for Errors {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                match self {
                    $(
                        Errors::$variant(msg) => write!(f, "{}: {}", stringify!($variant), msg),
                    )*
                }
            }
        }

        impl std::error::Error for Errors {}
    }
}

errors!(
    FileError;
);
