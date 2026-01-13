use line_index::WideEncoding;

#[derive(Default, Debug)]
pub enum Encoding {
    #[default]
    Utf8,
    Utf16,
    Utf32,
}

impl Encoding {
    /// Transform the LSP selected encoding into the expected `WideEncoding` for converting code units with the
    /// `line_index` crate
    #[must_use]
    pub fn to_wide(&self) -> Option<WideEncoding> {
        match self {
            Encoding::Utf8 => None,
            Encoding::Utf16 => Some(WideEncoding::Utf16),
            Encoding::Utf32 => Some(WideEncoding::Utf32),
        }
    }
}
