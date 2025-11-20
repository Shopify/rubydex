use line_index::LineCol;

/// A position composed of line and columns. Note that all values are zero indexed and columns are based on the LSP
/// specification, meaning that they are always based in code units and can be retrieved for the 3 supported encodings
/// (Utf8, Utf16, Utf32)
pub type Position = LineCol;
