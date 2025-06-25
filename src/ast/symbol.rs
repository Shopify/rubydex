use super::location::Location;

#[derive(Debug, Clone, PartialEq)]
pub enum SymbolKind {
    Class,
    Module,
    Constant,
    Method,
}

#[derive(Debug, Clone)]
pub struct Symbol {
    pub kind: SymbolKind,
    pub name: String,
    pub location: Location,
}

impl Symbol {
    pub fn new(kind: SymbolKind, name: String, location: Location) -> Self {
        Self { kind, name, location }
    }
}
