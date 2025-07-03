use crate::location::Location;
use crate::tables::global_tables::{NameId, intern_name, with_name};

#[derive(Debug)]
pub enum SymbolKind {
    Class,
    Module,
    Constant,
    Method,
}

impl SymbolKind {
    pub fn to_string(&self) -> &str {
        match self {
            SymbolKind::Class => "class",
            SymbolKind::Module => "module",
            SymbolKind::Constant => "constant",
            SymbolKind::Method => "method",
        }
    }
}

#[derive(Debug)]
pub struct Symbol {
    pub kind: SymbolKind,
    pub name_id: NameId, // Memory-efficient name reference using global table
    pub location: Location,
}

impl Symbol {
    pub fn new(kind: SymbolKind, name: String, location: Location) -> Self {
        let name_id = intern_name(&name);
        Self { kind, name_id, location }
    }

    pub fn new_with_id(kind: SymbolKind, name_id: NameId, location: Location) -> Self {
        Self { kind, name_id, location }
    }

    /// Get the symbol name as a string
    pub fn name(&self) -> Option<String> {
        with_name(self.name_id, |s| s.to_string())
    }

    /// Execute a function with the symbol name
    pub fn with_name<R>(&self, f: impl FnOnce(&str) -> R) -> Option<R> {
        with_name(self.name_id, f)
    }
}

impl std::fmt::Display for Symbol {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let name = self.name().unwrap_or_else(|| "<unknown>".to_string());
        write!(f, "{} {} ({})", self.kind.to_string(), name, self.location)
    }
}

#[derive(Debug)]
pub struct Class {
    pub base: Symbol,
    pub superclass: Option<NameId>, // Also intern superclass names
    pub visibility: Option<String>,
}

impl Class {
    pub fn new(name: String, location: Location, superclass: Option<String>, visibility: Option<String>) -> Self {
        let superclass = superclass.map(|s| intern_name(&s));
        Self {
            base: Symbol::new(SymbolKind::Class, name, location),
            superclass,
            visibility,
        }
    }

    pub fn new_with_ids(name_id: NameId, location: Location, superclass: Option<NameId>, visibility: Option<String>) -> Self {
        Self {
            base: Symbol::new_with_id(SymbolKind::Class, name_id, location),
            superclass,
            visibility,
        }
    }

    /// Get the superclass name as a string
    pub fn superclass_name(&self) -> Option<String> {
        self.superclass.and_then(|id| with_name(id, |s| s.to_string()))
    }
}

impl std::fmt::Display for Class {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let superclass = self.superclass_name().unwrap_or_else(|| String::new());
        write!(f, "{} < {}", self.base, superclass)
    }
}

#[derive(Debug)]
pub struct Module {
    pub base: Symbol,
    pub visibility: Option<String>,
}

impl Module {
    pub fn new(name: String, location: Location, visibility: Option<String>) -> Self {
        Self {
            base: Symbol::new(SymbolKind::Module, name, location),
            visibility,
        }
    }
}

impl std::fmt::Display for Module {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.base)
    }
}

#[derive(Debug)]
pub struct Constant {
    pub base: Symbol,
    pub visibility: Option<String>,
    pub value: Option<String>,
}

impl Constant {
    pub fn new(name: String, location: Location, visibility: Option<String>, value: Option<String>) -> Self {
        Self {
            base: Symbol::new(SymbolKind::Constant, name, location),
            visibility,
            value,
        }
    }
}

impl std::fmt::Display for Constant {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.base)
    }
}

#[derive(Debug)]
pub struct Method {
    pub base: Symbol,
    pub visibility: Option<String>,
    pub parameters: Vec<String>,
}

impl Method {
    pub fn new(name: String, location: Location, visibility: Option<String>, parameters: Vec<String>) -> Self {
        Self {
            base: Symbol::new(SymbolKind::Method, name, location),
            visibility,
            parameters,
        }
    }
}

impl std::fmt::Display for Method {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} ({})", self.base, self.parameters.join(", "))
    }
}
