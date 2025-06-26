use crate::location::Location;

#[derive(Debug)]
pub enum SymbolKind {
    Class,
    Module,
    Constant,
    Method,
}

#[derive(Debug)]
pub struct Symbol {
    pub kind: SymbolKind,
    pub name: String,
    pub location: Location,
}

#[derive(Debug)]
pub struct Class {
    pub base: Symbol,
    pub superclass: Option<String>,
    pub visibility: Option<String>,
}

#[derive(Debug)]
pub struct Module {
    pub base: Symbol,
    pub visibility: Option<String>,
}

#[derive(Debug)]
pub struct Constant {
    pub base: Symbol,
    pub visibility: Option<String>,
    pub value: Option<String>,
}

#[derive(Debug)]
pub struct Method {
    pub base: Symbol,
    pub visibility: Option<String>,
    pub arguments: Vec<String>,
}

impl Symbol {
    pub fn new(kind: SymbolKind, name: String, location: Location) -> Self {
        Self { kind, name, location }
    }
}

impl Class {
    pub fn new(name: String, location: Location, superclass: Option<String>, visibility: Option<String>) -> Self {
        Self {
            base: Symbol::new(SymbolKind::Class, name, location),
            superclass,
            visibility,
        }
    }
}

impl Module {
    pub fn new(name: String, location: Location, visibility: Option<String>) -> Self {
        Self {
            base: Symbol::new(SymbolKind::Module, name, location),
            visibility,
        }
    }
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

impl Method {
    pub fn new(name: String, location: Location, visibility: Option<String>, arguments: Vec<String>) -> Self {
        Self {
            base: Symbol::new(SymbolKind::Method, name, location),
            visibility,
            arguments,
        }
    }
}
