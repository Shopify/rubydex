use crate::location::Location;

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
    pub name: String,
    pub location: Location,
}

impl Symbol {
    pub fn new(kind: SymbolKind, name: String, location: Location) -> Self {
        Self { kind, name, location }
    }
}

impl std::fmt::Display for Symbol {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} {} ({})", self.kind.to_string(), self.name, self.location)
    }
}

#[derive(Debug)]
pub struct Class {
    pub base: Symbol,
    pub superclass: Option<String>,
    pub visibility: Option<String>,
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

impl std::fmt::Display for Class {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} < {}", self.base, self.superclass.as_ref().unwrap_or(&String::new()))
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
