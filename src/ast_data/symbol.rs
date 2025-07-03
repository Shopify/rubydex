use crate::location::Location;
use crate::tables::global_tables::{NameId, intern_name, with_name};

#[derive(Debug, PartialEq)]
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
pub enum SymbolData {
    Class(ClassData),
    Module(ModuleData),
    Constant(ConstantData),
    Method(MethodData),
}

impl std::fmt::Display for SymbolData {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            SymbolData::Class(data) => write!(f, "{}", data),
            SymbolData::Module(data) => write!(f, "{}", data),
            SymbolData::Constant(data) => write!(f, "{}", data),
            SymbolData::Method(data) => write!(f, "{}", data),
        }
    }
}

#[derive(Debug)]
pub struct Symbol {
    pub kind: SymbolKind,
    pub name_id: NameId, // Memory-efficient name reference using global table
    pub location: Location,
    pub data: Option<SymbolData>,
}

impl Symbol {
    pub fn new(kind: SymbolKind, name: String, location: Location, data: Option<SymbolData>) -> Self {
        let name_id = intern_name(&name);
        Self { kind, name_id, location, data }
    }

    pub fn new_with_id(kind: SymbolKind, name_id: NameId, location: Location, data: Option<SymbolData>) -> Self {
        Self { kind, name_id, location, data }
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
        let data_str = self.data.as_ref().map(|data| data.to_string()).unwrap_or(String::new());
        write!(f, "{} {} ({}) {}", self.kind.to_string(), name, self.location, data_str)
    }
}

#[derive(Debug)]
pub struct ClassData {
    pub superclass: Option<NameId>, // Also intern superclass names
    pub visibility: Option<String>,
}

impl ClassData {
    pub fn new(superclass: Option<String>, visibility: Option<String>) -> Self {
        let superclass = superclass.map(|s| intern_name(&s));
        Self { superclass, visibility }
    }

    pub fn new_with_id(superclass: Option<NameId>, visibility: Option<String>) -> Self {
        Self { superclass, visibility }
    }

    /// Get the superclass name as a string
    pub fn superclass_name(&self) -> Option<String> {
        self.superclass.and_then(|id| with_name(id, |s| s.to_string()))
    }
}

impl std::fmt::Display for ClassData {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let superclass = self.superclass_name().unwrap_or_else(|| String::new());
        write!(f, "< {}", superclass)
    }
}

#[derive(Debug)]
pub struct ModuleData {
    pub visibility: Option<String>,
}

impl std::fmt::Display for ModuleData {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "")
    }
}

#[derive(Debug)]
pub struct ConstantData {
    pub visibility: Option<String>,
    pub value: Option<String>,
}

impl std::fmt::Display for ConstantData {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.value.as_ref().unwrap_or(&String::new()))
    }
}

#[derive(Debug)]
pub struct MethodData {
    pub visibility: Option<String>,
    pub parameters: Vec<String>,
}

impl MethodData {
    pub fn new(visibility: Option<String>, parameters: Vec<String>) -> Self {
        Self { visibility, parameters }
    }

    /// Get all parameter names as strings
    pub fn parameter_names(&self) -> Vec<String> {
        self.parameters.iter()
            .map(|s| s.to_string())
            .collect()
    }
}

impl std::fmt::Display for MethodData {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let params = self.parameter_names().join(", ");
        write!(f, "{}", params)
    }
}
