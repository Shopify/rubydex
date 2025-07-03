use crate::location::Location;
use crate::tables::global_tables::{NameId, intern_name, with_name};

#[derive(Debug)]
pub enum Symbol {
    Class(Class),
    Module(Module),
    Constant(Constant),
    Method(Method),
}

impl std::fmt::Display for Symbol {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Symbol::Class(class) => write!(f, "{}", class),
            Symbol::Module(module) => write!(f, "{}", module),
            Symbol::Constant(constant) => write!(f, "{}", constant),
            Symbol::Method(method) => write!(f, "{}", method),
        }
    }
}

#[derive(Debug)]
pub struct Class {
    pub name_id: NameId, // Memory-efficient name reference using global table
    pub location: Location,
    pub superclass: Option<NameId>, // Also intern superclass names
    pub visibility: Option<String>, // Could be interned too, but less frequent
}

impl Class {
    pub fn new(name: String, location: Location, superclass: Option<String>, visibility: Option<String>) -> Self {
        let name_id = intern_name(&name);
        let superclass = superclass.map(|s| intern_name(&s));
        Self { name_id, location, superclass, visibility }
    }

    pub fn new_with_ids(name_id: NameId, location: Location, superclass: Option<NameId>, visibility: Option<String>) -> Self {
        Self { name_id, location, superclass, visibility }
    }

    /// Get the class name as a string
    pub fn name(&self) -> Option<String> {
        with_name(self.name_id, |s| s.to_string())
    }

    /// Get the superclass name as a string
    pub fn superclass_name(&self) -> Option<String> {
        self.superclass.and_then(|id| with_name(id, |s| s.to_string()))
    }

    /// Execute a function with the class name
    pub fn with_name<R>(&self, f: impl FnOnce(&str) -> R) -> Option<R> {
        with_name(self.name_id, f)
    }
}

impl std::fmt::Display for Class {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let name = self.name().unwrap_or_else(|| "<unknown>".to_string());
        let superclass = self.superclass_name().unwrap_or_else(|| String::new());
        write!(f, "{} < {}", name, superclass)
    }
}

#[derive(Debug)]
pub struct Module {
    pub name_id: NameId, // Memory-efficient name reference using global table
    pub location: Location,
    pub visibility: Option<String>,
}

impl Module {
    pub fn new(name: String, location: Location, visibility: Option<String>) -> Self {
        let name_id = intern_name(&name);
        Self { name_id, location, visibility }
    }

    pub fn new_with_id(name_id: NameId, location: Location, visibility: Option<String>) -> Self {
        Self { name_id, location, visibility }
    }

    /// Get the module name as a string
    pub fn name(&self) -> Option<String> {
        with_name(self.name_id, |s| s.to_string())
    }

    /// Execute a function with the module name
    pub fn with_name<R>(&self, f: impl FnOnce(&str) -> R) -> Option<R> {
        with_name(self.name_id, f)
    }
}

impl std::fmt::Display for Module {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let name = self.name().unwrap_or_else(|| "<unknown>".to_string());
        write!(f, "{}", name)
    }
}

#[derive(Debug)]
pub struct Constant {
    pub name_id: NameId, // Memory-efficient name reference using global table
    pub location: Location,
    pub visibility: Option<String>,
    pub value: Option<String>, // Could also be interned for common values
}

impl Constant {
    pub fn new(name: String, location: Location, visibility: Option<String>, value: Option<String>) -> Self {
        let name_id = intern_name(&name);
        Self { name_id, location, visibility, value }
    }

    pub fn new_with_id(name_id: NameId, location: Location, visibility: Option<String>, value: Option<String>) -> Self {
        Self { name_id, location, visibility, value }
    }

    /// Get the constant name as a string
    pub fn name(&self) -> Option<String> {
        with_name(self.name_id, |s| s.to_string())
    }

    /// Execute a function with the constant name
    pub fn with_name<R>(&self, f: impl FnOnce(&str) -> R) -> Option<R> {
        with_name(self.name_id, f)
    }
}

impl std::fmt::Display for Constant {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let name = self.name().unwrap_or_else(|| "<unknown>".to_string());
        write!(f, "{}", name)
    }
}

#[derive(Debug)]
pub struct Method {
    pub name_id: NameId, // Memory-efficient name reference using global table
    pub location: Location,
    pub visibility: Option<String>,
    pub parameters: Vec<NameId>, // Also intern parameter names for memory efficiency
}

impl Method {
    pub fn new(name: String, location: Location, visibility: Option<String>, parameters: Vec<String>) -> Self {
        let name_id = intern_name(&name);
        let parameters = parameters.into_iter().map(|p| intern_name(&p)).collect();
        Self { name_id, location, visibility, parameters }
    }

    pub fn new_with_ids(name_id: NameId, location: Location, visibility: Option<String>, parameters: Vec<NameId>) -> Self {
        Self { name_id, location, visibility, parameters }
    }

    /// Get the method name as a string
    pub fn name(&self) -> Option<String> {
        with_name(self.name_id, |s| s.to_string())
    }

    /// Get all parameter names as strings
    pub fn parameter_names(&self) -> Vec<String> {
        self.parameters.iter()
            .filter_map(|&id| with_name(id, |s| s.to_string()))
            .collect()
    }

    /// Execute a function with the method name
    pub fn with_name<R>(&self, f: impl FnOnce(&str) -> R) -> Option<R> {
        with_name(self.name_id, f)
    }
}

impl std::fmt::Display for Method {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let name = self.name().unwrap_or_else(|| "<unknown>".to_string());
        let params = self.parameter_names().join(", ");
        write!(f, "{} ({})", name, params)
    }
}