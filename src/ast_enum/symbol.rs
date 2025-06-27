use crate::location::Location;

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
    pub name: String,
    pub location: Location,
    pub superclass: Option<String>,
    pub visibility: Option<String>,
}

impl Class {
    pub fn new(name: String, location: Location, superclass: Option<String>, visibility: Option<String>) -> Self {
        Self { name, location, superclass, visibility }
    }
}

impl std::fmt::Display for Class {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} < {}", self.name, self.superclass.as_ref().unwrap_or(&String::new()))
    }
}

#[derive(Debug)]
pub struct Module {
    pub name: String,
    pub location: Location,
    pub visibility: Option<String>,
}

impl Module {
    pub fn new(name: String, location: Location, visibility: Option<String>) -> Self {
        Self { name, location, visibility }
    }
}

impl std::fmt::Display for Module {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name)
    }
}

#[derive(Debug)]
pub struct Constant {
    pub name: String,
    pub location: Location,
    pub visibility: Option<String>,
    pub value: Option<String>,
}

impl Constant {
    pub fn new(name: String, location: Location, visibility: Option<String>, value: Option<String>) -> Self {
        Self { name, location, visibility, value }
    }
}

impl std::fmt::Display for Constant {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name)
    }
}

#[derive(Debug)]
pub struct Method {
    pub name: String,
    pub location: Location,
    pub visibility: Option<String>,
    pub parameters: Vec<String>,
}

impl Method {
    pub fn new(name: String, location: Location, visibility: Option<String>, parameters: Vec<String>) -> Self {
        Self { name, location, visibility, parameters }
    }
}

impl std::fmt::Display for Method {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} ({})", self.name, self.parameters.join(", "))
    }
}