use crate::location::Location;

#[derive(Debug)]
pub enum Symbol {
    Class(Class),
    Module(Module),
    Constant(Constant),
    Method(Method),
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

#[derive(Debug)]
pub struct Method {
    pub name: String,
    pub location: Location,
    pub visibility: Option<String>,
    pub arguments: Vec<String>,
}

impl Method {
    pub fn new(name: String, location: Location, visibility: Option<String>, arguments: Vec<String>) -> Self {
        Self { name, location, visibility, arguments }
    }
}