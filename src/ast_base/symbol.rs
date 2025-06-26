use crate::location::Location;

#[derive(Debug)]
pub enum SymbolKind {
    Class,
    Module,
    Constant,
    Method,
}

#[derive(Debug)]
pub enum SymbolData {
    Class(ClassData),
    Module(ModuleData),
    Constant(ConstantData),
    Method(MethodData),
}

#[derive(Debug)]
pub struct Symbol {
    pub kind: SymbolKind,
    pub name: String,
    pub location: Location,
    pub data: Option<SymbolData>,
}

#[derive(Debug)]
pub struct ClassData {
    pub superclass: Option<String>,
    pub visibility: Option<String>,
}

#[derive(Debug)]
pub struct ModuleData {
    pub visibility: Option<String>,
}

#[derive(Debug)]
pub struct ConstantData {
    pub visibility: Option<String>,
    pub value: Option<String>,
}

#[derive(Debug)]
pub struct MethodData {
    pub visibility: Option<String>,
    pub arguments: Vec<String>,
}

impl Symbol {
    pub fn new(kind: SymbolKind, name: String, location: Location, data: Option<SymbolData>) -> Self {
        Self { kind, name, location, data }
    }
}
