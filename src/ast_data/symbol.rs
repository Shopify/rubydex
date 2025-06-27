use crate::location::Location;

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
    pub name: String,
    pub location: Location,
    pub data: Option<SymbolData>,
}

impl Symbol {
    pub fn new(kind: SymbolKind, name: String, location: Location, data: Option<SymbolData>) -> Self {
        Self { kind, name, location, data }
    }
}

impl std::fmt::Display for Symbol {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} {} ({}) {}", self.kind.to_string(), self.name, self.location, self.data.as_ref().map(|data| data.to_string()).unwrap_or(String::new()))
    }
}

#[derive(Debug)]
pub struct ClassData {
    pub superclass: Option<String>,
    pub visibility: Option<String>,
}

impl std::fmt::Display for ClassData {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "< {}", self.superclass.as_ref().unwrap_or(&String::new()))
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

impl std::fmt::Display for MethodData {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.parameters.join(", "))
    }
}
