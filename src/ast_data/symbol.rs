use crate::location::Location;
use crate::tables::{NameId, GlobalTables};
use crate::pool::PoolId;

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

impl SymbolData {
    pub fn to_string(&self, tables: &GlobalTables) -> String {
        match self {
            SymbolData::Class(data) => data.to_string(tables),
            SymbolData::Module(data) => data.to_string(tables),
            SymbolData::Constant(data) => data.to_string(tables),
            SymbolData::Method(data) => data.to_string(tables),
        }
    }
}

#[derive(Debug)]
pub struct Symbol {
    pub kind: SymbolKind,
    pub name_id: PoolId<NameId>,
    pub location: Location,
    pub data: Option<SymbolData>,
}

impl Symbol {
    pub fn new(kind: SymbolKind, name_id: PoolId<NameId>, location: Location, data: Option<SymbolData>) -> Self {
        Self { kind, name_id, location, data }
    }

    pub fn to_string(&self, tables: &GlobalTables) -> String {
        let name = tables.names.get(&self.name_id).unwrap().to_string();
        let data_str = self.data.as_ref().map(|data| data.to_string(tables)).unwrap_or(String::new());
        format!("{} {} ({}) {}", self.kind.to_string(), name, self.location.to_string(tables), data_str)
    }
}

#[derive(Debug)]
pub struct ClassData {
    pub superclass: Option<PoolId<NameId>>,
    pub visibility: Option<String>,
}

impl ClassData {
    pub fn new(superclass: Option<PoolId<NameId>>, visibility: Option<String>) -> Self {
        Self { superclass, visibility }
    }

    pub fn to_string(&self, tables: &GlobalTables) -> String {
        let superclass = self.superclass.as_ref().map(|id| tables.names.get(id).unwrap().to_string());
        format!("< {}", superclass.as_ref().unwrap_or(&String::new()))
    }
}

#[derive(Debug)]
pub struct ModuleData {
    pub visibility: Option<String>,
}

impl ModuleData {
    pub fn to_string(&self, tables: &GlobalTables) -> String {
        format!("{}", self.visibility.as_ref().unwrap_or(&String::new()))
    }
}

#[derive(Debug)]
pub struct ConstantData {
    pub visibility: Option<String>,
    pub value: Option<String>,
}

impl ConstantData {
    pub fn to_string(&self, tables: &GlobalTables) -> String {
        format!("{}", self.value.as_ref().unwrap_or(&String::new()))
    }
}

#[derive(Debug)]
pub struct MethodData {
    pub visibility: Option<String>,
    pub parameters: Vec<PoolId<NameId>>,
}

impl MethodData {
    pub fn new(visibility: Option<String>, parameters: Vec<PoolId<NameId>>) -> Self {
        Self { visibility, parameters }
    }

    pub fn to_string(&self, tables: &GlobalTables) -> String {
        let parameters = self.parameters.iter().map(|id| tables.names.get(id).unwrap().to_string()).collect::<Vec<String>>().join(", ");
        format!("{} ({})", self.visibility.as_ref().unwrap_or(&String::new()), parameters)
    }
}

