use crate::location::Location;
use crate::tables::{NameId, GlobalTables};
use crate::pool::PoolId;

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
    pub name_id: PoolId<NameId>,
    pub location: Location,
}

impl Symbol {
    pub fn new(kind: SymbolKind, name_id: PoolId<NameId>, location: Location) -> Self {
        Self { kind, name_id, location }
    }

    pub fn as_class(&self) -> Option<&Class> {
        match self.kind {
            SymbolKind::Class => Some(mem::transmute(self)),
            _ => None,
        }
    }

    pub fn as_module(&self) -> Option<&Module> {
        match self.kind {
            SymbolKind::Module => Some(&self),
            _ => None,
        }
    }

    pub fn as_constant(&self) -> Option<&Constant> {
        match self.kind {
            SymbolKind::Constant => Some(&self),
            _ => None,
        }
    }

    pub fn as_method(&self) -> Option<&Method> {
        match self.kind {
            SymbolKind::Method => Some(&self),
            _ => None,
        }
    }
}

#[derive(Debug)]
pub struct Class {
    pub base: Symbol,
    pub superclass: Option<PoolId<NameId>>,
    pub visibility: Option<String>,
}

impl Class {
    pub fn new(name_id: PoolId<NameId>, location: Location, superclass: Option<PoolId<NameId>>, visibility: Option<String>) -> Self {
        Self {
            base: Symbol::new(SymbolKind::Class, name_id, location),
            superclass,
            visibility,
        }
    }

    pub fn to_string(&self, tables: &GlobalTables) -> String {
        let superclass_name = self.superclass.as_ref().map(|id| tables.names.get(id).unwrap().to_string());
        format!("{} < {}", self.base.to_string(tables), superclass_name.as_ref().unwrap_or(&String::new()))
    }
}

#[derive(Debug)]
pub struct Module {
    pub base: Symbol,
    pub visibility: Option<String>,
}

impl Module {
    pub fn new(name_id: PoolId<NameId>, location: Location, visibility: Option<String>) -> Self {
        Self {
            base: Symbol::new(SymbolKind::Module, name_id, location),
            visibility,
        }
    }

    pub fn to_string(&self, tables: &GlobalTables) -> String {
        format!("{} ({})", self.base.to_string(tables), self.visibility.as_ref().unwrap_or(&String::new()))
    }
}

#[derive(Debug)]
pub struct Constant {
    pub base: Symbol,
    pub visibility: Option<String>,
    pub value: Option<String>,
}

impl Constant {
    pub fn new(name_id: PoolId<NameId>, location: Location, visibility: Option<String>, value: Option<String>) -> Self {
        Self {
            base: Symbol::new(SymbolKind::Constant, name_id, location),
            visibility,
            value,
        }
    }

    pub fn to_string(&self, tables: &GlobalTables) -> String {
        format!("{} ({})", self.base.to_string(tables), self.value.as_ref().unwrap_or(&String::new()))
    }
}


#[derive(Debug)]
pub struct Method {
    pub base: Symbol,
    pub visibility: Option<String>,
    pub parameters: Vec<PoolId<NameId>>,
}

impl Method {
    pub fn new(name_id: PoolId<NameId>, location: Location, visibility: Option<String>, parameters: Vec<PoolId<NameId>>) -> Self {
        Self {
            base: Symbol::new(SymbolKind::Method, name_id, location),
            visibility,
            parameters,
        }
    }

    pub fn to_string(&self, tables: &GlobalTables) -> String {
        let parameters = self.parameters.iter().map(|id| tables.names.get(id).unwrap().to_string()).collect::<Vec<String>>().join(", ");
        format!("{} ({})", self.base.to_string(tables), parameters)
    }
}
