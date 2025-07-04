use crate::location::Location;
use crate::tables::{NameId, GlobalTables};
use crate::pool::PoolId;

#[derive(Debug)]
pub enum Symbol {
    Class(Class),
    Module(Module),
    Constant(Constant),
    Method(Method),
    Var(Var),
}

impl Symbol {
    pub fn to_string(&self, tables: &GlobalTables) -> String {
        match self {
            Symbol::Class(class) => class.to_string(tables),
            Symbol::Module(module) => module.to_string(tables),
            Symbol::Constant(constant) => constant.to_string(tables),
            Symbol::Method(method) => method.to_string(tables),
            Symbol::Var(var) => var.to_string(tables),
        }
    }
}

#[derive(Debug)]
pub struct Class {
    pub name_id: PoolId<NameId>,
    pub location: Location,
    pub superclass: Option<PoolId<NameId>>,
    pub visibility: Option<String>,
}

impl Class {
    pub fn new(name_id: PoolId<NameId>, location: Location, superclass: Option<PoolId<NameId>>, visibility: Option<String>) -> Self {
        Self { name_id, location, superclass, visibility }
    }

    pub fn to_string(&self, tables: &GlobalTables) -> String {
        let superclass = self.superclass.as_ref().map(|id| tables.names.get(id).unwrap().to_string());
        let name = tables.names.get(&self.name_id).unwrap().to_string();
        if let Some(superclass) = superclass {
            format!("{} class {} < {} -- {}", self.visibility.as_ref().unwrap_or(&String::new()), name, superclass, self.location.to_string(tables))
        } else {
            format!("{} class {} -- {}", self.visibility.as_ref().unwrap_or(&String::new()), name, self.location.to_string(tables))
        }
    }
}

#[derive(Debug)]
pub struct Module {
    pub name_id: PoolId<NameId>,
    pub location: Location,
    pub visibility: Option<String>,
}

impl Module {
    pub fn new(name_id: PoolId<NameId>, location: Location, visibility: Option<String>) -> Self {
        Self { name_id, location, visibility }
    }

    pub fn to_string(&self, tables: &GlobalTables) -> String {
        format!("{} module {} -- {}", self.visibility.as_ref().unwrap_or(&String::new()), tables.names.get(&self.name_id).unwrap(), self.location.to_string(tables))
    }
}

#[derive(Debug)]
pub struct Constant {
    pub name_id: PoolId<NameId>,
    pub location: Location,
    pub visibility: Option<String>,
    pub value: Option<String>,
}

impl Constant {
    pub fn new(name_id: PoolId<NameId>, location: Location, visibility: Option<String>, value: Option<String>) -> Self {
        Self { name_id, location, visibility, value }
    }

    pub fn to_string(&self, tables: &GlobalTables) -> String {
        format!("{} constant {} = {} -- {}", self.visibility.as_ref().unwrap_or(&String::new()), tables.names.get(&self.name_id).unwrap(), self.value.as_ref().unwrap_or(&String::new()), self.location.to_string(tables))
    }
}

#[derive(Debug)]
pub struct Method {
    pub name_id: PoolId<NameId>,
    pub location: Location,
    pub visibility: Option<String>,
    pub parameters: Vec<PoolId<NameId>>,
}

impl Method {
    pub fn new(name_id: PoolId<NameId>, location: Location, visibility: Option<String>, parameters: Vec<PoolId<NameId>>) -> Self {
        Self { name_id, location, visibility, parameters }
    }

    pub fn to_string(&self, tables: &GlobalTables) -> String {
        let parameters = self.parameters.iter().map(|id| tables.names.get(id).unwrap().to_string()).collect::<Vec<String>>().join(", ");
        format!("{} method {}({}) -- {}", self.visibility.as_ref().unwrap_or(&String::new()), tables.names.get(&self.name_id).unwrap(), parameters, self.location.to_string(tables))
    }
}

#[derive(Debug)]
pub enum VarKind {
    Local,
    Instance,
    Class,
}

impl VarKind {
    pub fn to_string(&self) -> &str {
        match self {
            VarKind::Local => "local",
            VarKind::Instance => "instance",
            VarKind::Class => "class",
        }
    }
}

#[derive(Debug)]
pub struct Var {
    pub name_id: PoolId<NameId>,
    pub location: Location,
    pub visibility: Option<String>,
    pub kind: VarKind,
}

impl Var {
    pub fn new(name_id: PoolId<NameId>, location: Location, visibility: Option<String>, kind: VarKind) -> Self {
        Self { name_id, location, visibility, kind }
    }

    pub fn to_string(&self, tables: &GlobalTables) -> String {
        format!("{} var {} {} -- {}", self.kind.to_string(), self.visibility.as_ref().unwrap_or(&String::new()), tables.names.get(&self.name_id).unwrap(), self.location.to_string(tables))
    }
}