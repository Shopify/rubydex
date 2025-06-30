use std::collections::HashMap;

use crate::declaration::{ClassDeclaration, Declaration, ModuleDeclaration};

#[derive(Debug)]
pub struct Repository {
    entries: HashMap<String, Declaration>,
}

impl Repository {
    pub fn new() -> Self {
        Repository {
            entries: HashMap::new(),
        }
    }

    pub fn add_class(&mut self, name: String, location: ruby_prism::Location) {
        let declaration = Declaration::Class(ClassDeclaration::new(location));
        self.entries.insert(name, declaration);
    }

    pub fn add_module(&mut self, name: String, location: ruby_prism::Location) {
        let declaration = Declaration::Module(ModuleDeclaration::new(location));
        self.entries.insert(name, declaration);
    }

    pub fn size(&self) -> usize {
        self.entries.len()
    }
}

impl Default for Repository {
    fn default() -> Self {
        Self::new()
    }
}
