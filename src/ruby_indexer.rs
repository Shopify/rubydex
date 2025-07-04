use ruby_prism::Visit;
use std::sync::{Arc, Mutex};

use crate::{declaration::Location, Repository};

pub struct RubyIndexer<'a> {
    nesting: Vec<String>,
    repository: &'a Arc<Mutex<Repository>>,
}

impl<'a> RubyIndexer<'a> {
    pub fn new(repository: &'a Arc<Mutex<Repository>>) -> Self {
        RubyIndexer {
            nesting: Vec::new(),
            repository,
        }
    }

    pub fn index(&mut self, file_path: String) {
        let source = std::fs::read_to_string(&file_path).unwrap();
        let result = ruby_prism::parse(source.as_ref());

        self.visit(&result.node());
        self.nesting.clear();
    }
}

impl Visit<'_> for RubyIndexer<'_> {
    fn visit_class_node(&mut self, node: &ruby_prism::ClassNode) {
        match std::str::from_utf8(node.constant_path().location().as_slice()).map(str::to_string) {
            Ok(name) => {
                let mut repo_guard = self.repository.lock().unwrap();
                let loc = Location::from_prism_location(node.location());
                repo_guard.add_class(name, loc);
            }
            Err(_) => {
                // Name is not a valid UTF-8 string
            }
        }

        if let Some(body) = node.body() {
            self.visit(&body);
        }
    }

    fn visit_module_node(&mut self, node: &ruby_prism::ModuleNode) {
        match std::str::from_utf8(node.constant_path().location().as_slice()).map(str::to_string) {
            Ok(name) => {
                let mut repo_guard = self.repository.lock().unwrap();
                let loc = Location::from_prism_location(node.location());
                repo_guard.add_module(name, loc);
            }
            Err(_) => {
                // Name is not a valid UTF-8 string
            }
        }

        if let Some(body) = node.body() {
            self.visit(&body);
        }
    }
}
