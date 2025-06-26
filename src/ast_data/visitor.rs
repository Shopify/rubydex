use std::collections::HashMap;
use ruby_prism::{Visit};
use crate::ast_data::{symbol::{Symbol, Class, Module, Constant, Method}};
use crate::location::Location;

#[derive(Debug)]
pub struct Visitor<'a> {
    file: &'a str,
    symbols_table: &'a mut HashMap<String, Symbol>,
}

impl<'a> Visitor<'a> {
    pub fn new(file: &'a str, symbols_table: &'a mut HashMap<String, Symbol>) -> Self {
        Self { file, symbols_table }
    }
}

impl<'a> Visit<'a> for Visitor<'a> {
    fn visit_class_node(&mut self, node: &ruby_prism::ClassNode<'a>) {
        let class_name = String::from_utf8_lossy(node.name().as_slice());
        let location = Location::new(self.file.to_string(), node.location().start_offset(), node.location().end_offset());
        let symbol = Class::new(class_name.to_string(), location, None, None);
        self.symbols_table.insert(class_name.to_string(), Symbol::Class(symbol));

        ruby_prism::visit_class_node(self, node);
    }

    fn visit_module_node(&mut self, node: &ruby_prism::ModuleNode<'a>) {
        let module_name = String::from_utf8_lossy(node.name().as_slice());
        let location = Location::new(self.file.to_string(), node.location().start_offset(), node.location().end_offset());
        let symbol = Module::new(module_name.to_string(), location, None);
        self.symbols_table.insert(module_name.to_string(), Symbol::Module(symbol));

        ruby_prism::visit_module_node(self, node);
    }

    fn visit_constant_write_node(&mut self,node: &ruby_prism::ConstantWriteNode<'a>) {
        let constant_name = String::from_utf8_lossy(node.name().as_slice());
        let location = Location::new(self.file.to_string(), node.location().start_offset(), node.location().end_offset());
        let symbol = Constant::new(constant_name.to_string(), location, None, None);
        self.symbols_table.insert(constant_name.to_string(), Symbol::Constant(symbol));

        ruby_prism::visit_constant_write_node(self, node);
    }

    fn visit_def_node(&mut self, node: &ruby_prism::DefNode<'a>) {
        let method_name = String::from_utf8_lossy(node.name().as_slice());
        let location = Location::new(self.file.to_string(), node.location().start_offset(), node.location().end_offset());
        let symbol = Method::new(method_name.to_string(), location, None, Vec::new());
        self.symbols_table.insert(method_name.to_string(), Symbol::Method(symbol));

        ruby_prism::visit_def_node(self, node);
    }
}
