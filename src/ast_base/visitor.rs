use std::collections::HashMap;
use ruby_prism::{Visit};
use crate::ast_base::{symbol::{Symbol, Class, Module, Constant, Method}};
use crate::location::Location;
use crate::tables::{GlobalTables, NameId};
use crate::pool::PoolId;

#[derive(Debug)]
pub struct Visitor<'a> {
    tables: &'a mut GlobalTables,
    file: &'a str,
    symbols_table: &'a mut HashMap<String, Symbol>,
}

impl<'a> Visitor<'a> {
    pub fn new(tables: &'a mut GlobalTables, file: &'a str, symbols_table: &'a mut HashMap<String, Symbol>) -> Self {
        Self { tables, file, symbols_table }
    }
}

impl<'a> Visit<'a> for Visitor<'a> {
    fn visit_class_node(&mut self, node: &ruby_prism::ClassNode<'a>) {
        let class_name = String::from_utf8_lossy(node.name().as_slice());
        let location = Location::new(self.tables.files.add(self.file.to_string()), node.location().start_offset(), node.location().end_offset());

        let mut superclass: Option<PoolId<NameId>> = None;
        if let Some(superclass_name) = node.superclass() {
            superclass = Some(self.tables.names.add(String::from_utf8_lossy(superclass_name.location().as_slice()).to_string()));
        }

        let symbol = Class::new(self.tables.names.add(class_name.to_string()), location, superclass, None);
        self.symbols_table.insert(class_name.to_string(), symbol.base);

        ruby_prism::visit_class_node(self, node);
    }

    fn visit_module_node(&mut self, node: &ruby_prism::ModuleNode<'a>) {
        let module_name = String::from_utf8_lossy(node.name().as_slice());
        let location = Location::new(self.tables.files.add(self.file.to_string()), node.location().start_offset(), node.location().end_offset());
        let symbol = Module::new(self.tables.names.add(module_name.to_string()), location, None);
        self.symbols_table.insert(module_name.to_string(), symbol.base);

        ruby_prism::visit_module_node(self, node);
    }

    fn visit_constant_write_node(&mut self,node: &ruby_prism::ConstantWriteNode<'a>) {
        let constant_name = String::from_utf8_lossy(node.name().as_slice());
        let location = Location::new(self.tables.files.add(self.file.to_string()), node.location().start_offset(), node.location().end_offset());
        let symbol = Constant::new(self.tables.names.add(constant_name.to_string()), location, None, None);
        self.symbols_table.insert(constant_name.to_string(), symbol.base);

        ruby_prism::visit_constant_write_node(self, node);
    }

    fn visit_def_node(&mut self, node: &ruby_prism::DefNode<'a>) {
        let method_name = String::from_utf8_lossy(node.name().as_slice());
        let location = Location::new(self.tables.files.add(self.file.to_string()), node.location().start_offset(), node.location().end_offset());

        let mut parameters: Vec<PoolId<NameId>> = Vec::new();
        if let Some(parameters_list) = node.parameters() {
            for parameter in parameters_list.requireds().iter() {
                parameters.push(self.tables.names.add(String::from_utf8_lossy(parameter.location().as_slice()).to_string()));
            }
            for parameter in parameters_list.optionals().iter() {
                parameters.push(self.tables.names.add(String::from_utf8_lossy(parameter.location().as_slice()).to_string()));
            }
            for parameter in parameters_list.rest().iter() {
                parameters.push(self.tables.names.add(String::from_utf8_lossy(parameter.location().as_slice()).to_string()));
            }
            for parameter in parameters_list.keywords().iter() {
                parameters.push(self.tables.names.add(String::from_utf8_lossy(parameter.location().as_slice()).to_string()));
            }
            if let Some(rest) = parameters_list.keyword_rest() {
                parameters.push(self.tables.names.add(String::from_utf8_lossy(rest.location().as_slice()).to_string()));
            }
            if let Some(block) = parameters_list.block() {
                parameters.push(self.tables.names.add(String::from_utf8_lossy(block.location().as_slice()).to_string()));
            }
        }

        let symbol = Method::new(self.tables.names.add(method_name.to_string()), location, None, parameters);
        self.symbols_table.insert(method_name.to_string(), symbol.base);

        ruby_prism::visit_def_node(self, node);
    }
}
