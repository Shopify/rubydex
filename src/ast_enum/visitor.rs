use std::collections::HashMap;
use ruby_prism::{Visit};
use crate::ast_enum::{symbol::{Symbol, Class, Module, Constant, Method, Var, VarKind}};
use crate::location::Location;
use crate::tables::{GlobalTables, NameId};
use crate::pool::PoolId;

#[derive(Debug)]
pub struct Visitor<'a> {
    tables: &'a mut GlobalTables,
    file: &'a str,
    symbols_table: &'a mut HashMap<PoolId<NameId>, Vec<Symbol>>,
}

impl<'a> Visitor<'a> {
    pub fn new(tables: &'a mut GlobalTables, file: &'a str, symbols_table: &'a mut HashMap<PoolId<NameId>, Vec<Symbol>>) -> Self {
        Self { tables, file, symbols_table }
    }
}

impl<'a> Visit<'a> for Visitor<'a> {
    fn visit_class_node(&mut self, node: &ruby_prism::ClassNode<'a>) {
        let name = String::from_utf8_lossy(node.name().as_slice());
        let name_id = self.tables.names.add(name.to_string());

        let location = Location::new(self.tables.files.add(self.file.to_string()), node.location().start_offset(), node.location().end_offset());

        let mut superclass: Option<PoolId<NameId>> = None;
        if let Some(superclass_name) = node.superclass() {
            superclass = Some(self.tables.names.add(String::from_utf8_lossy(superclass_name.location().as_slice()).to_string()));
        }

        let symbol = Class::new(name_id, location, superclass, None);

        if let Some(symbols) = self.symbols_table.get_mut(&name_id) {
            symbols.push(Symbol::Class(symbol));
        } else {
            self.symbols_table.insert(name_id, vec![Symbol::Class(symbol)]);
        }

        ruby_prism::visit_class_node(self, node);
    }

    fn visit_module_node(&mut self, node: &ruby_prism::ModuleNode<'a>) {
        let name = String::from_utf8_lossy(node.name().as_slice());
        let name_id = self.tables.names.add(name.to_string());

        let location = Location::new(self.tables.files.add(self.file.to_string()), node.location().start_offset(), node.location().end_offset());
        let symbol = Module::new(name_id, location, None);

        if let Some(symbols) = self.symbols_table.get_mut(&name_id) {
            symbols.push(Symbol::Module(symbol));
        } else {
            self.symbols_table.insert(name_id, vec![Symbol::Module(symbol)]);
        }

        ruby_prism::visit_module_node(self, node);
    }

    fn visit_constant_write_node(&mut self,node: &ruby_prism::ConstantWriteNode<'a>) {
        let name = String::from_utf8_lossy(node.name().as_slice());
        let name_id = self.tables.names.add(name.to_string());

        let location = Location::new(self.tables.files.add(self.file.to_string()), node.location().start_offset(), node.location().end_offset());
        let value = String::from_utf8_lossy(node.value().location().as_slice());
        let symbol = Constant::new(name_id, location, None, Some(value.to_string()));

        if let Some(symbols) = self.symbols_table.get_mut(&name_id) {
            symbols.push(Symbol::Constant(symbol));
        } else {
            self.symbols_table.insert(name_id, vec![Symbol::Constant(symbol)]);
        }

        ruby_prism::visit_constant_write_node(self, node);
    }

    fn visit_def_node(&mut self, node: &ruby_prism::DefNode<'a>) {
        let name = String::from_utf8_lossy(node.name().as_slice());
        let name_id = self.tables.names.add(name.to_string());

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

        let symbol = Method::new(name_id, location, None, parameters);
        if let Some(symbols) = self.symbols_table.get_mut(&name_id) {
            symbols.push(Symbol::Method(symbol));
        } else {
            self.symbols_table.insert(name_id, vec![Symbol::Method(symbol)]);
        }

        ruby_prism::visit_def_node(self, node);
    }

    // fn visit_instance_variable_write_node(&mut self,node: &ruby_prism::InstanceVariableWriteNode<'a>) {
    //     let name = String::from_utf8_lossy(node.name().as_slice());
    //     let name_id = self.tables.names.add(name.to_string());

    //     let location = Location::new(self.tables.files.add(self.file.to_string()), node.location().start_offset(), node.location().end_offset());
    //     let symbol = Var::new(name_id, location, None, VarKind::Instance);

    //     if let Some(symbols) = self.symbols_table.get_mut(&name_id) {
    //         symbols.push(Symbol::Var(symbol));
    //     } else {
    //         self.symbols_table.insert(name_id, vec![Symbol::Var(symbol)]);
    //     }
    // }

    // fn visit_class_variable_write_node(&mut self, node: &ruby_prism::ClassVariableWriteNode<'a>) {
    //     let name = String::from_utf8_lossy(node.name().as_slice());
    //     let name_id = self.tables.names.add(name.to_string());

    //     let location = Location::new(self.tables.files.add(self.file.to_string()), node.location().start_offset(), node.location().end_offset());
    //     let symbol = Var::new(name_id, location, None, VarKind::Class);

    //     if let Some(symbols) = self.symbols_table.get_mut(&name_id) {
    //         symbols.push(Symbol::Var(symbol));
    //     } else {
    //         self.symbols_table.insert(name_id, vec![Symbol::Var(symbol)]);
    //     }
    // }

    // fn visit_local_variable_write_node(&mut self, node: &ruby_prism::LocalVariableWriteNode<'a>) {
    //     let name = String::from_utf8_lossy(node.name().as_slice());
    //     let name_id = self.tables.names.add(name.to_string());

    //     let location = Location::new(self.tables.files.add(self.file.to_string()), node.location().start_offset(), node.location().end_offset());

    //     let symbol = Var::new(name_id, location, None, VarKind::Local);

    //     if let Some(symbols) = self.symbols_table.get_mut(&name_id) {
    //         symbols.push(Symbol::Var(symbol));
    //     } else {
    //         self.symbols_table.insert(name_id, vec![Symbol::Var(symbol)]);
    //     }
    // }
}
