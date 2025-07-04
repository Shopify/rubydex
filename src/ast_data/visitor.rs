use std::collections::HashMap;
use ruby_prism::{Visit};
use crate::ast_data::symbol::{ClassData, ConstantData, MethodData, ModuleData, Symbol, SymbolData, SymbolKind, VarData, VarKind};
use crate::offset::Offset;
use crate::pool::PoolId;
use crate::tables::{GlobalTables, NameId};

#[derive(Debug)]
pub struct Visitor<'a> {
    tables: &'a mut GlobalTables,
    file: &'a str,
    symbols_table: &'a mut HashMap<PoolId<NameId>, Vec<Symbol>>,
    use_data: bool,
}

impl<'a> Visitor<'a> {
    pub fn new(tables: &'a mut GlobalTables, file: &'a str, symbols_table: &'a mut HashMap<PoolId<NameId>, Vec<Symbol>>, use_data: bool) -> Self {
        Self { tables, file, symbols_table, use_data }
    }

    pub fn location_to_offset(&self, location: &ruby_prism::Location) -> Offset {
        Offset::new(location.start_offset() as u32, location.end_offset() as u32)
    }
}

impl<'a> Visit<'a> for Visitor<'a> {
    fn visit_class_node(&mut self, node: &ruby_prism::ClassNode<'a>) {
        let name = String::from_utf8_lossy(node.name().as_slice());
        let name_id = self.tables.names.add(name.to_string());

        let offset = self.location_to_offset(&node.location());

        let mut superclass: Option<PoolId<NameId>> = None;
        if let Some(superclass_name) = node.superclass() {
            superclass = Some(self.tables.names.add(String::from_utf8_lossy(superclass_name.location().as_slice()).to_string()));
        }

        let data = if self.use_data {
            Some(SymbolData::Class(ClassData::new(superclass, None)))
        } else {
            None
        };

        let symbol = Symbol::new(SymbolKind::Class, name_id, offset, data);

        if let Some(symbols) = self.symbols_table.get_mut(&name_id) {
            symbols.push(symbol);
        } else {
            self.symbols_table.insert(name_id, vec![symbol]);
        }

        ruby_prism::visit_class_node(self, node);
    }

    fn visit_module_node(&mut self, node: &ruby_prism::ModuleNode<'a>) {
        let name = String::from_utf8_lossy(node.name().as_slice());
        let name_id = self.tables.names.add(name.to_string());

        let offset = self.location_to_offset(&node.location());

        let data = if self.use_data {
            Some(SymbolData::Module(ModuleData::new(None)))
        } else {
            None
        };

        let symbol = Symbol::new(SymbolKind::Module, name_id, offset, data);

        if let Some(symbols) = self.symbols_table.get_mut(&name_id) {
            symbols.push(symbol);
        } else {
            self.symbols_table.insert(name_id, vec![symbol]);
        }

        ruby_prism::visit_module_node(self, node);
    }

    fn visit_constant_write_node(&mut self,node: &ruby_prism::ConstantWriteNode<'a>) {
        let name = String::from_utf8_lossy(node.name().as_slice());
        let name_id = self.tables.names.add(name.to_string());

        let offset = self.location_to_offset(&node.location());
        let value = String::from_utf8_lossy(node.value().location().as_slice());

        let data = if self.use_data {
            Some(SymbolData::Constant(ConstantData::new(None, Some(value.to_string()))))
        } else {
            None
        };

        let symbol = Symbol::new(SymbolKind::Constant, name_id, offset, data);

        if let Some(symbols) = self.symbols_table.get_mut(&name_id) {
            symbols.push(symbol);
        } else {
            self.symbols_table.insert(name_id, vec![symbol]);
        }

        ruby_prism::visit_constant_write_node(self, node);
    }

    fn visit_def_node(&mut self, node: &ruby_prism::DefNode<'a>) {
        let name = String::from_utf8_lossy(node.name().as_slice());
        let name_id = self.tables.names.add(name.to_string());

        let offset = self.location_to_offset(&node.location());

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

        let data = if self.use_data {
            Some(SymbolData::Method(MethodData::new(None, parameters)))
        } else {
            None
        };

        let symbol = Symbol::new(SymbolKind::Method, name_id, offset, data);

        if let Some(symbols) = self.symbols_table.get_mut(&name_id) {
            symbols.push(symbol);
        } else {
            self.symbols_table.insert(name_id, vec![symbol]);
        }

        ruby_prism::visit_def_node(self, node);
    }

    fn visit_instance_variable_write_node(&mut self,node: &ruby_prism::InstanceVariableWriteNode<'a>) {
        let name = String::from_utf8_lossy(node.name().as_slice());
        let name_id = self.tables.names.add(name.to_string());

        let offset = self.location_to_offset(&node.location());

        let data = if self.use_data {
            Some(SymbolData::Var(VarData::new(VarKind::Instance, None)))
        } else {
            None
        };

        let symbol = Symbol::new(SymbolKind::Var, name_id, offset, data);

        if let Some(symbols) = self.symbols_table.get_mut(&name_id) {
            symbols.push(symbol);
        } else {
            self.symbols_table.insert(name_id, vec![symbol]);
        }
    }

    fn visit_class_variable_write_node(&mut self, node: &ruby_prism::ClassVariableWriteNode<'a>) {
        let name = String::from_utf8_lossy(node.name().as_slice());
        let name_id = self.tables.names.add(name.to_string());

        let offset = self.location_to_offset(&node.location());

        let data = if self.use_data {
            Some(SymbolData::Var(VarData::new(VarKind::Class, None)))
        } else {
            None
        };

        let symbol = Symbol::new(SymbolKind::Var, name_id, offset, data);

        if let Some(symbols) = self.symbols_table.get_mut(&name_id) {
            symbols.push(symbol);
        } else {
            self.symbols_table.insert(name_id, vec![symbol]);
        }
    }

    fn visit_local_variable_write_node(&mut self, node: &ruby_prism::LocalVariableWriteNode<'a>) {
        let name = String::from_utf8_lossy(node.name().as_slice());
        let name_id = self.tables.names.add(name.to_string());

        let offset = self.location_to_offset(&node.location());

        let data = if self.use_data {
            Some(SymbolData::Var(VarData::new(VarKind::Local, None)))
        } else {
            None
        };

        let symbol = Symbol::new(SymbolKind::Var, name_id, offset, data);

        if let Some(symbols) = self.symbols_table.get_mut(&name_id) {
            symbols.push(symbol);
        } else {
            self.symbols_table.insert(name_id, vec![symbol]);
        }
    }

    fn visit_global_variable_write_node(&mut self, node: &ruby_prism::GlobalVariableWriteNode<'a>) {
        let name = String::from_utf8_lossy(node.name().as_slice());
        let name_id = self.tables.names.add(name.to_string());

        let offset = self.location_to_offset(&node.location());

        let data = if self.use_data {
            Some(SymbolData::Var(VarData::new(VarKind::Global, None)))
        } else {
            None
        };

        let symbol = Symbol::new(SymbolKind::Var, name_id, offset, data);

        if let Some(symbols) = self.symbols_table.get_mut(&name_id) {
            symbols.push(symbol);
        } else {
            self.symbols_table.insert(name_id, vec![symbol]);
        }
    }
}
