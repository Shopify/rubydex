use ruby_prism::Visit;

use crate::pool::PoolId;

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct FileId(u32);

#[derive(Debug)]
pub struct File {
    pub name_id: PoolId<FileId>,
}

impl File {
    pub fn show(&self) {
        println!("{:?}", self.name_id);
    }
}

#[derive(Debug)]
pub struct Offset {
    pub start_offset: u32,
    pub end_offset: u32,
}

impl Offset {
    pub fn show(&self) {
        println!("{}-{}", self.start_offset, self.end_offset);
    }
}

#[derive(Debug)]
pub struct Visitor<'a> {
    locations: &'a mut Vec<Offset>,
    file: &'a str,
}

impl<'a> Visitor<'a> {
    pub fn new(locations: &'a mut Vec<Offset>, file: &'a str) -> Self {
        Self { locations, file }
    }

    fn make_offset(&self, node: ruby_prism::Location<'a>) -> Offset {
        Offset {
            start_offset: node.start_offset() as u32,
            end_offset: node.end_offset() as u32
        }
    }
}

impl<'a> Visit<'a> for Visitor<'a> {
    fn visit_class_node(&mut self, node: &ruby_prism::ClassNode<'a>) {
        let offset = self.make_offset(node.location());
        self.locations.push(offset);

        ruby_prism::visit_class_node(self, node);
    }

    fn visit_module_node(&mut self, node: &ruby_prism::ModuleNode<'a>) {
        let offset = self.make_offset(node.location());
        self.locations.push(offset);

        ruby_prism::visit_module_node(self, node);
    }

    fn visit_singleton_class_node(&mut self, node: &ruby_prism::SingletonClassNode<'a>) {
        let offset = self.make_offset(node.location());
        self.locations.push(offset);

        ruby_prism::visit_singleton_class_node(self, node);
    }

    fn visit_constant_write_node(&mut self, node: &ruby_prism::ConstantWriteNode<'a>) {
        let offset = self.make_offset(node.location());
        self.locations.push(offset);

        ruby_prism::visit_constant_write_node(self, node);
    }

    fn visit_constant_path_write_node(&mut self, node: &ruby_prism::ConstantPathWriteNode<'a>) {
        let offset = self.make_offset(node.location());
        self.locations.push(offset);

        ruby_prism::visit_constant_path_write_node(self, node);
    }

    fn visit_def_node(&mut self, node: &ruby_prism::DefNode<'a>) {
        let offset = self.make_offset(node.location());
        self.locations.push(offset);

        ruby_prism::visit_def_node(self, node);
    }

    fn visit_local_variable_write_node(&mut self, node: &ruby_prism::LocalVariableWriteNode<'a>) {
        let offset = self.make_offset(node.location());
        self.locations.push(offset);

        ruby_prism::visit_local_variable_write_node(self, node);
    }

    fn visit_instance_variable_write_node(&mut self, node: &ruby_prism::InstanceVariableWriteNode<'a>) {
        let offset = self.make_offset(node.location());
        self.locations.push(offset);

        ruby_prism::visit_instance_variable_write_node(self, node);
    }

    fn visit_class_variable_write_node(&mut self, node: &ruby_prism::ClassVariableWriteNode<'a>) {
        let offset = self.make_offset(node.location());
        self.locations.push(offset);

        ruby_prism::visit_class_variable_write_node(self, node);
    }

    fn visit_global_variable_write_node(&mut self, node: &ruby_prism::GlobalVariableWriteNode<'a>) {
        let offset = self.make_offset(node.location());
        self.locations.push(offset);

        ruby_prism::visit_global_variable_write_node(self, node);
    }
}