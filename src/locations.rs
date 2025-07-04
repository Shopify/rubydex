use ruby_prism::Visit;

use crate::pool::PoolId;

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct FileId(u32);

#[derive(Debug)]
pub struct Location {
    pub file_id: PoolId<FileId>,
    pub start_offset: usize,
    pub end_offset: usize,
}

impl Location {
    pub fn show(&self) {
        println!("{:?}:{}-{}", self.file_id, self.start_offset, self.end_offset);
    }
}

#[derive(Debug)]
pub struct Visitor<'a> {
    locations: &'a mut Vec<Location>,
    file_id: PoolId<FileId>,
}

impl<'a> Visitor<'a> {
    pub fn new(locations: &'a mut Vec<Location>, file_id: PoolId<FileId>) -> Self {
        Self { locations, file_id }
    }

    fn make_location(&self, node: ruby_prism::Location<'a>) -> Location {
        Location {
            file_id: self.file_id,
            start_offset: node.start_offset(),
            end_offset: node.end_offset()
        }
    }
}

impl<'a> Visit<'a> for Visitor<'a> {
    fn visit_class_node(&mut self, node: &ruby_prism::ClassNode<'a>) {
        let location = self.make_location(node.location());
        self.locations.push(location);

        ruby_prism::visit_class_node(self, node);
    }

    fn visit_module_node(&mut self, node: &ruby_prism::ModuleNode<'a>) {
        let location = self.make_location(node.location());
        self.locations.push(location);

        ruby_prism::visit_module_node(self, node);
    }

    fn visit_singleton_class_node(&mut self, node: &ruby_prism::SingletonClassNode<'a>) {
        let location = self.make_location(node.location());
        self.locations.push(location);

        ruby_prism::visit_singleton_class_node(self, node);
    }

    fn visit_constant_write_node(&mut self, node: &ruby_prism::ConstantWriteNode<'a>) {
        let location = self.make_location(node.location());
        self.locations.push(location);

        ruby_prism::visit_constant_write_node(self, node);
    }

    fn visit_constant_path_write_node(&mut self, node: &ruby_prism::ConstantPathWriteNode<'a>) {
        let location = self.make_location(node.location());
        self.locations.push(location);

        ruby_prism::visit_constant_path_write_node(self, node);
    }

    fn visit_def_node(&mut self, node: &ruby_prism::DefNode<'a>) {
        let location = self.make_location(node.location());
        self.locations.push(location);

        ruby_prism::visit_def_node(self, node);
    }

    fn visit_local_variable_write_node(&mut self, node: &ruby_prism::LocalVariableWriteNode<'a>) {
        let location = self.make_location(node.location());
        self.locations.push(location);

        ruby_prism::visit_local_variable_write_node(self, node);
    }

    fn visit_instance_variable_write_node(&mut self, node: &ruby_prism::InstanceVariableWriteNode<'a>) {
        let location = self.make_location(node.location());
        self.locations.push(location);

        ruby_prism::visit_instance_variable_write_node(self, node);
    }

    fn visit_class_variable_write_node(&mut self, node: &ruby_prism::ClassVariableWriteNode<'a>) {
        let location = self.make_location(node.location());
        self.locations.push(location);

        ruby_prism::visit_class_variable_write_node(self, node);
    }

    fn visit_global_variable_write_node(&mut self, node: &ruby_prism::GlobalVariableWriteNode<'a>) {
        let location = self.make_location(node.location());
        self.locations.push(location);

        ruby_prism::visit_global_variable_write_node(self, node);
    }
}