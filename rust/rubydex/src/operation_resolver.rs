use std::default;

use crate::model::graph::Graph;

use crate::operation::Operation;
use crate::operation::ruby_builder::{OperationBuilderResult, RubyOperationBuilder};
use crate::model::definitions::ModuleDefinition;

pub struct OperationsResolver<'a> {
    graph: &'a mut Graph,
    operation_results: &'a Vec<OperationBuilderResult>,
}

impl<'a> OperationsResolver<'a> {
    pub fn new(graph: &'a mut Graph, operation_results: &'a Vec<OperationBuilderResult>) -> Self {
        Self {
            graph,
            operation_results,
        }
    }

    pub fn resolve(&mut self) {
        for operation_result in self.operation_results {
            self.read_operations(operation_result);
        }
    }

    fn read_operations(&mut self, operation_result: &OperationBuilderResult) {
        for op in &operation_result.operations {
            self.handle_operation(operation_result, op);
        }
    }
    fn handle_operation(&mut self, operation_result: &OperationBuilderResult, operation: &Operation) {
        match operation {
            // Operation::EnterClass(enter_class) => {
            //     println!("EnterClass: {:?}", enter_class);
            // }
            Operation::EnterModule(enter_module) => {
                self.handle_enter_module(operation_result, enter_module);
            }
            _ => {}
        }
    }

    fn handle_enter_module(
        &mut self,
        operation_result: &OperationBuilderResult,
        enter_module: &crate::operation::EnterModule,
    ) {
        // Here you can implement the logic to handle the EnterModule operation
        // For example, you might want to add the module to the graph or perform other actions
        println!("Handling EnterModule: {:?}", enter_module);

        // Create the definition
        let def = ModuleDefinition::new(
            operation.name_id,
            uri_id: UriId,
            offset: Offset,
            name_offset: Offset,
            comments: Box<[Comment]>,
            flags: DefinitionFlags,
            lexical_nesting_id: Option<DefinitionId>,
        )

        // Create the declaration
    }
}
