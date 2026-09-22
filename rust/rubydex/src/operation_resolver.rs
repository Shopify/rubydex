use crate::model::graph::Graph;

use crate::model::definitions::{ClassDefinition, Definition, ModuleDefinition};
use crate::model::ids::DefinitionId;
use crate::operation::Operation;
use crate::operation::ruby_builder::OperationBuilderResult;

#[derive(Clone, Copy)]
enum Nesting {
    LexicalScope { definition_id: DefinitionId },
}

pub struct OperationsResolver<'a> {
    graph: &'a mut Graph,
    operation_results: &'a Vec<OperationBuilderResult>,
    nesting_stack: Vec<Nesting>,
}

impl<'a> OperationsResolver<'a> {
    pub fn new(graph: &'a mut Graph, operation_results: &'a Vec<OperationBuilderResult>) -> Self {
        Self {
            graph,
            operation_results,
            nesting_stack: Vec::new(),
        }
    }

    pub fn resolve(&mut self) {
        for operation_result in self.operation_results {
            self.read_operations(operation_result);
        }
    }

    fn read_operations(&mut self, operation_result: &OperationBuilderResult) {
        for op in &operation_result.operations {
            self.handle_operation(operation_result, op.clone());
        }
    }
    fn handle_operation(&mut self, operation_result: &OperationBuilderResult, operation: Operation) {
        match operation {
            Operation::EnterClass(enter_class) => {
                self.handle_enter_class(operation_result, enter_class);
            }
            Operation::EnterModule(enter_module) => {
                self.handle_enter_module(operation_result, enter_module);
            }
            Operation::ExitScope => {
                self.handle_exit_scope();
            }

            _ => {}
        }
    }

    fn handle_enter_module(
        &mut self,
        operation_result: &OperationBuilderResult,
        operation: crate::operation::EnterModule,
    ) {
        println!("Handling EnterModule: {operation:?}");

        // Create the definition
        let def = ModuleDefinition::new(
            operation.name_id,
            operation.uri_id, // TODO: we should share the URI id through a document rather than per operation
            operation.offset,
            operation.name_offset,
            operation.comments,
            operation.flags,
            self.nesting_lexical_scope(),
        );

        let name_ref = operation_result.names.get(&operation.name_id).unwrap().clone();
        let str_ref = operation_result.strings.get(name_ref.str()).unwrap().clone();

        self.graph.insert_string(*name_ref.str(), str_ref);

        self.graph.insert_name(
            operation.name_id,
            operation_result.names.get(&operation.name_id).unwrap().clone(),
        );

        self.graph.add_definition(Definition::Module(Box::new(def)));
    }

    fn handle_enter_class(
        &mut self,
        operation_result: &OperationBuilderResult,
        operation: crate::operation::EnterClass,
    ) {
        println!("Handling EnterClass: {operation:?}");

        // Create the definition
        let def = ClassDefinition::new(
            operation.name_id,
            operation.uri_id, // TODO: we should share the URI id through a document rather than per operation
            operation.offset,
            operation.name_offset,
            operation.comments,
            operation.flags,
            self.nesting_lexical_scope(),
            None,
        );

        let name_ref = operation_result.names.get(&operation.name_id).unwrap().clone();
        let str_ref = operation_result.strings.get(name_ref.str()).unwrap().clone();

        self.graph.insert_string(*name_ref.str(), str_ref);

        self.graph.insert_name(
            operation.name_id,
            operation_result.names.get(&operation.name_id).unwrap().clone(),
        );

        self.nesting_stack.push(Nesting::LexicalScope {
            definition_id: def.id(),
        });

        self.graph.add_definition(Definition::Class(Box::new(def)));

        // TODO: let's try to create a delcaration with the knowledge that we have right now
    }

    fn handle_exit_scope(&mut self) {
        if let Some(Nesting::LexicalScope { definition_id: _ }) = self.nesting_stack.pop() {
            // Successfully exited a lexical scope
        } else {
            eprintln!("Warning: Attempted to exit a scope when no lexical scope was active");
        }
    }

    fn nesting_lexical_scope(&self) -> Option<DefinitionId> {
        self.nesting_stack
            .iter()
            .rfind(|nesting| matches!(nesting, Nesting::LexicalScope { .. }))
            .map(|nesting| {
                let Nesting::LexicalScope { definition_id } = nesting;
                *definition_id
            })
    }
}
