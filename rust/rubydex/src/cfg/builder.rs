use ruby_prism::{Node, Visit};

use crate::{
    cfg::{BasicBlock, BlockId, ControlFlowGraph, Instruction, Terminator},
    offset::Offset,
};

/// Builds one CFG for a body of Ruby statements represented by a Prism node.
///
/// Instructions currently retain only source locations. The builder establishes
/// block boundaries and edges for sequential statements, conditionals, loops,
/// and returns; instruction semantics will be added separately.
///
/// # Panics
///
/// Panics if the node's source range does not fit in `u32`.
#[must_use]
pub fn build_cfg(node: &Node<'_>) -> ControlFlowGraph {
    let mut builder = Builder::new();
    builder.visit(node);
    let end_offset = node
        .location()
        .end_offset()
        .try_into()
        .expect("source offset must fit in u32");
    builder.finish(end_offset)
}

struct Builder {
    blocks: Vec<BasicBlock>,
    current: Option<BlockId>,
    branch_stack: Vec<(Offset, bool)>,
}

impl Builder {
    fn new() -> Self {
        Self {
            blocks: vec![
                BasicBlock::new(BlockId::ENTRY, Vec::new(), Terminator::Todo),
                BasicBlock::new(BlockId::EXIT, Vec::new(), Terminator::Exit),
            ],
            current: Some(BlockId::ENTRY),
            branch_stack: Vec::new(),
        }
    }

    /// Finalizes any block that still needs a terminator and returns the graph.
    fn finish(mut self, end: u32) -> ControlFlowGraph {
        for block in &mut self.blocks {
            if matches!(block.terminator, Terminator::Todo) {
                block.terminator = Terminator::Jump {
                    target: BlockId::EXIT,
                    location: Offset::new(end, end),
                };
            }
        }
        ControlFlowGraph::new(self.blocks)
    }

    /// Builds the predicate, body, alternate path, and merge edges shared by `if` and `unless`.
    fn visit_if(
        &mut self,
        predicate: &Node<'_>,
        body: Option<ruby_prism::StatementsNode<'_>>,
        alternate: Option<Node<'_>>,
        location: &ruby_prism::Location<'_>,
        body_on_false: bool,
    ) {
        if self.current.is_none() {
            return;
        }
        self.visit(predicate);
        let Some(current) = self.current else {
            return;
        };
        let body_block = self.fresh_block();
        let alternate_block = self.fresh_block();
        // `unless` keeps the original predicate and swaps its successors.
        let (truthy, falsey) = if body_on_false {
            (alternate_block, body_block)
        } else {
            (body_block, alternate_block)
        };
        self.set_terminator(
            current,
            Terminator::Branch {
                then_block: truthy,
                else_block: falsey,
                location: Offset::from_prism_location(&predicate.location()),
            },
        );

        self.current = Some(body_block);
        if let Some(body) = body {
            self.visit(&body.as_node());
        }
        let body_end = self.current;

        self.current = Some(alternate_block);
        if let Some(alternate) = alternate {
            self.visit(&alternate);
        }
        let alternate_end = self.current;
        self.current = self.join(body_end, alternate_end, location);
    }

    /// Builds a loop header, body, exit path, and body-to-header backedge shared by `while` and `until`.
    fn visit_loop(
        &mut self,
        predicate: &Node<'_>,
        body: Option<ruby_prism::StatementsNode<'_>>,
        location: &ruby_prism::Location<'_>,
        body_on_false: bool,
    ) {
        let Some(current) = self.current else {
            return;
        };
        let header = self.fresh_block();
        let body_block = self.fresh_block();
        let after = self.fresh_block();
        self.set_terminator(
            current,
            Terminator::Jump {
                target: header,
                location: Offset::from_prism_location(location),
            },
        );

        self.current = Some(header);
        self.visit(predicate);
        let Some(condition_end) = self.current else {
            return;
        };
        // `until` keeps the original predicate and swaps its successors.
        let (truthy, falsey) = if body_on_false {
            (after, body_block)
        } else {
            (body_block, after)
        };
        self.set_terminator(
            condition_end,
            Terminator::Branch {
                then_block: truthy,
                else_block: falsey,
                location: Offset::from_prism_location(&predicate.location()),
            },
        );

        self.current = Some(body_block);
        if let Some(body) = body {
            self.visit(&body.as_node());
        }
        if let Some(body_end) = self.current {
            self.jump_if_todo(body_end, header, Offset::from_prism_location(location));
        }
        self.current = Some(after);
    }

    /// Combines two continuing control-flow paths, creating a merge block when both paths remain live.
    fn join(
        &mut self,
        left: Option<BlockId>,
        right: Option<BlockId>,
        location: &ruby_prism::Location<'_>,
    ) -> Option<BlockId> {
        match (left, right) {
            (None, None) => None,
            (Some(block), None) | (None, Some(block)) => Some(block),
            (Some(left), Some(right)) => {
                let join = self.fresh_block();
                let location = Offset::from_prism_location(location);
                self.jump_if_todo(left, join, location.clone());
                self.jump_if_todo(right, join, location);
                Some(join)
            }
        }
    }

    /// Returns the block currently receiving instructions, creating a new unreachable block when a previous statement
    /// terminated its path.
    fn ensure_current(&mut self) -> BlockId {
        if let Some(current) = self.current {
            current
        } else {
            let block = self.fresh_block();
            self.current = Some(block);
            block
        }
    }

    /// Appends an empty block with the next sequential ID.
    fn fresh_block(&mut self) -> BlockId {
        let id = BlockId::new(self.blocks.len().try_into().expect("block count must fit in u32"));
        self.blocks.push(BasicBlock::new(id, Vec::new(), Terminator::Todo));
        id
    }

    /// Adds an instruction to the current block.
    fn emit(&mut self, location: Offset) {
        let block = self.ensure_current();
        self.blocks[block.index()].instructions.push(Instruction::new(location));
    }

    /// Adds an unconditional edge only when the source block has not already been terminated.
    fn jump_if_todo(&mut self, from: BlockId, target: BlockId, location: Offset) {
        if matches!(self.blocks[from.index()].terminator, Terminator::Todo) {
            self.set_terminator(from, Terminator::Jump { target, location });
        }
    }

    /// Replaces a block's construction placeholder with its final terminator.
    fn set_terminator(&mut self, block: BlockId, terminator: Terminator) {
        debug_assert!(matches!(self.blocks[block.index()].terminator, Terminator::Todo));
        self.blocks[block.index()].terminator = terminator;
    }
}

impl<'pr> Visit<'pr> for Builder {
    fn visit_branch_node_enter(&mut self, node: Node<'pr>) {
        let emit = !matches!(
            node,
            Node::ProgramNode { .. }
                | Node::StatementsNode { .. }
                | Node::ArgumentsNode { .. }
                | Node::ElseNode { .. }
                | Node::IfNode { .. }
                | Node::UnlessNode { .. }
                | Node::WhileNode { .. }
                | Node::UntilNode { .. }
                | Node::ReturnNode { .. }
                | Node::ParenthesesNode { .. }
        );
        self.branch_stack
            .push((Offset::from_prism_location(&node.location()), emit));
    }

    fn visit_branch_node_leave(&mut self) {
        let (location, emit) = self.branch_stack.pop().expect("branch visitor stack must be balanced");
        if emit {
            self.emit(location);
        }
    }

    fn visit_leaf_node_enter(&mut self, node: Node<'pr>) {
        self.emit(Offset::from_prism_location(&node.location()));
    }

    fn visit_statements_node(&mut self, node: &ruby_prism::StatementsNode<'pr>) {
        for statement in &node.body() {
            self.ensure_current();
            self.visit(&statement);
        }
    }

    fn visit_if_node(&mut self, node: &ruby_prism::IfNode<'pr>) {
        self.visit_if(
            &node.predicate(),
            node.statements(),
            node.subsequent(),
            &node.location(),
            false,
        );
    }

    fn visit_unless_node(&mut self, node: &ruby_prism::UnlessNode<'pr>) {
        self.visit_if(
            &node.predicate(),
            node.statements(),
            node.else_clause().map(|node| node.as_node()),
            &node.location(),
            true,
        );
    }

    fn visit_while_node(&mut self, node: &ruby_prism::WhileNode<'pr>) {
        self.visit_loop(&node.predicate(), node.statements(), &node.location(), false);
    }

    fn visit_until_node(&mut self, node: &ruby_prism::UntilNode<'pr>) {
        self.visit_loop(&node.predicate(), node.statements(), &node.location(), true);
    }

    fn visit_return_node(&mut self, node: &ruby_prism::ReturnNode<'pr>) {
        if let Some(arguments) = node.arguments() {
            self.visit(&arguments.as_node());
        }
        let current = self.ensure_current();
        self.emit(Offset::from_prism_location(&node.location()));
        self.set_terminator(
            current,
            Terminator::Jump {
                target: BlockId::EXIT,
                location: Offset::from_prism_location(&node.location()),
            },
        );
        self.current = None;
    }

    fn visit_parentheses_node(&mut self, node: &ruby_prism::ParenthesesNode<'pr>) {
        if let Some(body) = node.body() {
            self.visit(&body);
        }
    }
}

#[cfg(test)]
mod tests {
    use std::fmt::Write;

    use super::{ControlFlowGraph, Terminator, build_cfg as build_cfg_from_node};

    struct BuiltCfg<'a> {
        source: &'a str,
        graph: ControlFlowGraph,
    }

    fn build_cfg(source: &str) -> BuiltCfg<'_> {
        let parsed = ruby_prism::parse(source.as_bytes());
        assert!(parsed.errors().next().is_none(), "test fixture must contain valid Ruby");
        BuiltCfg {
            source,
            graph: build_cfg_from_node(&parsed.node()),
        }
    }

    fn render_cfg(source: &str, cfg: &ControlFlowGraph) -> String {
        let mut output = String::new();
        for block in cfg.blocks() {
            writeln!(output, "{}:", block.id()).unwrap();
            if block.instructions().is_empty() {
                writeln!(output, "  instructions: []").unwrap();
            } else {
                writeln!(output, "  instructions:").unwrap();
                for instruction in block.instructions() {
                    let location = instruction.location();
                    let instruction_source = source[location.start() as usize..location.end() as usize].trim();
                    writeln!(output, "    {instruction_source}").unwrap();
                }
            }
            match block.terminator() {
                Terminator::Todo => writeln!(output, "  terminator: todo").unwrap(),
                Terminator::Jump { target, .. } => {
                    writeln!(output, "  terminator: jump {target}").unwrap();
                }
                Terminator::Branch {
                    then_block, else_block, ..
                } => {
                    writeln!(output, "  terminator: branch true={then_block} false={else_block}").unwrap();
                }
                Terminator::Exit => writeln!(output, "  terminator: exit").unwrap(),
            }
            output.push('\n');
        }
        output.trim_end().to_string()
    }

    fn normalize_expected(expected: &str) -> String {
        let lines = expected.lines().collect::<Vec<_>>();
        let first = lines.iter().position(|line| !line.trim().is_empty()).unwrap_or(0);
        let last = lines
            .iter()
            .rposition(|line| !line.trim().is_empty())
            .map_or(first, |last| last + 1);
        let lines = &lines[first..last];
        let indentation = lines
            .iter()
            .filter(|line| !line.trim().is_empty())
            .map(|line| line.len() - line.trim_start().len())
            .min()
            .unwrap_or(0);
        lines
            .iter()
            .map(|line| line.get(indentation..).unwrap_or_default())
            .collect::<Vec<_>>()
            .join("\n")
    }

    macro_rules! assert_cfg {
        ($cfg:expr, $expected:expr) => {{
            let cfg = &$cfg;
            assert_eq!(render_cfg(cfg.source, &cfg.graph), normalize_expected($expected));
        }};
    }

    #[test]
    fn builds_a_cfg_for_statements_node() {
        let cfg = build_cfg({
            "
            first
            second
            third
            "
        });
        assert_cfg!(
            cfg,
            "
            bb0:
              instructions:
                first
                second
                third
              terminator: jump bb1

            bb1:
              instructions: []
              terminator: exit
            "
        );
    }

    #[test]
    fn builds_a_cfg_for_if_node() {
        let cfg = build_cfg({
            "
            if condition
              left_one
              left_two
            else
              right_one
              right_two
            end
            "
        });
        assert_cfg!(
            cfg,
            "
            bb0:
              instructions:
                condition
              terminator: branch true=bb2 false=bb3

            bb1:
              instructions: []
              terminator: exit

            bb2:
              instructions:
                left_one
                left_two
              terminator: jump bb4

            bb3:
              instructions:
                right_one
                right_two
              terminator: jump bb4

            bb4:
              instructions: []
              terminator: jump bb1
            "
        );
    }

    #[test]
    fn builds_a_cfg_for_unless_node() {
        let cfg = build_cfg({
            "
            unless condition
              body_one
              body_two
            else
              alternate_one
              alternate_two
            end
            "
        });
        assert_cfg!(
            cfg,
            "
            bb0:
              instructions:
                condition
              terminator: branch true=bb3 false=bb2

            bb1:
              instructions: []
              terminator: exit

            bb2:
              instructions:
                body_one
                body_two
              terminator: jump bb4

            bb3:
              instructions:
                alternate_one
                alternate_two
              terminator: jump bb4

            bb4:
              instructions: []
              terminator: jump bb1
            "
        );
    }

    #[test]
    fn builds_a_cfg_for_while_node() {
        let cfg = build_cfg({
            "
            while condition
              work_one
              work_two
            end
            "
        });
        assert_cfg!(
            cfg,
            "
            bb0:
              instructions: []
              terminator: jump bb2

            bb1:
              instructions: []
              terminator: exit

            bb2:
              instructions:
                condition
              terminator: branch true=bb3 false=bb4

            bb3:
              instructions:
                work_one
                work_two
              terminator: jump bb2

            bb4:
              instructions: []
              terminator: jump bb1
            "
        );
    }

    #[test]
    fn builds_a_cfg_for_until_node() {
        let cfg = build_cfg({
            "
            until condition
              work_one
              work_two
            end
            "
        });
        assert_cfg!(
            cfg,
            "
            bb0:
              instructions: []
              terminator: jump bb2

            bb1:
              instructions: []
              terminator: exit

            bb2:
              instructions:
                condition
              terminator: branch true=bb4 false=bb3

            bb3:
              instructions:
                work_one
                work_two
              terminator: jump bb2

            bb4:
              instructions: []
              terminator: jump bb1
            "
        );
    }

    #[test]
    fn builds_a_cfg_for_while_node_with_a_branching_predicate() {
        let cfg = build_cfg({
            "
            while (if condition
              left
            else
              right
            end)
              work
            end
            "
        });
        assert_cfg!(
            cfg,
            "
            bb0:
              instructions: []
              terminator: jump bb2

            bb1:
              instructions: []
              terminator: exit

            bb2:
              instructions:
                condition
              terminator: branch true=bb5 false=bb6

            bb3:
              instructions:
                work
              terminator: jump bb2

            bb4:
              instructions: []
              terminator: jump bb1

            bb5:
              instructions:
                left
              terminator: jump bb7

            bb6:
              instructions:
                right
              terminator: jump bb7

            bb7:
              instructions: []
              terminator: branch true=bb3 false=bb4
            "
        );
    }

    #[test]
    fn builds_a_cfg_for_return_node() {
        let cfg = build_cfg({
            "
            prepare
            return value
            "
        });
        assert_cfg!(
            cfg,
            "
            bb0:
              instructions:
                prepare
                value
                return value
              terminator: jump bb1

            bb1:
              instructions: []
              terminator: exit
            "
        );
    }

    #[test]
    fn builds_a_cfg_for_parentheses_node() {
        let cfg = build_cfg({
            "
            (
              first
              second
            )
            "
        });
        assert_cfg!(
            cfg,
            "
            bb0:
              instructions:
                first
                second
              terminator: jump bb1

            bb1:
              instructions: []
              terminator: exit
            "
        );
    }
}
