//! Control-flow graph topology for Ruby method bodies.
//!
//! A control-flow graph (CFG) models every path execution can take through a
//! method body. Each node is a [`BasicBlock`]: a straight-line sequence of
//! [`Instruction`]s that always execute together. A block ends with one
//! [`Terminator`], which transfers control to its successor blocks.

pub mod builder;

use std::fmt;

use crate::offset::Offset;

/// Index of a basic block inside a [`ControlFlowGraph`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct BlockId(u32);

impl BlockId {
    /// The unique entry block. Method execution starts here.
    pub const ENTRY: Self = Self(0);
    /// The unique exit block. Every terminating path eventually reaches it.
    pub const EXIT: Self = Self(1);

    #[must_use]
    pub const fn new(value: u32) -> Self {
        Self(value)
    }

    #[must_use]
    pub const fn get(self) -> u32 {
        self.0
    }

    fn index(self) -> usize {
        self.0 as usize
    }
}

impl fmt::Display for BlockId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "bb{}", self.0)
    }
}

/// An instruction whose semantics have not been lowered yet.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Instruction {
    /// The corresponding byte range in the Ruby source.
    location: Offset,
}

impl Instruction {
    #[must_use]
    pub const fn new(location: Offset) -> Self {
        Self { location }
    }

    #[must_use]
    pub const fn location(&self) -> &Offset {
        &self.location
    }
}

/// The control-flow instruction that ends a basic block.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Terminator {
    /// Construction placeholder for a block whose terminator has not been set.
    /// The builder replaces it before returning a completed graph.
    Todo,
    /// Transfers control unconditionally to one successor.
    Jump {
        /// The block that executes next.
        target: BlockId,
        /// The Ruby source range responsible for the transfer.
        location: Offset,
    },
    /// Selects one of two successors. The branch condition will be introduced
    /// with instruction semantics in a later layer.
    Branch {
        /// The successor used when the condition is truthy.
        then_block: BlockId,
        /// The successor used when the condition is false or `nil`.
        else_block: BlockId,
        /// The Ruby source range responsible for the branch.
        location: Offset,
    },
    /// Ends method execution. Only [`BlockId::EXIT`] may use this terminator.
    Exit,
}

impl Terminator {
    pub fn successors(&self) -> impl Iterator<Item = BlockId> {
        let successors = match self {
            Self::Todo | Self::Exit => [None, None],
            Self::Jump { target, .. } => [Some(*target), None],
            Self::Branch {
                then_block, else_block, ..
            } => [Some(*then_block), (*then_block != *else_block).then_some(*else_block)],
        };
        successors.into_iter().flatten()
    }
}

/// A maximal straight-line instruction sequence with one terminator.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BasicBlock {
    /// Position of this block in its CFG's block array.
    id: BlockId,
    /// Instructions that execute in source order when control enters the block.
    instructions: Vec<Instruction>,
    /// The control-flow instruction that ends the block.
    terminator: Terminator,
}

impl BasicBlock {
    #[must_use]
    pub fn new(id: BlockId, instructions: Vec<Instruction>, terminator: Terminator) -> Self {
        Self {
            id,
            instructions,
            terminator,
        }
    }

    #[must_use]
    pub const fn id(&self) -> BlockId {
        self.id
    }

    #[must_use]
    pub fn instructions(&self) -> &[Instruction] {
        &self.instructions
    }

    #[must_use]
    pub const fn terminator(&self) -> &Terminator {
        &self.terminator
    }
}

/// A method's basic blocks and the control-flow edges between them.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ControlFlowGraph {
    blocks: Vec<BasicBlock>,
}

impl ControlFlowGraph {
    /// Creates a CFG from its blocks.
    #[must_use]
    pub fn new(blocks: Vec<BasicBlock>) -> Self {
        Self { blocks }
    }

    #[must_use]
    pub fn blocks(&self) -> &[BasicBlock] {
        &self.blocks
    }

    #[must_use]
    pub fn block(&self, id: BlockId) -> Option<&BasicBlock> {
        self.blocks.get(id.index())
    }

    #[must_use]
    pub fn entry(&self) -> &BasicBlock {
        &self.blocks[BlockId::ENTRY.index()]
    }

    #[must_use]
    pub fn exit(&self) -> &BasicBlock {
        &self.blocks[BlockId::EXIT.index()]
    }
}
