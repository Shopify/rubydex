use crate::model::ids::{DeclarationId, StringId, UriId};
use crate::offset::Offset;

/// Represents a potential DSL call captured during indexing.
///
/// Events form a tree structure via `parent_id`, enabling Ruby plugins
/// to reconstruct block hierarchy (e.g., RSpec describe/context nesting).
#[derive(Debug, Clone)]
pub struct DslEvent {
    /// Unique ID within this file (0-indexed, assigned during capture)
    pub id: usize,
    /// ID of the enclosing DSL call that has a block, or None if top-level
    pub parent_id: Option<usize>,
    /// The name of the method being called (e.g., "belongs_to", "let", "describe")
    pub method_name: StringId,
    /// The arguments as string representations (symbols/strings only)
    pub arguments: Vec<StringId>,
    /// The Ruby lexical nesting stack at the time of the call (DeclarationIds)
    pub nesting_stack: Vec<DeclarationId>,
    /// Source location (byte offset)
    pub offset: Offset,
    /// Whether this call has a block attached
    pub has_block: bool,
    /// The receiver of the call if present (e.g., "Foo" for `Foo.method`, "self" for `self.method`)
    pub receiver: Option<StringId>,
}

impl DslEvent {
    pub fn new(
        id: usize,
        parent_id: Option<usize>,
        method_name: StringId,
        arguments: Vec<StringId>,
        nesting_stack: Vec<DeclarationId>,
        offset: Offset,
        has_block: bool,
        receiver: Option<StringId>,
    ) -> Self {
        Self {
            id,
            parent_id,
            method_name,
            arguments,
            nesting_stack,
            offset,
            has_block,
            receiver,
        }
    }
}

/// Collection of DSL events for a single file
#[derive(Debug)]
pub struct FileDslEvents {
    pub uri_id: UriId,
    pub events: Vec<DslEvent>,
    next_id: usize,
}

impl Default for FileDslEvents {
    fn default() -> Self {
        Self {
            uri_id: UriId::new(0),
            events: Vec::new(),
            next_id: 0,
        }
    }
}

impl FileDslEvents {
    pub fn new(uri_id: UriId) -> Self {
        Self {
            uri_id,
            events: Vec::new(),
            next_id: 0,
        }
    }

    /// Allocate the next event ID
    pub fn next_id(&mut self) -> usize {
        let id = self.next_id;
        self.next_id += 1;
        id
    }

    pub fn push(&mut self, event: DslEvent) {
        self.events.push(event);
    }

    pub fn is_empty(&self) -> bool {
        self.events.is_empty()
    }
}
