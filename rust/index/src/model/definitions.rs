//! The definitions of the Ruby constructs in the source code.
//!
//! All the definitions constitute the `Declaration` of a name.
//!
//! Consider the following example:
//!
//! ```ruby
//! module Foo
//!   class Bar; end
//! end
//!
//! class Foo::Bar; end
//! ```
//!
//! There are 3 definitions:
//!
//! 1. The module definition for `Foo`
//! 2. The class definition for `Foo::Bar` inside `Foo`
//! 3. The class definition for `Foo::Bar` again
//!
//! And there are 2 declarations:
//!
//! 1. The declaration for the name `Foo`
//! 2. The declaration for the name `Foo::Bar`

use crate::offset::Offset;

#[derive(Debug)]
pub enum Definition {
    Class(Box<ClassDefinition>),
    Module(Box<ModuleDefinition>),
}

impl Definition {
    #[must_use]
    pub fn start_offset(&self) -> u32 {
        match self {
            Definition::Class(it) => it.offset.start_offset(),
            Definition::Module(it) => it.offset.start_offset(),
        }
    }
}

/// A class definition
///
/// # Example
/// ```ruby
/// class Foo
/// end
/// ```
#[derive(Debug)]
pub struct ClassDefinition {
    offset: Offset,
}

impl ClassDefinition {
    #[must_use]
    pub const fn new(offset: Offset) -> Self {
        Self { offset }
    }
}

/// A module definition
///
/// # Example
/// ```ruby
/// module Foo
/// end
/// ```
#[derive(Debug)]
pub struct ModuleDefinition {
    offset: Offset,
}

impl ModuleDefinition {
    #[must_use]
    pub const fn new(offset: Offset) -> Self {
        Self { offset }
    }
}
