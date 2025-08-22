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
    Constant(Box<ConstantDefinition>),
    AttrAccessor(Box<AttrAccessorDefinition>),
    AttrReader(Box<AttrReaderDefinition>),
    GlobalVariable(Box<GlobalVariableDefinition>),
    InstanceVariable(Box<InstanceVariableDefinition>),
    ClassVariable(Box<ClassVariableDefinition>),
}

impl Definition {
    #[must_use]
    pub fn start(&self) -> u32 {
        match self {
            Definition::Class(it) => it.offset.start(),
            Definition::Module(it) => it.offset.start(),
            Definition::Constant(it) => it.offset.start(),
            Definition::GlobalVariable(it) => it.offset.start(),
            Definition::InstanceVariable(it) => it.offset.start(),
            Definition::ClassVariable(it) => it.offset.start(),
            Definition::AttrAccessor(it) => it.offset.start(),
            Definition::AttrReader(it) => it.offset.start(),
        }
    }

    #[must_use]
    pub fn end(&self) -> u32 {
        match self {
            Definition::Class(it) => it.offset.end(),
            Definition::Module(it) => it.offset.end(),
            Definition::Constant(it) => it.offset.end(),
            Definition::GlobalVariable(it) => it.offset.end(),
            Definition::InstanceVariable(it) => it.offset.end(),
            Definition::ClassVariable(it) => it.offset.end(),
            Definition::AttrAccessor(it) => it.offset.end(),
            Definition::AttrReader(it) => it.offset.end(),
        }
    }

    // Mapping of a definition's type to the definition_type enum value in the DB.
    #[must_use]
    pub fn type_id(&self) -> u8 {
        // NOTE: update the schema.sql file to match when adding new types
        match self {
            Definition::Class(_) => 0,
            Definition::Module(_) => 1,
            Definition::Constant(_) => 2,
            Definition::GlobalVariable(_) => 3,
            Definition::InstanceVariable(_) => 4,
            Definition::ClassVariable(_) => 5,
            Definition::AttrAccessor(_) => 6,
            Definition::AttrReader(_) => 7,
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

/// A constant definition
///
/// # Example
/// ```ruby
/// FOO = 1
/// ```
#[derive(Debug)]
pub struct ConstantDefinition {
    offset: Offset,
}

impl ConstantDefinition {
    #[must_use]
    pub const fn new(offset: Offset) -> Self {
        Self { offset }
    }
}

/// An attr accessor definition
///
/// # Example
/// ```ruby
/// attr_accessor :foo
/// ```
#[derive(Debug)]
pub struct AttrAccessorDefinition {
    offset: Offset,
}

impl AttrAccessorDefinition {
    #[must_use]
    pub const fn new(offset: Offset) -> Self {
        Self { offset }
    }
}

/// An attr reader definition
///
/// # Example
/// ```ruby
/// attr_reader :foo
/// ```
#[derive(Debug)]
pub struct AttrReaderDefinition {
    offset: Offset,
}

impl AttrReaderDefinition {
    #[must_use]
    pub const fn new(offset: Offset) -> Self {
        Self { offset }
    }
}

/// A global variable definition
///
/// # Example
/// ```ruby
/// $foo = 1
/// ```
#[derive(Debug)]
pub struct GlobalVariableDefinition {
    pub offset: Offset,
}

impl GlobalVariableDefinition {
    #[must_use]
    pub const fn new(offset: Offset) -> Self {
        Self { offset }
    }
}

/// An instance variable definition
///
/// # Example
/// ```ruby
/// @foo = 1
/// ```
#[derive(Debug)]
pub struct InstanceVariableDefinition {
    offset: Offset,
}

impl InstanceVariableDefinition {
    #[must_use]
    pub const fn new(offset: Offset) -> Self {
        Self { offset }
    }
}

/// A class variable definition
///
/// # Example
/// ```ruby
/// @@foo = 1
/// ```
#[derive(Debug)]
pub struct ClassVariableDefinition {
    offset: Offset,
}

impl ClassVariableDefinition {
    #[must_use]
    pub const fn new(offset: Offset) -> Self {
        Self { offset }
    }
}
