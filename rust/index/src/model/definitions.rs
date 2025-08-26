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

use serde::{Deserialize, Serialize};

use crate::offset::Offset;

#[derive(Debug, Serialize, Deserialize)]
pub enum Definition {
    Class(Box<ClassDefinition>),
    Module(Box<ModuleDefinition>),
    Constant(Box<ConstantDefinition>),
    Method(Box<MethodDefinition>),
    AttrAccessor(Box<AttrAccessorDefinition>),
    AttrReader(Box<AttrReaderDefinition>),
    AttrWriter(Box<AttrWriterDefinition>),
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
            Definition::AttrWriter(it) => it.offset.start(),
            Definition::Method(it) => it.offset.start(),
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
            Definition::AttrWriter(it) => it.offset.end(),
            Definition::Method(it) => it.offset.end(),
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
            Definition::AttrWriter(_) => 8,
            Definition::Method(_) => 9,
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
#[derive(Debug, Serialize, Deserialize)]
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
#[derive(Debug, Serialize, Deserialize)]
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
#[derive(Debug, Serialize, Deserialize)]
pub struct ConstantDefinition {
    offset: Offset,
}

impl ConstantDefinition {
    #[must_use]
    pub const fn new(offset: Offset) -> Self {
        Self { offset }
    }
}

/// A method definition
///
/// # Example
/// ```ruby
/// def foo(bar, baz)
/// end
/// ```
#[derive(Debug, Serialize, Deserialize)]
pub struct MethodDefinition {
    offset: Offset,
    parameters: Vec<Parameter>,
    is_singleton: bool,
}

impl MethodDefinition {
    #[must_use]
    pub const fn new(offset: Offset, parameters: Vec<Parameter>, is_singleton: bool) -> Self {
        Self {
            offset,
            parameters,
            is_singleton,
        }
    }

    #[must_use]
    pub fn parameters(&self) -> &Vec<Parameter> {
        &self.parameters
    }

    #[must_use]
    pub fn is_singleton(&self) -> bool {
        self.is_singleton
    }
}

#[derive(Debug, Serialize, Deserialize)]
pub enum Parameter {
    RequiredPositional(ParameterStruct),
    OptionalPositional(ParameterStruct),
    RestPositional(ParameterStruct),
    Post(ParameterStruct),
    RequiredKeyword(ParameterStruct),
    OptionalKeyword(ParameterStruct),
    RestKeyword(ParameterStruct),
    Block(ParameterStruct),
}

#[derive(Debug, Serialize, Deserialize)]
pub struct ParameterStruct {
    offset: Offset,
    name: String,
}

impl ParameterStruct {
    #[must_use]
    pub const fn new(offset: Offset, name: String) -> Self {
        Self { offset, name }
    }

    #[must_use]
    pub fn offset(&self) -> &Offset {
        &self.offset
    }

    #[must_use]
    pub fn name(&self) -> &str {
        &self.name
    }
}

/// An attr accessor definition
///
/// # Example
/// ```ruby
/// attr_accessor :foo
/// ```
#[derive(Debug, Serialize, Deserialize)]
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
#[derive(Debug, Serialize, Deserialize)]
pub struct AttrReaderDefinition {
    offset: Offset,
}

impl AttrReaderDefinition {
    #[must_use]
    pub const fn new(offset: Offset) -> Self {
        Self { offset }
    }
}

/// An attr writer definition
///
/// # Example
/// ```ruby
/// attr_writer :foo
/// ```
#[derive(Debug, Serialize, Deserialize)]
pub struct AttrWriterDefinition {
    offset: Offset,
}

impl AttrWriterDefinition {
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
#[derive(Debug, Serialize, Deserialize)]
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
#[derive(Debug, Serialize, Deserialize)]
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
#[derive(Debug, Serialize, Deserialize)]
pub struct ClassVariableDefinition {
    offset: Offset,
}

impl ClassVariableDefinition {
    #[must_use]
    pub const fn new(offset: Offset) -> Self {
        Self { offset }
    }
}
