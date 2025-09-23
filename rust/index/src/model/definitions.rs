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

use crate::{
    model::ids::{NameId, UriId},
    offset::Offset,
};

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

    #[must_use]
    pub fn kind(&self) -> &'static str {
        match self {
            Definition::Class(_) => "Class",
            Definition::Module(_) => "Module",
            Definition::Constant(_) => "Constant",
            Definition::Method(_) => "Method",
            Definition::AttrAccessor(_) => "AttrAccessor",
            Definition::AttrReader(_) => "AttrReader",
            Definition::AttrWriter(_) => "AttrWriter",
            Definition::GlobalVariable(_) => "GlobalVariable",
            Definition::InstanceVariable(_) => "InstanceVariable",
            Definition::ClassVariable(_) => "ClassVariable",
        }
    }

    #[must_use]
    pub fn name_id(&self) -> &NameId {
        match self {
            Definition::Class(it) => &it.name_id,
            Definition::Module(it) => &it.name_id,
            Definition::Constant(it) => &it.name_id,
            Definition::GlobalVariable(it) => &it.name_id,
            Definition::InstanceVariable(it) => &it.name_id,
            Definition::ClassVariable(it) => &it.name_id,
            Definition::AttrAccessor(it) => &it.name_id,
            Definition::AttrReader(it) => &it.name_id,
            Definition::AttrWriter(it) => &it.name_id,
            Definition::Method(it) => &it.name_id,
        }
    }

    #[must_use]
    pub fn uri_id(&self) -> &UriId {
        match self {
            Definition::Class(it) => &it.uri_id,
            Definition::Module(it) => &it.uri_id,
            Definition::Constant(it) => &it.uri_id,
            Definition::GlobalVariable(it) => &it.uri_id,
            Definition::InstanceVariable(it) => &it.uri_id,
            Definition::ClassVariable(it) => &it.uri_id,
            Definition::AttrAccessor(it) => &it.uri_id,
            Definition::AttrReader(it) => &it.uri_id,
            Definition::AttrWriter(it) => &it.uri_id,
            Definition::Method(it) => &it.uri_id,
        }
    }

    #[must_use]
    pub fn comments(&self) -> &str {
        match self {
            Definition::Class(it) => &it.comments,
            Definition::Module(it) => &it.comments,
            Definition::Constant(it) => &it.comments,
            Definition::Method(it) => &it.comments,
            Definition::AttrAccessor(it) => &it.comments,
            Definition::AttrReader(it) => &it.comments,
            Definition::AttrWriter(it) => &it.comments,
            Definition::GlobalVariable(it) => &it.comments,
            Definition::InstanceVariable(it) => &it.comments,
            Definition::ClassVariable(it) => &it.comments,
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
    name_id: NameId,
    uri_id: UriId,
    offset: Offset,
    comments: String,
}

impl ClassDefinition {
    #[must_use]
    pub const fn new(name_id: NameId, uri_id: UriId, offset: Offset, comments: String) -> Self {
        Self {
            name_id,
            uri_id,
            offset,
            comments,
        }
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
    name_id: NameId,
    uri_id: UriId,
    offset: Offset,
    comments: String,
}

impl ModuleDefinition {
    #[must_use]
    pub const fn new(name_id: NameId, uri_id: UriId, offset: Offset, comments: String) -> Self {
        Self {
            name_id,
            uri_id,
            offset,
            comments,
        }
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
    name_id: NameId,
    uri_id: UriId,
    offset: Offset,
    comments: String,
}

impl ConstantDefinition {
    #[must_use]
    pub const fn new(name_id: NameId, uri_id: UriId, offset: Offset, comments: String) -> Self {
        Self {
            name_id,
            uri_id,
            offset,
            comments,
        }
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
    name_id: NameId,
    uri_id: UriId,
    offset: Offset,
    parameters: Vec<Parameter>,
    is_singleton: bool,
    comments: String,
}

impl MethodDefinition {
    #[must_use]
    pub const fn new(
        name_id: NameId,
        uri_id: UriId,
        offset: Offset,
        parameters: Vec<Parameter>,
        is_singleton: bool,
        comments: String,
    ) -> Self {
        Self {
            name_id,
            uri_id,
            offset,
            parameters,
            is_singleton,
            comments,
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
    Forward(ParameterStruct),
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
    name_id: NameId,
    uri_id: UriId,
    offset: Offset,
    comments: String,
}

impl AttrAccessorDefinition {
    #[must_use]
    pub const fn new(name_id: NameId, uri_id: UriId, offset: Offset, comments: String) -> Self {
        Self {
            name_id,
            uri_id,
            offset,
            comments,
        }
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
    name_id: NameId,
    uri_id: UriId,
    offset: Offset,
    comments: String,
}

impl AttrReaderDefinition {
    #[must_use]
    pub const fn new(name_id: NameId, uri_id: UriId, offset: Offset, comments: String) -> Self {
        Self {
            name_id,
            uri_id,
            offset,
            comments,
        }
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
    name_id: NameId,
    uri_id: UriId,
    offset: Offset,
    comments: String,
}

impl AttrWriterDefinition {
    #[must_use]
    pub const fn new(name_id: NameId, uri_id: UriId, offset: Offset, comments: String) -> Self {
        Self {
            name_id,
            uri_id,
            offset,
            comments,
        }
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
    name_id: NameId,
    uri_id: UriId,
    pub offset: Offset,
    comments: String,
}

impl GlobalVariableDefinition {
    #[must_use]
    pub const fn new(name_id: NameId, uri_id: UriId, offset: Offset, comments: String) -> Self {
        Self {
            name_id,
            uri_id,
            offset,
            comments,
        }
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
    name_id: NameId,
    uri_id: UriId,
    offset: Offset,
    comments: String,
}

impl InstanceVariableDefinition {
    #[must_use]
    pub const fn new(name_id: NameId, uri_id: UriId, offset: Offset, comments: String) -> Self {
        Self {
            name_id,
            uri_id,
            offset,
            comments,
        }
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
    name_id: NameId,
    uri_id: UriId,
    offset: Offset,
    comments: String,
}

impl ClassVariableDefinition {
    #[must_use]
    pub const fn new(name_id: NameId, uri_id: UriId, offset: Offset, comments: String) -> Self {
        Self {
            name_id,
            uri_id,
            offset,
            comments,
        }
    }
}
