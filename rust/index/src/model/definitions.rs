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

macro_rules! all_definitions {
    ($value:expr, $var:ident => $expr:expr) => {
        match $value {
            Definition::Class($var) => $expr,
            Definition::Module($var) => $expr,
            Definition::Constant($var) => $expr,
            Definition::GlobalVariable($var) => $expr,
            Definition::InstanceVariable($var) => $expr,
            Definition::ClassVariable($var) => $expr,
            Definition::AttrAccessor($var) => $expr,
            Definition::AttrReader($var) => $expr,
            Definition::AttrWriter($var) => $expr,
            Definition::Method($var) => $expr,
        }
    };
}

impl Definition {
    #[must_use]
    pub fn start(&self) -> u32 {
        all_definitions!(self, it => it.offset.start())
    }

    #[must_use]
    pub fn end(&self) -> u32 {
        all_definitions!(self, it => it.offset.end())
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
        all_definitions!(self, it => &it.name_id)
    }

    #[must_use]
    pub fn uri_id(&self) -> &UriId {
        all_definitions!(self, it => &it.uri_id)
    }

    #[must_use]
    pub fn comments(&self) -> Option<&[String]> {
        all_definitions!(self, it => it.comments.as_deref())
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
    comments: Option<Box<[String]>>,
}

impl ClassDefinition {
    #[must_use]
    pub fn new(name_id: NameId, uri_id: UriId, offset: Offset, comments: Option<Vec<String>>) -> Self {
        Self {
            name_id,
            uri_id,
            offset,
            comments: comments.map(|c| c.into_boxed_slice()),
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
    comments: Option<Box<[String]>>,
}

impl ModuleDefinition {
    #[must_use]
    pub fn new(name_id: NameId, uri_id: UriId, offset: Offset, comments: Option<Vec<String>>) -> Self {
        Self {
            name_id,
            uri_id,
            offset,
            comments: comments.map(|c| c.into_boxed_slice()),
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
    comments: Option<Box<[String]>>,
}

impl ConstantDefinition {
    #[must_use]
    pub fn new(name_id: NameId, uri_id: UriId, offset: Offset, comments: Option<Vec<String>>) -> Self {
        Self {
            name_id,
            uri_id,
            offset,
            comments: comments.map(|c| c.into_boxed_slice()),
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
    comments: Option<Box<[String]>>,
}

impl MethodDefinition {
    #[must_use]
    pub fn new(
        name_id: NameId,
        uri_id: UriId,
        offset: Offset,
        parameters: Vec<Parameter>,
        is_singleton: bool,
        comments: Option<Vec<String>>,
    ) -> Self {
        Self {
            name_id,
            uri_id,
            offset,
            parameters,
            is_singleton,
            comments: comments.map(|c| c.into_boxed_slice()),
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
    comments: Option<Box<[String]>>,
}

impl AttrAccessorDefinition {
    #[must_use]
    pub fn new(name_id: NameId, uri_id: UriId, offset: Offset, comments: Option<Vec<String>>) -> Self {
        Self {
            name_id,
            uri_id,
            offset,
            comments: comments.map(|c| c.into_boxed_slice()),
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
    comments: Option<Box<[String]>>,
}

impl AttrReaderDefinition {
    #[must_use]
    pub fn new(name_id: NameId, uri_id: UriId, offset: Offset, comments: Option<Vec<String>>) -> Self {
        Self {
            name_id,
            uri_id,
            offset,
            comments: comments.map(|c| c.into_boxed_slice()),
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
    comments: Option<Box<[String]>>,
}

impl AttrWriterDefinition {
    #[must_use]
    pub fn new(name_id: NameId, uri_id: UriId, offset: Offset, comments: Option<Vec<String>>) -> Self {
        Self {
            name_id,
            uri_id,
            offset,
            comments: comments.map(|c| c.into_boxed_slice()),
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
    comments: Option<Box<[String]>>,
}

impl GlobalVariableDefinition {
    #[must_use]
    pub fn new(name_id: NameId, uri_id: UriId, offset: Offset, comments: Option<Vec<String>>) -> Self {
        Self {
            name_id,
            uri_id,
            offset,
            comments: comments.map(|c| c.into_boxed_slice()),
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
    comments: Option<Box<[String]>>,
}

impl InstanceVariableDefinition {
    #[must_use]
    pub fn new(name_id: NameId, uri_id: UriId, offset: Offset, comments: Option<Vec<String>>) -> Self {
        Self {
            name_id,
            uri_id,
            offset,
            comments: comments.map(|c| c.into_boxed_slice()),
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
    comments: Option<Box<[String]>>,
}

impl ClassVariableDefinition {
    #[must_use]
    pub fn new(name_id: NameId, uri_id: UriId, offset: Offset, comments: Option<Vec<String>>) -> Self {
        Self {
            name_id,
            uri_id,
            offset,
            comments: comments.map(|c| c.into_boxed_slice()),
        }
    }
}
