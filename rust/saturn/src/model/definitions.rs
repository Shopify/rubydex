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
    model::{
        comment::Comment,
        ids::{DeclarationId, UriId},
    },
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
    pub fn declaration_id(&self) -> &DeclarationId {
        all_definitions!(self, it => &it.declaration_id)
    }

    #[must_use]
    pub fn uri_id(&self) -> &UriId {
        all_definitions!(self, it => &it.uri_id)
    }

    #[must_use]
    pub fn comments(&self) -> &Vec<Comment> {
        all_definitions!(self, it => &it.comments)
    }

    /// Serializes the definition into the byte vector we store in the database
    ///
    /// # Panics
    ///
    /// This method will only panic if serialization fails, which should never happen
    #[must_use]
    pub fn serialize(&self) -> Vec<u8> {
        rmp_serde::to_vec(self).expect("Serializing document should always succeed")
    }

    /// # Panics
    ///
    /// This method will only panic if serialization fails, which should never happen unless there's corrupt data stored
    /// in the database
    #[must_use]
    pub fn deserialize(data: &[u8]) -> Self {
        rmp_serde::from_slice(data).expect("Deserializing document should always succeed")
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
    declaration_id: DeclarationId,
    uri_id: UriId,
    offset: Offset,
    comments: Vec<Comment>,
}

impl ClassDefinition {
    #[must_use]
    pub const fn new(declaration_id: DeclarationId, uri_id: UriId, offset: Offset, comments: Vec<Comment>) -> Self {
        Self {
            declaration_id,
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
    declaration_id: DeclarationId,
    uri_id: UriId,
    offset: Offset,
    comments: Vec<Comment>,
}

impl ModuleDefinition {
    #[must_use]
    pub const fn new(declaration_id: DeclarationId, uri_id: UriId, offset: Offset, comments: Vec<Comment>) -> Self {
        Self {
            declaration_id,
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
    declaration_id: DeclarationId,
    uri_id: UriId,
    offset: Offset,
    comments: Vec<Comment>,
}

impl ConstantDefinition {
    #[must_use]
    pub const fn new(declaration_id: DeclarationId, uri_id: UriId, offset: Offset, comments: Vec<Comment>) -> Self {
        Self {
            declaration_id,
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
    declaration_id: DeclarationId,
    uri_id: UriId,
    offset: Offset,
    parameters: Vec<Parameter>,
    is_singleton: bool,
    comments: Vec<Comment>,
}

impl MethodDefinition {
    #[must_use]
    pub const fn new(
        declaration_id: DeclarationId,
        uri_id: UriId,
        offset: Offset,
        parameters: Vec<Parameter>,
        is_singleton: bool,
        comments: Vec<Comment>,
    ) -> Self {
        Self {
            declaration_id,
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
    declaration_id: DeclarationId,
    uri_id: UriId,
    offset: Offset,
    comments: Vec<Comment>,
}

impl AttrAccessorDefinition {
    #[must_use]
    pub const fn new(declaration_id: DeclarationId, uri_id: UriId, offset: Offset, comments: Vec<Comment>) -> Self {
        Self {
            declaration_id,
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
    declaration_id: DeclarationId,
    uri_id: UriId,
    offset: Offset,
    comments: Vec<Comment>,
}

impl AttrReaderDefinition {
    #[must_use]
    pub const fn new(declaration_id: DeclarationId, uri_id: UriId, offset: Offset, comments: Vec<Comment>) -> Self {
        Self {
            declaration_id,
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
    declaration_id: DeclarationId,
    uri_id: UriId,
    offset: Offset,
    comments: Vec<Comment>,
}

impl AttrWriterDefinition {
    #[must_use]
    pub const fn new(declaration_id: DeclarationId, uri_id: UriId, offset: Offset, comments: Vec<Comment>) -> Self {
        Self {
            declaration_id,
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
    declaration_id: DeclarationId,
    uri_id: UriId,
    pub offset: Offset,
    comments: Vec<Comment>,
}

impl GlobalVariableDefinition {
    #[must_use]
    pub const fn new(declaration_id: DeclarationId, uri_id: UriId, offset: Offset, comments: Vec<Comment>) -> Self {
        Self {
            declaration_id,
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
    declaration_id: DeclarationId,
    uri_id: UriId,
    offset: Offset,
    comments: Vec<Comment>,
}

impl InstanceVariableDefinition {
    #[must_use]
    pub const fn new(declaration_id: DeclarationId, uri_id: UriId, offset: Offset, comments: Vec<Comment>) -> Self {
        Self {
            declaration_id,
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
    declaration_id: DeclarationId,
    uri_id: UriId,
    offset: Offset,
    comments: Vec<Comment>,
}

impl ClassVariableDefinition {
    #[must_use]
    pub const fn new(declaration_id: DeclarationId, uri_id: UriId, offset: Offset, comments: Vec<Comment>) -> Self {
        Self {
            declaration_id,
            uri_id,
            offset,
            comments,
        }
    }
}
