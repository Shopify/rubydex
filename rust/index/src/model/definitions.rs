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
use crate::pools::name_pool::NameId;
use crate::pools::uri_pool::UriPool;

/// The definition of a Ruby construct in the source code.
///
/// This is the root of the definition tree.
/// See `Definition`'s variants for more details.
#[derive(Debug)]
pub enum Definition {
    Class(Box<ClassDefinition>),
    Module(Box<ModuleDefinition>),
    SingletonClass(Box<SingletonClassDefinition>),
    Constant(Box<ConstantDefinition>),
    Method(Box<MethodDefinition>),
    AttrAccessor(Box<AttrAccessorDefinition>),
    AttrReader(Box<AttrReaderDefinition>),
    AttrWriter(Box<AttrWriterDefinition>),
}

impl Definition {
    #[must_use]
    pub fn as_class(&self) -> Option<&ClassDefinition> {
        match self {
            Self::Class(class) => Some(class),
            _ => None,
        }
    }

    #[must_use]
    pub fn as_module(&self) -> Option<&ModuleDefinition> {
        match self {
            Self::Module(module) => Some(module),
            _ => None,
        }
    }

    #[must_use]
    pub fn as_singleton_class(&self) -> Option<&SingletonClassDefinition> {
        match self {
            Self::SingletonClass(singleton) => Some(singleton),
            _ => None,
        }
    }

    #[must_use]
    pub fn as_constant(&self) -> Option<&ConstantDefinition> {
        match self {
            Self::Constant(constant) => Some(constant),
            _ => None,
        }
    }

    #[must_use]
    pub fn as_method(&self) -> Option<&MethodDefinition> {
        match self {
            Self::Method(method) => Some(method),
            _ => None,
        }
    }

    #[must_use]
    pub fn as_attr_accessor(&self) -> Option<&AttrAccessorDefinition> {
        match self {
            Self::AttrAccessor(attr_accessor) => Some(attr_accessor),
            _ => None,
        }
    }

    #[must_use]
    pub fn as_attr_reader(&self) -> Option<&AttrReaderDefinition> {
        match self {
            Self::AttrReader(attr_reader) => Some(attr_reader),
            _ => None,
        }
    }

    #[must_use]
    pub fn as_attr_writer(&self) -> Option<&AttrWriterDefinition> {
        match self {
            Self::AttrWriter(attr_writer) => Some(attr_writer),
            _ => None,
        }
    }
}

/// The definition of a class in the source code.
///
/// A class can have multiple definitions for the same name.
#[derive(Debug)]
pub struct ClassDefinition {
    pub offset: Offset,
    pub superclass: Option<NameId>,
}

impl ClassDefinition {
    #[must_use]
    pub const fn new(offset: Offset, superclass: Option<NameId>) -> Self {
        Self { offset, superclass }
    }
}

/// The definition of a module in the source code.
///
/// A module can have multiple definitions for the same name.
#[derive(Debug)]
pub struct ModuleDefinition {
    pub offset: Offset,
}

impl ModuleDefinition {
    #[must_use]
    pub const fn new(offset: Offset) -> Self {
        Self { offset }
    }
}

/// The definition of a singleton class in the source code.
#[derive(Debug)]
pub struct SingletonClassDefinition {
    pub offset: Offset,
}

impl SingletonClassDefinition {
    #[must_use]
    pub const fn new(offset: Offset) -> Self {
        Self { offset }
    }
}

/// The definition of a constant in the source code.
///
/// This represent where a constant is being assigned.
#[derive(Debug)]
pub struct ConstantDefinition {
    pub offset: Offset,
}

impl ConstantDefinition {
    #[must_use]
    pub const fn new(offset: Offset) -> Self {
        Self { offset }
    }
}

#[derive(Debug)]
pub struct MethodDefinition {
    pub offset: Offset,
    pub parameters: Vec<Parameter>,
    pub is_singleton: bool,
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
}

#[derive(Debug)]
pub struct Parameter {
    pub name_id: NameId,
    pub offset: Offset,
    pub kind: ParameterKind,
}

impl Parameter {
    #[must_use]
    pub const fn new(name_id: NameId, offset: Offset, kind: ParameterKind) -> Self {
        Self { name_id, offset, kind }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum ParameterKind {
    RequiredPositional,
    OptionalPositional,
    RestPositional,
    Post,
    RequiredKeyword,
    OptionalKeyword,
    RestKeyword,
    Block,
}

impl std::fmt::Display for ParameterKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::RequiredPositional => write!(f, "required positional"),
            Self::OptionalPositional => write!(f, "optional positional"),
            Self::RestPositional => write!(f, "rest positional"),
            Self::Post => write!(f, "post positional"),
            Self::RequiredKeyword => write!(f, "required keyword"),
            Self::OptionalKeyword => write!(f, "optional keyword"),
            Self::RestKeyword => write!(f, "rest keyword"),
            Self::Block => write!(f, "block"),
        }
    }
}

#[derive(Debug)]
pub struct AttrAccessorDefinition {
    pub offset: Offset,
}

impl AttrAccessorDefinition {
    #[must_use]
    pub const fn new(offset: Offset) -> Self {
        Self { offset }
    }
}

#[derive(Debug)]
pub struct AttrReaderDefinition {
    pub offset: Offset,
}

impl AttrReaderDefinition {
    #[must_use]
    pub const fn new(offset: Offset) -> Self {
        Self { offset }
    }
}

#[derive(Debug)]
pub struct AttrWriterDefinition {
    pub offset: Offset,
}

impl AttrWriterDefinition {
    #[must_use]
    pub const fn new(offset: Offset) -> Self {
        Self { offset }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::pools::name_pool::NameId;
    use crate::pools::string_pool::PoolId;
    use crate::pools::uri_pool::UriId;
    use string_interner::Symbol;
    use string_interner::symbol::SymbolU32;

    fn create_test_offset() -> Offset {
        Offset::new(UriId::from_symbol(SymbolU32::try_from_usize(0).unwrap()), 0, 10)
    }

    fn create_test_name_id() -> NameId {
        NameId::from_symbol(SymbolU32::try_from_usize(0).unwrap())
    }

    #[test]
    fn test_as_class() {
        let definition = Definition::Class(Box::new(ClassDefinition::new(create_test_offset(), None)));

        assert!(definition.as_class().is_some());
        assert!(definition.as_module().is_none());
        assert!(definition.as_singleton_class().is_none());
        assert!(definition.as_constant().is_none());
        assert!(definition.as_method().is_none());
        assert!(definition.as_attr_accessor().is_none());
        assert!(definition.as_attr_reader().is_none());
        assert!(definition.as_attr_writer().is_none());
    }

    #[test]
    fn test_as_module() {
        let definition = Definition::Module(Box::new(ModuleDefinition::new(create_test_offset())));

        assert!(definition.as_class().is_none());
        assert!(definition.as_module().is_some());
        assert!(definition.as_singleton_class().is_none());
        assert!(definition.as_constant().is_none());
        assert!(definition.as_method().is_none());
        assert!(definition.as_attr_accessor().is_none());
        assert!(definition.as_attr_reader().is_none());
        assert!(definition.as_attr_writer().is_none());
    }

    #[test]
    fn test_as_singleton_class() {
        let definition = Definition::SingletonClass(Box::new(SingletonClassDefinition::new(create_test_offset())));

        assert!(definition.as_class().is_none());
        assert!(definition.as_module().is_none());
        assert!(definition.as_singleton_class().is_some());
        assert!(definition.as_constant().is_none());
        assert!(definition.as_method().is_none());
        assert!(definition.as_attr_accessor().is_none());
        assert!(definition.as_attr_reader().is_none());
        assert!(definition.as_attr_writer().is_none());
    }

    #[test]
    fn test_as_constant() {
        let definition = Definition::Constant(Box::new(ConstantDefinition::new(create_test_offset())));

        assert!(definition.as_class().is_none());
        assert!(definition.as_module().is_none());
        assert!(definition.as_singleton_class().is_none());
        assert!(definition.as_constant().is_some());
        assert!(definition.as_method().is_none());
        assert!(definition.as_attr_accessor().is_none());
        assert!(definition.as_attr_reader().is_none());
        assert!(definition.as_attr_writer().is_none());
    }

    #[test]
    fn test_as_method() {
        let definition = Definition::Method(Box::new(MethodDefinition::new(create_test_offset(), Vec::new(), false)));

        assert!(definition.as_class().is_none());
        assert!(definition.as_module().is_none());
        assert!(definition.as_singleton_class().is_none());
        assert!(definition.as_constant().is_none());
        assert!(definition.as_method().is_some());
        assert!(definition.as_attr_accessor().is_none());
        assert!(definition.as_attr_reader().is_none());
        assert!(definition.as_attr_writer().is_none());
    }

    #[test]
    fn test_as_attr_accessor() {
        let definition = Definition::AttrAccessor(Box::new(AttrAccessorDefinition::new(create_test_offset())));

        assert!(definition.as_class().is_none());
        assert!(definition.as_module().is_none());
        assert!(definition.as_singleton_class().is_none());
        assert!(definition.as_constant().is_none());
        assert!(definition.as_method().is_none());
        assert!(definition.as_attr_accessor().is_some());
        assert!(definition.as_attr_reader().is_none());
        assert!(definition.as_attr_writer().is_none());
    }

    #[test]
    fn test_as_attr_reader() {
        let definition = Definition::AttrReader(Box::new(AttrReaderDefinition::new(create_test_offset())));

        assert!(definition.as_class().is_none());
        assert!(definition.as_module().is_none());
        assert!(definition.as_singleton_class().is_none());
        assert!(definition.as_constant().is_none());
        assert!(definition.as_method().is_none());
        assert!(definition.as_attr_accessor().is_none());
        assert!(definition.as_attr_reader().is_some());
        assert!(definition.as_attr_writer().is_none());
    }

    #[test]
    fn test_as_attr_writer() {
        let definition = Definition::AttrWriter(Box::new(AttrWriterDefinition::new(create_test_offset())));

        assert!(definition.as_class().is_none());
        assert!(definition.as_module().is_none());
        assert!(definition.as_singleton_class().is_none());
        assert!(definition.as_constant().is_none());
        assert!(definition.as_method().is_none());
        assert!(definition.as_attr_accessor().is_none());
        assert!(definition.as_attr_reader().is_none());
        assert!(definition.as_attr_writer().is_some());
    }
}
