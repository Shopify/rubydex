//! The definitions of the symbols in the source code.
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
//! There are 3 symbol definitions:
//!
//! 1. The module definition for `Foo`
//! 2. The class definition for `Foo::Bar` inside `Foo`
//! 3. The class definition for `Foo::Bar` again

use crate::offset::Offset;
use crate::pools::name_pool::{NameId, NamePool};
use crate::pools::uri_pool::UriPool;

/// The definition of a symbol in the source code.
///
/// This is the root of the symbol definition tree.
/// See `SymbolDefinition`'s variants for more details.
#[derive(Debug)]
pub enum SymbolDefinition {
    Class(Box<ClassDefinition>),
    Module(Box<ModuleDefinition>),
    SingletonClass(Box<SingletonClassDefinition>),
    Constant(Box<ConstantDefinition>),
}

impl SymbolDefinition {
    /// Converts the symbol definition to a string.
    ///
    /// # Arguments
    ///
    /// * `uri_pool` - The uri pool to resolve the uri from the uri ID
    /// * `name_pool` - The name pool to resolve the name from the name ID
    ///
    /// # Returns
    ///
    /// A string representation of the symbol definition.
    #[must_use]
    pub fn to_string(&self, uri_pool: &UriPool, name_pool: &NamePool) -> String {
        match self {
            Self::Class(class) => class.to_string(uri_pool, name_pool),
            Self::Module(module) => module.to_string(uri_pool, name_pool),
            Self::SingletonClass(singleton) => singleton.to_string(uri_pool, name_pool),
            Self::Constant(constant) => constant.to_string(uri_pool, name_pool),
        }
    }
}

/// The definition of a class in the source code.
///
/// A class can have multiple definitions for the same name.
#[derive(Debug)]
pub struct ClassDefinition {
    pub name_id: NameId,
    pub offset: Offset,
    pub superclass: Option<NameId>,
}

impl ClassDefinition {
    #[must_use]
    pub const fn new(name_id: NameId, offset: Offset, superclass: Option<NameId>) -> Self {
        Self {
            name_id,
            offset,
            superclass,
        }
    }

    /// Converts the class definition to a string.
    ///
    /// # Arguments
    ///
    /// * `uri_pool` - The uri pool to resolve the uri from the uri ID
    /// * `name_pool` - The name pool to resolve the name from the name ID
    ///
    /// # Returns
    ///
    /// A string representation of the class definition.
    ///
    /// # Panics
    ///
    /// Panics if the name or superclass is not found in the name pool.
    #[must_use]
    pub fn to_string(&self, uri_pool: &UriPool, name_pool: &NamePool) -> String {
        format!(
            "class {} ({})",
            name_pool.get(self.name_id).unwrap(),
            self.offset.to_string(uri_pool)
        )
    }
}

/// The definition of a module in the source code.
///
/// A module can have multiple definitions for the same name.
#[derive(Debug)]
pub struct ModuleDefinition {
    pub name_id: NameId,
    pub offset: Offset,
}

impl ModuleDefinition {
    #[must_use]
    pub const fn new(name_id: NameId, offset: Offset) -> Self {
        Self { name_id, offset }
    }

    /// Converts the module definition to a string.
    ///
    /// # Arguments
    ///
    /// * `uri_pool` - The uri pool to resolve the uri from the uri ID
    /// * `name_pool` - The name pool to resolve the name from the name ID
    ///
    /// # Returns
    ///
    /// A string representation of the module definition.
    ///
    /// # Panics
    ///
    /// Panics if the name is not found in the name pool.
    #[must_use]
    pub fn to_string(&self, uri_pool: &UriPool, name_pool: &NamePool) -> String {
        format!(
            "module {} ({})",
            name_pool.get(self.name_id).unwrap(),
            self.offset.to_string(uri_pool)
        )
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

    /// Converts the singleton class definition to a string.
    ///
    /// # Arguments
    ///
    /// * `uri_pool` - The uri pool to resolve the uri from the uri ID
    /// * `name_pool` - The name pool to resolve the name from the name ID
    ///
    /// # Returns
    ///
    /// A string representation of the singleton class definition.
    #[must_use]
    pub fn to_string(&self, uri_pool: &UriPool, _name_pool: &NamePool) -> String {
        format!("singleton class ({})", self.offset.to_string(uri_pool))
    }
}

/// The definition of a constant in the source code.
///
/// This represent where a constant is being assigned.
#[derive(Debug)]
pub struct ConstantDefinition {
    pub name_id: NameId,
    pub offset: Offset,
}

impl ConstantDefinition {
    #[must_use]
    pub const fn new(name_id: NameId, offset: Offset) -> Self {
        Self { name_id, offset }
    }

    /// Converts the constant definition to a string.
    ///
    /// # Arguments
    ///
    /// * `uri_pool` - The uri pool to resolve the uri from the uri ID
    /// * `name_pool` - The name pool to resolve the name from the name ID
    ///
    /// # Returns
    ///
    /// A string representation of the constant definition.
    #[must_use]
    pub fn to_string(&self, uri_pool: &UriPool, name_pool: &NamePool) -> String {
        format!(
            "constant {} ({})",
            name_pool.get(self.name_id).unwrap(),
            self.offset.to_string(uri_pool)
        )
    }
}
