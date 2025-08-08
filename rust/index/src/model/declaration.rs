//! Represent the declaration of a name in the Ruby model.
//!
//! A declaration is composed of one or more `Definition`s.
//!
//! Let's consider the following example:
//! ```ruby
//! class Foo; end
//! def Foo; end
//! ```
//!
//! There are 2 definitions:
//! 1. The class definition for `Foo`
//! 2. The method definition for `Foo`
//!
//! and only one declaration: for the name `Foo`

use crate::model::definitions::Definition;

#[derive(Debug)]
pub struct Declaration {
    pub name: String,
    definitions: Vec<Definition>,
}

impl Declaration {
    #[must_use]
    pub fn new(name: String, definitions: Option<Vec<Definition>>) -> Self {
        Self {
            name,
            definitions: definitions.unwrap_or_default(),
        }
    }
}
