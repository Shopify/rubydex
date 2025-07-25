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

use crate::pools::name_pool::{NameId, NamePool};

#[derive(Debug)]
pub struct Declaration {
    pub name_id: NameId,
}

impl Declaration {
    pub fn new(name_id: NameId) -> Self {
        Self { name_id }
    }

    #[must_use]
    pub fn to_string(&self, name_pool: &NamePool) -> String {
        format!("declaration {}", name_pool.get(self.name_id).unwrap())
    }
}
