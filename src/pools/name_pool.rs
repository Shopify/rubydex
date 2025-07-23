//! Name management with efficient string interning.
//!
//! See [`StringPool`].

use crate::pools::string_pool::{PoolId, StringPool, SymbolId};

/// A unique identifier for a name in a [`NamePool`].
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct NameId(SymbolId);

impl PoolId for NameId {
    fn from_symbol(symbol: SymbolId) -> Self {
        Self(symbol)
    }

    fn to_symbol(&self) -> SymbolId {
        self.0
    }
}

/// A specialized string pool for efficient name storage and retrieval using [`NameId`].
pub type NamePool = StringPool<NameId>;
