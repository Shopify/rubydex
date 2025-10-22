use crate::model::ids::DeclarationId;
use std::sync::Arc;

/// The nesting structure is a linked list that represents the chain of lexical scopes we found. This is used to resolve
/// constant references and always links scopes based on their fully qualified name with whatever scope came before.
/// Take the following example:
///
/// ```ruby
/// module Foo
///   CONST = 42
///
///   class ::Bar
///     CONST # resolves to Foo::CONST because we're inside the `Foo` namespace
///   end
/// end
///
/// class Bar
///   CONST # NameError because this `Bar` is not lexically inside `Foo`
/// end
///
/// ```
///
/// In this example, the top level definition of `::Bar` has the fully qualified name `Bar`, but it is _still_ lexically
/// connected to `Foo` for purposes of constant resolution. The code in the example does not crash and properly finds
/// `Foo::CONST`. Therefore, to properly resolve constants, we need to remember the exact chain of lexical scopes we
/// entered, independentally if the fully qualified name was reset by a top level reference.
#[derive(Debug)]
pub struct Nesting {
    /// The parent scope if any
    parent: Option<Arc<Nesting>>,
    /// The declaration ID based on the fully qualified name of this scope
    declaration_id: DeclarationId,
}

impl Nesting {
    #[must_use]
    pub fn new(parent: Option<Arc<Nesting>>, declaration_id: DeclarationId) -> Self {
        Self { parent, declaration_id }
    }

    #[must_use]
    pub fn declaration_id(&self) -> &DeclarationId {
        &self.declaration_id
    }

    #[must_use]
    pub fn parent(&self) -> &Option<Arc<Nesting>> {
        &self.parent
    }

    #[cfg(test)]
    #[must_use]
    pub fn ids_as_vec(&self) -> Vec<DeclarationId> {
        if let Some(parent) = &self.parent {
            let mut vec = parent.ids_as_vec();
            vec.push(self.declaration_id);
            vec
        } else {
            vec![self.declaration_id]
        }
    }
}

/// The Scope structure maintains both the `Nesting` linked list of all lexical scopes we encountered and a name stack,
/// so that we can compute fully qualified names ahead of time for all entries in the graph and their respective
/// declaration IDs
#[derive(Default)]
pub struct Scope {
    /// A vector of **fully qualified names**. For example:
    ///
    /// ```ruby
    /// module Foo
    ///   class ::Bar
    ///   end
    ///   class Baz::Qux
    ///   end
    /// end
    /// ```
    ///
    /// Results in the stack being: `["Foo", "Bar", "Foo::Baz::Qux"]`
    name_stack: Vec<String>,
    /// The current chain of lexical scopes
    nesting: Option<Arc<Nesting>>,
}

impl Scope {
    #[must_use]
    pub fn new() -> Self {
        Self {
            name_stack: Vec::new(),
            nesting: None,
        }
    }

    #[must_use]
    pub fn nesting(&self) -> &Option<Arc<Nesting>> {
        &self.nesting
    }

    /// Fully qualify the given name according to the current scope. Takes top level references into account. For example:
    ///
    /// ```ruby
    /// module Foo
    ///   # fully_qualify("Bar") => "Foo::Bar"
    ///   # fully_qualify("::Bar") => "Bar"
    ///   class Baz
    ///     # fully_qualify("::Qux") => "Qux"
    ///     # fully_qualify("Qux") => "Foo::Baz::Qux"
    ///   end
    /// end
    /// ```
    #[must_use]
    pub fn fully_qualify(&self, name: &str) -> String {
        if let Some(trimmed) = name.strip_prefix("::") {
            trimmed.to_string()
        } else if let Some(previous) = self.name_stack.last() {
            format!("{previous}::{name}")
        } else {
            name.to_string()
        }
    }

    /// Enters a new namespace (module or class), modifying the scope accordingly
    pub fn enter(&mut self, name: &str) -> (String, DeclarationId) {
        let fully_qualified_name = self.fully_qualify(name);
        let id = DeclarationId::from(&fully_qualified_name);
        self.name_stack.push(fully_qualified_name.clone());
        self.enter_nesting(id);
        (fully_qualified_name, id)
    }

    /// Leaves the current namespace, restoring the previous scope
    pub fn leave(&mut self) {
        self.name_stack.pop();

        if let Some(current) = &self.nesting {
            self.nesting = current.parent.as_ref().map(Arc::clone);
        }
    }

    fn enter_nesting(&mut self, declaration_id: DeclarationId) {
        if let Some(current) = &self.nesting {
            let new_nesting = Arc::new(Nesting::new(Some(Arc::clone(current)), declaration_id));
            self.nesting = Some(new_nesting);
        } else {
            let new_nesting = Arc::new(Nesting::new(None, declaration_id));
            self.nesting = Some(new_nesting);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn regular_nesting() {
        let mut scope = Scope::new();

        // Foo
        let (name, id) = scope.enter("Foo");
        assert_eq!(name, String::from("Foo"));
        assert_eq!(id, DeclarationId::from("Foo"));
        assert_eq!(
            scope.nesting.as_ref().unwrap().ids_as_vec(),
            vec![DeclarationId::from("Foo")]
        );

        // Bar
        let (name, id) = scope.enter("Bar");
        assert_eq!(name, String::from("Foo::Bar"));
        assert_eq!(id, DeclarationId::from("Foo::Bar"));
        assert_eq!(
            scope.nesting.as_ref().unwrap().ids_as_vec(),
            vec![DeclarationId::from("Foo"), DeclarationId::from("Foo::Bar")]
        );

        scope.leave();
        assert_eq!(
            scope.nesting.as_ref().unwrap().ids_as_vec(),
            vec![DeclarationId::from("Foo")]
        );
        assert_eq!(scope.fully_qualify("CONST"), String::from("Foo::CONST"));

        scope.leave();
    }

    #[test]
    fn compact_namespace_nesting() {
        let mut scope = Scope::new();

        // Foo
        let (name, id) = scope.enter("Foo");
        assert_eq!(name, String::from("Foo"));
        assert_eq!(id, DeclarationId::from("Foo"));
        assert_eq!(
            scope.nesting.as_ref().unwrap().ids_as_vec(),
            vec![DeclarationId::from("Foo")]
        );

        // Bar::Baz
        let (name, id) = scope.enter("Bar::Baz");
        assert_eq!(name, String::from("Foo::Bar::Baz"));
        assert_eq!(id, DeclarationId::from("Foo::Bar::Baz"));
        assert_eq!(scope.fully_qualify("CONST"), String::from("Foo::Bar::Baz::CONST"));
        assert_eq!(
            scope.nesting.as_ref().unwrap().ids_as_vec(),
            vec![DeclarationId::from("Foo"), DeclarationId::from("Foo::Bar::Baz")]
        );

        scope.leave();
        assert_eq!(
            scope.nesting.as_ref().unwrap().ids_as_vec(),
            vec![DeclarationId::from("Foo")]
        );
        assert_eq!(scope.fully_qualify("CONST"), String::from("Foo::CONST"));

        scope.leave();
    }

    #[test]
    fn top_level_nesting() {
        let mut scope = Scope::new();

        // Foo
        let (name, id) = scope.enter("Foo");
        assert_eq!(name, String::from("Foo"));
        assert_eq!(id, DeclarationId::from("Foo"));

        assert_eq!(
            scope.nesting.as_ref().unwrap().ids_as_vec(),
            vec![DeclarationId::from("Foo")]
        );

        // ::Bar
        let (name, id) = scope.enter("::Bar");
        assert_eq!(name, String::from("Bar"));
        assert_eq!(id, DeclarationId::from("Bar"));
        assert_eq!(scope.fully_qualify("CONST"), String::from("Bar::CONST"));
        assert_eq!(
            scope.nesting.as_ref().unwrap().ids_as_vec(),
            vec![DeclarationId::from("Foo"), DeclarationId::from("Bar")]
        );

        scope.leave();
        assert_eq!(
            scope.nesting.as_ref().unwrap().ids_as_vec(),
            vec![DeclarationId::from("Foo")]
        );
        assert_eq!(scope.fully_qualify("CONST"), String::from("Foo::CONST"));

        scope.leave();
    }

    #[test]
    fn top_level_compact_nesting() {
        let mut scope = Scope::new();

        // Foo
        let (name, id) = scope.enter("Foo");
        assert_eq!(name, String::from("Foo"));
        assert_eq!(id, DeclarationId::from("Foo"));

        assert_eq!(
            scope.nesting.as_ref().unwrap().ids_as_vec(),
            vec![DeclarationId::from("Foo")]
        );

        // ::Bar::Baz
        let (name, id) = scope.enter("::Bar::Baz");
        assert_eq!(name, String::from("Bar::Baz"));
        assert_eq!(id, DeclarationId::from("Bar::Baz"));
        assert_eq!(scope.fully_qualify("CONST"), String::from("Bar::Baz::CONST"));
        assert_eq!(
            scope.nesting.as_ref().unwrap().ids_as_vec(),
            vec![DeclarationId::from("Foo"), DeclarationId::from("Bar::Baz")]
        );

        scope.leave();
        assert_eq!(
            scope.nesting.as_ref().unwrap().ids_as_vec(),
            vec![DeclarationId::from("Foo")]
        );
        assert_eq!(scope.fully_qualify("CONST"), String::from("Foo::CONST"));

        scope.leave();
    }
}
