use std::sync::LazyLock;

use url::Url;

use crate::{
    indexing::{self, LanguageId},
    model::{
        declaration::{ClassDeclaration, Declaration, Namespace},
        graph::Graph,
        ids::{DeclarationId, UriId},
    },
};

pub const BUILT_IN_URI: &str = "rubydex:built-in";
pub static BUILT_IN_URI_ID: LazyLock<UriId> = LazyLock::new(|| UriId::from(BUILT_IN_URI));

pub static KERNEL_ID: LazyLock<DeclarationId> = LazyLock::new(|| DeclarationId::from("Kernel"));
pub static BASIC_OBJECT_ID: LazyLock<DeclarationId> = LazyLock::new(|| DeclarationId::from("BasicObject"));
pub static OBJECT_ID: LazyLock<DeclarationId> = LazyLock::new(|| DeclarationId::from("Object"));
pub static MODULE_ID: LazyLock<DeclarationId> = LazyLock::new(|| DeclarationId::from("Module"));
pub static CLASS_ID: LazyLock<DeclarationId> = LazyLock::new(|| DeclarationId::from("Class"));

/// The declaration ids that [`add_built_in_data`] unconditionally inserts into every graph, and thus
/// are guaranteed to already exist in a fresh graph.
///
/// This is the set the `SQLite` cache excludes from serialized closures: a fresh graph already has
/// them, so they are never stored or merged. `Kernel` is deliberately **absent** — although it is
/// declared in the built-in source below, its declaration is materialized by *resolution*, not by
/// this seeding, so a fresh (unresolved) graph does not contain it and it must be serialized like
/// any other declaration.
///
/// Must stay in sync with the explicit `declarations.insert(...)` calls in [`add_built_in_data`].
#[must_use]
pub fn seeded_declaration_ids() -> [DeclarationId; 4] {
    [*BASIC_OBJECT_ID, *OBJECT_ID, *MODULE_ID, *CLASS_ID]
}

/// Adds core classes and modules data to the graph so that resolution can provide correct results even when not
/// indexing the complete RBS core definitions
///
/// # Panics
///
/// Will panic if the built-in URI is invalid
pub fn add_built_in_data(graph: &mut Graph) {
    // We need definitions to ensure that ancestor linearization happens naturally through the algorithm. Trying to set
    // ancestors directly on declarations doesn't work because the algorithm erases the ancestors and there are no
    // definitions to inform it of the superclasses and mixins.
    let uri = Url::parse(BUILT_IN_URI).unwrap();
    let source = r"
      class BasicObject
      end

      module Kernel
      end

      class Object < BasicObject
        include Kernel
      end

      class Module < Object
      end

      class Class < Module
      end
    ";
    indexing::index_source(graph, uri.as_ref(), source, &LanguageId::Rbs);

    // Creating declarations eagerly is still necessary because we need to associate correct ownership data no matter in
    // what order we discover classes and modules
    let declarations = graph.declarations_mut();

    // Built-in declarations that always exist in the Ruby object model
    declarations.insert(
        *BASIC_OBJECT_ID,
        Declaration::Namespace(Namespace::Class(Box::new(ClassDeclaration::new(
            "BasicObject".to_string(),
            *OBJECT_ID,
        )))),
    );
    declarations.insert(
        *OBJECT_ID,
        Declaration::Namespace(Namespace::Class(Box::new(ClassDeclaration::new(
            "Object".to_string(),
            *OBJECT_ID,
        )))),
    );
    declarations.insert(
        *MODULE_ID,
        Declaration::Namespace(Namespace::Class(Box::new(ClassDeclaration::new(
            "Module".to_string(),
            *OBJECT_ID,
        )))),
    );
    declarations.insert(
        *CLASS_ID,
        Declaration::Namespace(Namespace::Class(Box::new(ClassDeclaration::new(
            "Class".to_string(),
            *OBJECT_ID,
        )))),
    );
}
