use std::sync::LazyLock;

use url::Url;

use crate::{
    indexing::{self, LanguageId},
    model::{
        declaration::{ClassDeclaration, Declaration, Namespace},
        graph::Graph,
        ids::DeclarationId,
    },
};

pub static KERNEL_ID: LazyLock<DeclarationId> = LazyLock::new(|| DeclarationId::from("Kernel"));
pub static BASIC_OBJECT_ID: LazyLock<DeclarationId> = LazyLock::new(|| DeclarationId::from("BasicObject"));
pub static OBJECT_ID: LazyLock<DeclarationId> = LazyLock::new(|| DeclarationId::from("Object"));
pub static MODULE_ID: LazyLock<DeclarationId> = LazyLock::new(|| DeclarationId::from("Module"));
pub static CLASS_ID: LazyLock<DeclarationId> = LazyLock::new(|| DeclarationId::from("Class"));

/// Returns true if `decl_id` is a built-in declaration that should never be
/// removed from the graph, even when it has no backing definitions.
#[must_use]
pub fn is_built_in(decl_id: &DeclarationId) -> bool {
    *decl_id == *KERNEL_ID
        || *decl_id == *BASIC_OBJECT_ID
        || *decl_id == *OBJECT_ID
        || *decl_id == *MODULE_ID
        || *decl_id == *CLASS_ID
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
    let uri = Url::parse("rubydex:built-in").unwrap();
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
