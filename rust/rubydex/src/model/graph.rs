use std::collections::hash_map::Entry;
use std::collections::HashMap;
use std::sync::LazyLock;

use crate::diagnostic::Diagnostic;
use crate::indexing::local_graph::LocalGraph;
use crate::model::declaration::{Ancestor, Declaration, Namespace};
use crate::model::definitions::{Definition, Mixin};
use crate::model::document::Document;
use crate::model::encoding::Encoding;
use crate::model::identity_maps::{IdentityHashMap, IdentityHashSet};
use crate::model::ids::{DeclarationId, DefinitionId, NameId, ReferenceId, StringId, UriId};
use crate::model::name::{NameRef, ResolvedName};
// use crate::model::integrity::IntegrityChecker;
use crate::model::references::{ConstantReference, MethodRef};
use crate::stats;

pub static OBJECT_ID: LazyLock<DeclarationId> = LazyLock::new(|| DeclarationId::from("Object"));
pub static MODULE_ID: LazyLock<DeclarationId> = LazyLock::new(|| DeclarationId::from("Module"));
pub static CLASS_ID: LazyLock<DeclarationId> = LazyLock::new(|| DeclarationId::from("Class"));

// The `Graph` is the global representation of the entire Ruby codebase. It contains all declarations and their
// relationships
#[derive(Default, Debug)]
pub struct Graph {
    // Map of declaration nodes
    declarations: IdentityHashMap<DeclarationId, Declaration>,
    // Map of document nodes
    documents: IdentityHashMap<UriId, Document>,
    // Map of definition nodes
    definitions: IdentityHashMap<DefinitionId, Definition>,

    definitions_to_declarations: IdentityHashMap<DefinitionId, DeclarationId>,

    // Map of unqualified names
    strings: IdentityHashMap<StringId, String>,
    // Map of names
    names: IdentityHashMap<NameId, NameRef>,
    // Map of constant references
    constant_references: IdentityHashMap<ReferenceId, ConstantReference>,
    // Map of method references that still need to be resolved
    method_references: IdentityHashMap<ReferenceId, MethodRef>,

    diagnostics: Vec<Diagnostic>,
    /// The position encoding used for LSP line/column locations. Not related to the actual encoding of the file
    position_encoding: Encoding,

    /// Synthetic method definitions added by plugins: (def_id, owner_fqn)
    synthetic_methods: Vec<(DefinitionId, String)>,
    /// Synthetic class definitions added by plugins: (def_id, fqn)
    synthetic_classes: Vec<(DefinitionId, String)>,
    /// Synthetic module definitions added by plugins: (def_id, fqn)
    synthetic_modules: Vec<(DefinitionId, String)>,
    /// Synthetic mixin relationships: (target_decl_id, mixin)
    synthetic_mixins: Vec<(DeclarationId, Mixin)>,
    /// Included hooks: trigger_module_fqn -> vec of modules to extend
    included_hooks: HashMap<String, Vec<String>>,
    /// Counter for synthetic definition IDs (negative values)
    synthetic_id_counter: i64,
}

impl Graph {
    #[must_use]
    pub fn new() -> Self {
        Self {
            declarations: IdentityHashMap::default(),
            definitions: IdentityHashMap::default(),
            definitions_to_declarations: IdentityHashMap::default(),
            documents: IdentityHashMap::default(),
            strings: IdentityHashMap::default(),
            names: IdentityHashMap::default(),
            constant_references: IdentityHashMap::default(),
            method_references: IdentityHashMap::default(),
            diagnostics: Vec::new(),
            position_encoding: Encoding::default(),
            synthetic_methods: Vec::new(),
            synthetic_classes: Vec::new(),
            synthetic_modules: Vec::new(),
            synthetic_mixins: Vec::new(),
            included_hooks: HashMap::new(),
            synthetic_id_counter: 0,
        }
    }

    // Returns an immutable reference to the declarations map
    #[must_use]
    pub fn declarations(&self) -> &IdentityHashMap<DeclarationId, Declaration> {
        &self.declarations
    }

    /// Returns a mutable reference to the declarations map
    #[must_use]
    pub fn declarations_mut(&mut self) -> &mut IdentityHashMap<DeclarationId, Declaration> {
        &mut self.declarations
    }

    /// # Panics
    ///
    /// Panics if acquiring a write lock fails
    pub fn add_declaration<F>(&mut self, declaration_id: DeclarationId, definition_id: DefinitionId, constructor: F)
    where
        F: FnOnce() -> Declaration,
    {
        self.definitions_to_declarations.insert(definition_id, declaration_id);

        let declaration = self.declarations.entry(declaration_id).or_insert_with(constructor);
        declaration.add_definition(definition_id);
    }

    /// # Panics
    ///
    /// Panics if acquiring a write lock fails
    pub fn clear_declarations(&mut self) {
        self.definitions_to_declarations.clear();
        self.declarations.clear();
    }

    // Returns an immutable reference to the definitions map
    #[must_use]
    pub fn definitions(&self) -> &IdentityHashMap<DefinitionId, Definition> {
        &self.definitions
    }

    /// Returns the ID of the unqualified name of a definition
    ///
    /// # Panics
    ///
    /// This will panic if there's inconsistent data in the graph
    #[must_use]
    pub fn definition_string_id(&self, definition: &Definition) -> StringId {
        let id = match definition {
            Definition::Class(it) => {
                let name = self.names.get(it.name_id()).unwrap();
                name.str()
            }
            Definition::SingletonClass(it) => {
                let name = self.names.get(it.name_id()).unwrap();
                name.str()
            }
            Definition::Module(it) => {
                let name = self.names.get(it.name_id()).unwrap();
                name.str()
            }
            Definition::Constant(it) => {
                let name = self.names.get(it.name_id()).unwrap();
                name.str()
            }
            Definition::ConstantAlias(it) => {
                let name = self.names.get(it.name_id()).unwrap();
                name.str()
            }
            Definition::GlobalVariable(it) => it.str_id(),
            Definition::InstanceVariable(it) => it.str_id(),
            Definition::ClassVariable(it) => it.str_id(),
            Definition::AttrAccessor(it) => it.str_id(),
            Definition::AttrReader(it) => it.str_id(),
            Definition::AttrWriter(it) => it.str_id(),
            Definition::Method(it) => it.str_id(),
            Definition::MethodAlias(it) => it.new_name_str_id(),
            Definition::GlobalVariableAlias(it) => it.new_name_str_id(),
        };

        *id
    }

    // Returns an immutable reference to the strings map
    #[must_use]
    pub fn strings(&self) -> &IdentityHashMap<StringId, String> {
        &self.strings
    }

    // Returns an immutable reference to the URI pool map
    #[must_use]
    pub fn documents(&self) -> &IdentityHashMap<UriId, Document> {
        &self.documents
    }

    #[must_use]
    pub fn definitions_to_declarations(&self) -> &IdentityHashMap<DefinitionId, DeclarationId> {
        &self.definitions_to_declarations
    }

    // Returns an immutable reference to the constant references map
    #[must_use]
    pub fn constant_references(&self) -> &IdentityHashMap<ReferenceId, ConstantReference> {
        &self.constant_references
    }

    // Returns an immutable reference to the method references map
    #[must_use]
    pub fn method_references(&self) -> &IdentityHashMap<ReferenceId, MethodRef> {
        &self.method_references
    }

    #[must_use]
    pub fn diagnostics(&self) -> &Vec<Diagnostic> {
        &self.diagnostics
    }

    #[must_use]
    pub fn synthetic_methods(&self) -> &[(DefinitionId, String)] {
        &self.synthetic_methods
    }

    #[must_use]
    pub fn synthetic_classes(&self) -> &[(DefinitionId, String)] {
        &self.synthetic_classes
    }

    #[must_use]
    pub fn synthetic_modules(&self) -> &[(DefinitionId, String)] {
        &self.synthetic_modules
    }

    #[must_use]
    pub fn synthetic_mixins(&self) -> &[(DeclarationId, Mixin)] {
        &self.synthetic_mixins
    }

    #[must_use]
    pub fn included_hooks(&self) -> &HashMap<String, Vec<String>> {
        &self.included_hooks
    }

    /// Registers an included hook: when trigger_module is included, extend with extend_module.
    pub fn register_included_hook(&mut self, trigger_module: &str, extend_module: &str) {
        self.included_hooks
            .entry(trigger_module.to_string())
            .or_default()
            .push(extend_module.to_string());
    }

    /// Adds a synthetic mixin relationship created by plugins.
    /// Returns true if the target exists and mixin was added, false otherwise.
    pub fn add_synthetic_mixin(&mut self, target_name: &str, module_name: &str, mixin_type: u8) -> bool {
        use crate::model::name::{Name, NameRef};

        // Check target exists
        let target_decl_id = DeclarationId::from(target_name);
        if !self.declarations().contains_key(&target_decl_id) {
            return false;
        }

        // Create a name for the module reference
        let unqualified = module_name.rsplit("::").next().unwrap_or(module_name);
        let module_str_id = self.intern_string(unqualified.to_string());
        let name = Name::new(module_str_id, None, None);
        let name_id = name.id();
        self.names.insert(name_id, NameRef::Unresolved(Box::new(name)));

        // Create the mixin based on type: 0=Include, 1=Prepend, 2=Extend
        let mixin = match mixin_type {
            0 => Mixin::Include(name_id),
            1 => Mixin::Prepend(name_id),
            2 => Mixin::Extend(name_id),
            _ => return false,
        };

        self.synthetic_mixins.push((target_decl_id, mixin));
        true
    }

    /// Returns a mutable reference to the definitions map
    #[must_use]
    pub fn definitions_mut(&mut self) -> &mut IdentityHashMap<DefinitionId, Definition> {
        &mut self.definitions
    }

    /// Interns a string and returns its ID
    pub fn intern_string(&mut self, string: String) -> StringId {
        let string_id = StringId::from(&string);
        self.strings.insert(string_id, string);
        string_id
    }

    /// Returns next synthetic definition ID (negative to avoid collision with hash-based IDs)
    fn next_synthetic_id(&mut self) -> i64 {
        self.synthetic_id_counter -= 1;
        self.synthetic_id_counter
    }

    /// Adds a synthetic method definition created by plugins.
    /// Returns true if the owner exists and method was added, false otherwise.
    pub fn add_synthetic_method(
        &mut self,
        owner_name: &str,
        method_name: &str,
        file_path: &str,
        line: u32,
        column: u32,
    ) -> bool {
        use crate::model::definitions::{DefinitionFlags, MethodDefinition};
        use crate::model::visibility::Visibility;
        use crate::offset::Offset;

        // Check owner exists
        let owner_decl_id = DeclarationId::from(owner_name);
        if !self.declarations().contains_key(&owner_decl_id) {
            return false;
        }

        let uri_id = UriId::from(file_path);
        let name_str_id = self.intern_string(method_name.to_string());

        // Create synthetic offset using line and column
        // We use a simple encoding: start position based on line/column
        let start = (line.saturating_sub(1)) * 1000 + column;
        let offset = Offset::new(start, start + method_name.len() as u32);

        // Create definition ID (negative to avoid collision)
        let def_id = DefinitionId::new(self.next_synthetic_id());

        let definition = Definition::Method(Box::new(MethodDefinition::new(
            name_str_id,
            uri_id,
            offset,
            Vec::new(), // no comments
            DefinitionFlags::empty(),
            None,                // lexical_nesting_id
            Vec::new(),          // no parameters
            Visibility::Public,
            None,                // no receiver
        )));

        self.definitions.insert(def_id, definition);
        self.synthetic_methods.push((def_id, owner_name.to_string()));
        true
    }

    /// Adds a synthetic class definition created by plugins.
    /// Returns true since synthetic classes can always be created.
    pub fn add_synthetic_class(
        &mut self,
        class_name: &str,
        parent_name: Option<&str>,
        file_path: &str,
        line: u32,
        column: u32,
    ) -> bool {
        use crate::model::definitions::{ClassDefinition, DefinitionFlags};
        use crate::model::name::{Name, NameRef};
        use crate::offset::Offset;

        let uri_id = UriId::from(file_path);
        let unqualified = class_name.rsplit("::").next().unwrap_or(class_name);
        let name_str_id = self.intern_string(unqualified.to_string());

        // Create synthetic offset using line and column
        let start = (line.saturating_sub(1)) * 1000 + column;
        let offset = Offset::new(start, start + class_name.len() as u32);

        // Create definition ID (negative to avoid collision)
        let def_id = DefinitionId::new(self.next_synthetic_id());

        // Create the name for this class definition and insert into names map
        let name = Name::new(name_str_id, None, None);
        let name_id = name.id();
        self.names.insert(name_id, NameRef::Unresolved(Box::new(name)));

        // Create parent reference if provided
        let parent_name_id = parent_name.map(|p| {
            let parent_unqualified = p.rsplit("::").next().unwrap_or(p);
            let parent_str_id = self.intern_string(parent_unqualified.to_string());
            let parent_name = Name::new(parent_str_id, None, None);
            let parent_name_id = parent_name.id();
            self.names.insert(parent_name_id, NameRef::Unresolved(Box::new(parent_name)));
            parent_name_id
        });

        let definition = Definition::Class(Box::new(ClassDefinition::new(
            name_id,
            uri_id,
            offset,
            Vec::new(), // no comments
            DefinitionFlags::empty(),
            None, // lexical_nesting_id
            parent_name_id,
        )));

        self.definitions.insert(def_id, definition);
        self.synthetic_classes.push((def_id, class_name.to_string()));
        true
    }

    /// Adds a synthetic module definition created by plugins.
    /// Returns true since synthetic modules can always be created.
    pub fn add_synthetic_module(
        &mut self,
        module_name: &str,
        file_path: &str,
        line: u32,
        column: u32,
    ) -> bool {
        use crate::model::definitions::{DefinitionFlags, ModuleDefinition};
        use crate::model::name::{Name, NameRef};
        use crate::offset::Offset;

        let uri_id = UriId::from(file_path);
        let unqualified = module_name.rsplit("::").next().unwrap_or(module_name);
        let name_str_id = self.intern_string(unqualified.to_string());

        // Create synthetic offset using line and column
        let start = (line.saturating_sub(1)) * 1000 + column;
        let offset = Offset::new(start, start + module_name.len() as u32);

        // Create definition ID (negative to avoid collision)
        let def_id = DefinitionId::new(self.next_synthetic_id());

        // Create the name for this module definition and insert into names map
        let name = Name::new(name_str_id, None, None);
        let name_id = name.id();
        self.names.insert(name_id, NameRef::Unresolved(Box::new(name)));

        let definition = Definition::Module(Box::new(ModuleDefinition::new(
            name_id,
            uri_id,
            offset,
            Vec::new(), // no comments
            DefinitionFlags::empty(),
            None, // lexical_nesting_id
        )));

        self.definitions.insert(def_id, definition);
        self.synthetic_modules.push((def_id, module_name.to_string()));
        true
    }

    /// # Panics
    ///
    /// Panics if acquiring a read lock fails
    #[must_use]
    pub fn get(&self, name: &str) -> Option<Vec<&Definition>> {
        let declaration_id = DeclarationId::from(name);
        let declaration = self.declarations.get(&declaration_id)?;

        Some(
            declaration
                .definitions()
                .iter()
                .filter_map(|id| self.definitions.get(id))
                .collect(),
        )
    }

    /// Returns all target declaration IDs for a constant alias.
    ///
    /// A constant alias can have multiple definitions (e.g., conditional assignment in different files),
    /// each potentially pointing to a different target. This method collects all resolved targets.
    ///
    /// Returns `None` if the declaration doesn't exist or is not a constant alias.
    /// Returns `Some(vec![])` if no targets have been resolved yet.
    #[must_use]
    pub fn alias_targets(&self, declaration_id: &DeclarationId) -> Option<Vec<DeclarationId>> {
        let declaration = self.declarations.get(declaration_id)?;

        let Declaration::ConstantAlias(_) = declaration else {
            return None;
        };

        let mut targets = Vec::new();
        for definition_id in declaration.definitions() {
            let Some(Definition::ConstantAlias(alias_def)) = self.definitions.get(definition_id) else {
                continue;
            };

            let target_name_id = alias_def.target_name_id();
            let Some(name_ref) = self.names.get(target_name_id) else {
                continue;
            };

            if let NameRef::Resolved(resolved) = name_ref {
                let target_id = *resolved.declaration_id();
                if !targets.contains(&target_id) {
                    targets.push(target_id);
                }
            }
        }

        Some(targets)
    }

    #[must_use]
    pub fn names(&self) -> &IdentityHashMap<NameId, NameRef> {
        &self.names
    }

    fn untrack_name(&mut self, name_id: NameId) {
        if let Some(name_ref) = self.names.get_mut(&name_id)
            && !name_ref.decrement_ref_count()
        {
            self.names.remove(&name_id);
        }
    }

    /// Register a member relationship from a declaration to another declaration through its unqualified name id. For example, in
    ///
    /// ```ruby
    /// module Foo
    ///   class Bar; end
    ///   def baz; end
    /// end
    /// ```
    ///
    /// `Foo` has two members:
    /// ```ruby
    /// {
    ///   NameId(Bar) => DeclarationId(Bar)
    ///   NameId(baz) => DeclarationId(baz)
    /// }
    /// ```
    ///
    /// # Panics
    ///
    /// Will panic if the declaration ID passed doesn't belong to a namespace declaration
    pub fn add_member(
        &mut self,
        owner_id: &DeclarationId,
        member_declaration_id: DeclarationId,
        member_str_id: StringId,
    ) {
        if let Some(declaration) = self.declarations.get_mut(owner_id) {
            match declaration {
                Declaration::Namespace(Namespace::Class(it)) => it.add_member(member_str_id, member_declaration_id),
                Declaration::Namespace(Namespace::Module(it)) => it.add_member(member_str_id, member_declaration_id),
                Declaration::Namespace(Namespace::SingletonClass(it)) => {
                    it.add_member(member_str_id, member_declaration_id);
                }
                Declaration::Constant(_) => {
                    // TODO: temporary hack to avoid crashing on `Struct.new`, `Class.new` and `Module.new`
                }
                _ => panic!("Tried to add member to a declaration that isn't a namespace"),
            }
        }
    }

    /// # Panics
    ///
    /// This function will panic when trying to record a resolve name for a name ID that does not exist
    pub fn record_resolved_name(&mut self, name_id: NameId, declaration_id: DeclarationId) {
        match self.names.entry(name_id) {
            Entry::Occupied(entry) => match entry.get() {
                NameRef::Unresolved(_) => {
                    if let NameRef::Unresolved(unresolved) = entry.remove() {
                        let resolved_name = NameRef::Resolved(Box::new(ResolvedName::new(*unresolved, declaration_id)));
                        self.names.insert(name_id, resolved_name);
                    }
                }
                NameRef::Resolved(_) => {
                    // TODO: consider if this is a valid scenario with the resolution phase design. Either collect
                    // metrics here or panic if it's never supposed to occur
                }
            },
            Entry::Vacant(_) => panic!("Trying to record resolved name for a name ID that does not exist"),
        }
    }

    /// # Panics
    ///
    /// Panics if acquiring a write lock fails
    pub fn record_resolved_reference(&mut self, reference_id: ReferenceId, declaration_id: DeclarationId) {
        if let Some(declaration) = self.declarations.get_mut(&declaration_id) {
            declaration.add_reference(reference_id);
        }
    }

    //// Handles the deletion of a document identified by `uri`
    pub fn delete_uri(&mut self, uri: &str) {
        let uri_id = UriId::from(uri);
        self.remove_definitions_for_uri(uri_id);
        self.documents.remove(&uri_id);
    }

    /// Merges everything in `other` into this Graph. This method is meant to merge all graph representations from
    /// different threads, but not meant to handle updates to the existing global representation
    pub fn extend(&mut self, local_graph: LocalGraph) {
        let (uri_id, document, definitions, strings, names, constant_references, method_references, diagnostics) =
            local_graph.into_parts();

        self.documents.insert(uri_id, document);
        self.definitions.extend(definitions);
        self.strings.extend(strings);
        for (name_id, name_ref) in names {
            match self.names.entry(name_id) {
                Entry::Occupied(mut entry) => {
                    entry.get_mut().increment_ref_count(name_ref.ref_count());
                }
                Entry::Vacant(entry) => {
                    entry.insert(name_ref);
                }
            }
        }
        self.constant_references.extend(constant_references);
        self.method_references.extend(method_references);
        self.diagnostics.extend(diagnostics);
    }

    /// Updates the global representation with the information contained in `other`, handling deletions, insertions and
    /// updates to existing entries
    pub fn update(&mut self, other: LocalGraph) {
        // For each URI that was indexed through `other`, check what was discovered and update our current global
        // representation
        let uri_id = other.uri_id();
        self.remove_definitions_for_uri(uri_id);

        self.extend(other);
    }

    // Removes all nodes and relationships associated to the given URI. This is used to clean up stale data when a
    // document (identified by `uri_id`) changes or when a document is closed and we need to clean up the memory
    fn remove_definitions_for_uri(&mut self, uri_id: UriId) {
        let Some(document) = self.documents.remove(&uri_id) else {
            return;
        };

        // TODO: Remove method references from method declarations once method inference is implemented
        for ref_id in document.method_references() {
            self.method_references.remove(ref_id);
        }

        for ref_id in document.constant_references() {
            if let Some(constant_ref) = self.constant_references.remove(ref_id) {
                if let Some(NameRef::Resolved(resolved)) = self.names.get(constant_ref.name_id())
                    && let Some(declaration) = self.declarations.get_mut(resolved.declaration_id())
                {
                    declaration.remove_reference(ref_id);
                }
                self.untrack_name(*constant_ref.name_id());
            }
        }

        // Vector of (owner_declaration_id, member_name_id) to delete after processing all definitions
        let mut members_to_delete: Vec<(DeclarationId, StringId)> = Vec::new();
        let mut declarations_to_delete: Vec<DeclarationId> = Vec::new();
        let mut declarations_to_invalidate_ancestor_chains: Vec<DeclarationId> = Vec::new();

        for def_id in document.definitions() {
            if let Some(definition) = self.definitions.remove(def_id) {
                if let Some(declaration_id) = self.definitions_to_declarations.remove(def_id)
                    && let Some(declaration) = self.declarations.get_mut(&declaration_id)
                    && declaration.remove_definition(def_id)
                {
                    if declaration.as_namespace().is_some() {
                        declarations_to_invalidate_ancestor_chains.push(declaration_id);
                    }

                    if declaration.has_no_definitions() {
                        let unqualified_str_id = StringId::from(&declaration.unqualified_name());
                        members_to_delete.push((*declaration.owner_id(), unqualified_str_id));
                        declarations_to_delete.push(declaration_id);
                    }
                }

                if let Some(name_id) = definition.name_id() {
                    self.untrack_name(*name_id);
                }
            }
        }

        self.invalidate_ancestor_chains(declarations_to_invalidate_ancestor_chains);

        for declaration_id in declarations_to_delete {
            self.declarations.remove(&declaration_id);
        }

        // Clean up any members that pointed to declarations that were removed
        for (owner_id, member_str_id) in members_to_delete {
            // Remove the `if` and use `unwrap` once we are indexing RBS files to have `Object`
            if let Some(owner) = self.declarations.get_mut(&owner_id) {
                match owner {
                    Declaration::Namespace(Namespace::Class(owner)) => {
                        owner.remove_member(&member_str_id);
                    }
                    Declaration::Namespace(Namespace::SingletonClass(owner)) => {
                        owner.remove_member(&member_str_id);
                    }
                    Declaration::Namespace(Namespace::Module(owner)) => {
                        owner.remove_member(&member_str_id);
                    }
                    _ => {} // Nothing happens
                }
            }
        }
    }

    fn invalidate_ancestor_chains(&self, initial_ids: Vec<DeclarationId>) {
        let declarations = &self.declarations;
        let mut queue = initial_ids;
        let mut visited = IdentityHashSet::<DeclarationId>::default();

        while let Some(declaration_id) = queue.pop() {
            if !visited.insert(declaration_id) {
                continue;
            }

            let Some(decl) = declarations.get(&declaration_id) else {
                continue;
            };

            let namespace = decl.as_namespace().expect("expected namespace declaration");

            namespace.for_each_ancestor(|ancestor| {
                if let Ancestor::Complete(ancestor_id) = ancestor
                    && let Some(ancestor_decl) = declarations.get(ancestor_id)
                {
                    ancestor_decl
                        .as_namespace()
                        .expect("expected namespace declaration")
                        .remove_descendant(&declaration_id);
                }
            });

            namespace.for_each_descendant(|descendant_id| {
                queue.push(*descendant_id);
            });

            namespace.clear_ancestors();
            namespace.clear_descendants();
        }
    }

    /// Sets the encoding that should be used for transforming byte offsets into LSP code unit line/column positions
    pub fn set_encoding(&mut self, encoding: Encoding) {
        self.position_encoding = encoding;
    }

    #[must_use]
    pub fn encoding(&self) -> &Encoding {
        &self.position_encoding
    }

    /// Asserts that the index is in a valid state.
    #[cfg(test)]
    pub fn assert_integrity(&self) {
        // Self::integrity_checker().assert_integrity(self);
    }

    // #[allow(clippy::too_many_lines)]
    // #[must_use]
    // pub fn integrity_checker() -> IntegrityChecker {
    //     let mut checker = IntegrityChecker::new();

    //     checker.add_rule("Each `declaration` has at least one definition", |index, errors| {
    //         for declaration in index.declarations().values() {
    //             if declaration.name() != "<main>" && declaration.definitions().is_empty() {
    //                 errors.push(format!(
    //                     "Declaration '{}' exists in `declarations`, but is not associated to any definitions",
    //                     declaration.name()
    //                 ));
    //             }
    //         }
    //     });

    //     checker.add_rule(
    //         "Each `definition` declaration_id is registered in `declarations`",
    //         |index, errors| {
    //             for definition_id in index.definitions().keys() {
    //                 let declaration_id = index.definitions_to_declarations.get(definition_id).unwrap();
    //                 if !index.declarations().contains_key(declaration_id) {
    //                     errors.push(format!(
    //                         "Definition '{definition_id}' is registered in `definitions` but not in `declarations`"
    //                     ));
    //                 }
    //             }
    //         },
    //     );

    //     checker.add_rule(
    //         "Each `declaration` definition is registered in `definitions`",
    //         |index, errors| {
    //             for declaration in index.declarations().values() {
    //                 for definition_id in declaration.definitions() {
    //                     if !index.definitions().contains_key(definition_id) {
    //                         errors.push(format!(
    //                             "Definition '{definition_id}' exists in `declarations` but not in `definitions`"
    //                         ));
    //                     }
    //                 }
    //             }
    //         },
    //     );

    //     checker.add_rule("Each `definition` URI is registered in `documents`", |index, errors| {
    //         for definition in index.definitions().values() {
    //             let uri_id = definition.uri_id();
    //             if !index.documents().contains_key(uri_id) {
    //                 errors.push(format!(
    //                     "URI id '{uri_id}' is registered in `definitions` but not in `documents`"
    //                 ));
    //             }
    //         }
    //     });

    //     checker.add_rule(
    //         "Each `document` definition is registered in `definitions`",
    //         |index, errors| {
    //             for document in index.documents().values() {
    //                 for definition_id in document.definitions() {
    //                     if !index.definitions().contains_key(definition_id) {
    //                         errors.push(format!(
    //                             "Definition '{definition_id}' is registered in `uris_to_definitions` but not in `definitions`"
    //                         ));
    //                     }
    //                 }
    //             }
    //         },
    //     );

    //     checker.add_rule("Each `definitions` URI is registered in `uri_pool`", |index, errors| {
    //         for definition in index.definitions().values() {
    //             let uri_id = definition.uri_id();
    //             if !index.documents().contains_key(uri_id) {
    //                 errors.push(format!(
    //                     "URI id '{uri_id}' is referenced by a definition but not present in `uri_pool`"
    //                 ));
    //             }
    //         }
    //     });

    //     checker
    // }

    /// # Panics
    ///
    /// Panics if acquiring a read lock fails
    #[allow(clippy::cast_precision_loss)]
    pub fn print_query_statistics(&self) {
        use std::collections::HashMap;

        let mut declarations_with_docs = 0;
        let mut total_doc_size = 0;
        let mut declarations_types: HashMap<&str, usize> = HashMap::new();
        let mut definition_types: HashMap<&str, usize> = HashMap::new();
        let mut multi_definition_count = 0;

        for declaration in self.declarations.values() {
            // Check documentation
            if let Some(definitions) = self.get(declaration.name()) {
                let has_docs = definitions.iter().any(|def| !def.comments().is_empty());
                if has_docs {
                    declarations_with_docs += 1;
                    let doc_size: usize = definitions.iter().map(|def| def.comments().len()).sum();
                    total_doc_size += doc_size;
                }
            }

            *declarations_types.entry(declaration.kind()).or_insert(0) += 1;

            // Count definitions by type
            let definition_count = declaration.definitions().len();
            if definition_count > 1 {
                multi_definition_count += 1;
            }

            for def_id in declaration.definitions() {
                if let Some(def) = self.definitions().get(def_id) {
                    *definition_types.entry(def.kind()).or_insert(0) += 1;
                }
            }
        }

        println!();
        println!("Query statistics");
        let total_declarations = self.declarations.len();
        println!("  Total declarations:         {total_declarations}");
        println!(
            "  With documentation:         {} ({:.1}%)",
            declarations_with_docs,
            stats::percentage(declarations_with_docs, total_declarations)
        );
        println!(
            "  Without documentation:      {} ({:.1}%)",
            total_declarations - declarations_with_docs,
            stats::percentage(total_declarations - declarations_with_docs, total_declarations)
        );
        println!("  Total documentation size:   {total_doc_size} bytes");
        println!(
            "  Multi-definition names:     {} ({:.1}%)",
            multi_definition_count,
            stats::percentage(multi_definition_count, total_declarations)
        );

        println!();
        println!("Declaration breakdown:");
        let mut types: Vec<_> = declarations_types.iter().collect();
        types.sort_by_key(|(_, count)| std::cmp::Reverse(**count));
        for (kind, count) in types {
            println!("  {kind:20} {count:6}");
        }

        println!();
        println!("Definition breakdown:");
        let mut types: Vec<_> = definition_types.iter().collect();
        types.sort_by_key(|(_, count)| std::cmp::Reverse(**count));
        for (kind, count) in types {
            println!("  {kind:20} {count:6}");
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::model::comment::Comment;
    use crate::model::declaration::Ancestors;
    use crate::test_utils::GraphTest;

    #[test]
    fn deleting_a_uri() {
        let mut context = GraphTest::new();

        context.index_uri("file:///foo.rb", "module Foo; end");
        context.delete_uri("file:///foo.rb");
        context.resolve();

        assert!(context.graph().documents.is_empty());
        assert!(context.graph().definitions.is_empty());
        // Object is left
        assert!(
            context
                .graph()
                .declarations()
                .get(&DeclarationId::from("Foo"))
                .is_none()
        );

        context.graph().assert_integrity();
    }

    #[test]
    fn updating_index_with_deleted_definitions() {
        let mut context = GraphTest::new();

        context.index_uri("file:///foo.rb", "module Foo; end");

        assert_eq!(context.graph().definitions.len(), 1);
        assert_eq!(context.graph().documents.len(), 1);

        // Update with empty content to remove definitions but keep the URI
        context.index_uri("file:///foo.rb", "");

        assert!(context.graph().definitions.is_empty());

        // URI remains if the file was not deleted, but definitions got erased
        assert_eq!(context.graph().documents.len(), 1);
    }

    #[test]
    fn updating_index_with_deleted_definitions_after_resolution() {
        let mut context = GraphTest::new();

        context.index_uri("file:///foo.rb", "module Foo; end");
        context.resolve();

        assert_eq!(context.graph().definitions.len(), 1);
        assert_eq!(context.graph().definitions_to_declarations().len(), 1);
        assert_eq!(context.graph().documents.len(), 1);

        {
            assert!(
                context
                    .graph()
                    .declarations()
                    .get(&DeclarationId::from("Foo"))
                    .is_some()
            );
        }

        // Update with empty content to remove definitions but keep the URI
        context.index_uri("file:///foo.rb", "");

        assert!(context.graph().definitions.is_empty());
        assert!(context.graph().definitions_to_declarations().is_empty());
        // URI remains if the file was not deleted, but definitions and declarations got erased
        assert_eq!(context.graph().documents.len(), 1);

        {
            assert!(
                context
                    .graph()
                    .declarations()
                    .get(&DeclarationId::from("Foo"))
                    .is_none()
            );
        }

        context.graph().assert_integrity();
    }

    #[test]
    fn updating_index_with_deleted_references() {
        let mut context = GraphTest::new();

        context.index_uri("file:///definition.rb", "module Foo; end");
        context.index_uri(
            "file:///references.rb",
            r"
            Foo
            bar
            BAZ
            ",
        );
        context.resolve();

        assert_eq!(context.graph().documents.len(), 2);
        assert_eq!(context.graph().method_references.len(), 1);
        assert_eq!(context.graph().constant_references.len(), 2);
        {
            let declaration = context.graph().declarations().get(&DeclarationId::from("Foo")).unwrap();
            assert_eq!(declaration.references().len(), 1);
        }

        // Update with empty content to remove definitions but keep the URI
        context.index_uri("file:///references.rb", "");

        // URI remains if the file was not deleted, but references got erased
        assert_eq!(context.graph().documents.len(), 2);
        assert!(context.graph().method_references.is_empty());
        assert!(context.graph().constant_references.is_empty());
        {
            let declaration = context.graph().declarations().get(&DeclarationId::from("Foo")).unwrap();
            assert!(declaration.references().is_empty());
        }

        context.graph().assert_integrity();
    }

    #[test]
    fn invalidating_ancestor_chains_when_document_changes() {
        let mut context = GraphTest::new();

        context.index_uri("file:///a.rb", "class Foo; include Bar; def method_name; end; end");
        context.index_uri("file:///b.rb", "class Foo; end");
        context.index_uri("file:///c.rb", "module Bar; end");
        context.index_uri("file:///d.rb", "class Baz < Foo; end");
        context.resolve();

        let foo_declaration = context.graph().declarations().get(&DeclarationId::from("Foo")).unwrap();
        assert!(matches!(
            foo_declaration.as_namespace().unwrap().ancestors(),
            Ancestors::Complete(_)
        ));

        let baz_declaration = context.graph().declarations().get(&DeclarationId::from("Baz")).unwrap();
        assert!(matches!(
            baz_declaration.as_namespace().unwrap().ancestors(),
            Ancestors::Complete(_)
        ));

        {
            let Declaration::Namespace(Namespace::Module(bar)) =
                context.graph().declarations().get(&DeclarationId::from("Bar")).unwrap()
            else {
                panic!("Expected Bar to be a module");
            };
            assert!(bar.descendants().contains(&DeclarationId::from("Foo")));

            let Declaration::Namespace(Namespace::Class(foo)) =
                context.graph().declarations().get(&DeclarationId::from("Foo")).unwrap()
            else {
                panic!("Expected Foo to be a class");
            };
            assert!(foo.descendants().contains(&DeclarationId::from("Baz")));
        }

        context.index_uri("file:///a.rb", "");

        {
            let Declaration::Namespace(Namespace::Class(foo)) =
                context.graph().declarations().get(&DeclarationId::from("Foo")).unwrap()
            else {
                panic!("Expected Foo to be a class");
            };
            assert!(matches!(foo.clone_ancestors(), Ancestors::Partial(a) if a.is_empty()));
            assert!(foo.descendants().is_empty());

            let Declaration::Namespace(Namespace::Class(baz)) =
                context.graph().declarations().get(&DeclarationId::from("Baz")).unwrap()
            else {
                panic!("Expected Baz to be a class");
            };
            assert!(matches!(baz.clone_ancestors(), Ancestors::Partial(a) if a.is_empty()));
            assert!(baz.descendants().is_empty());

            let Declaration::Namespace(Namespace::Module(bar)) =
                context.graph().declarations().get(&DeclarationId::from("Bar")).unwrap()
            else {
                panic!("Expected Bar to be a module");
            };
            assert!(!bar.descendants().contains(&DeclarationId::from("Foo")));
        }

        context.resolve();

        let baz_declaration = context.graph().declarations().get(&DeclarationId::from("Baz")).unwrap();
        assert!(matches!(
            baz_declaration.as_namespace().unwrap().ancestors(),
            Ancestors::Complete(_)
        ));

        {
            let Declaration::Namespace(Namespace::Class(foo)) =
                context.graph().declarations().get(&DeclarationId::from("Foo")).unwrap()
            else {
                panic!("Expected Foo to be a class");
            };
            assert!(foo.descendants().contains(&DeclarationId::from("Baz")));
        }
    }

    #[test]
    fn name_count_increments_for_duplicates() {
        let mut context = GraphTest::new();

        context.index_uri("file:///foo.rb", "module Foo; end");
        context.index_uri("file:///foo2.rb", "module Foo; end");
        context.index_uri("file:///foo3.rb", "Foo");
        context.resolve();

        assert_eq!(context.graph().names().len(), 1);
        let name_ref = context.graph().names().values().next().unwrap();
        assert_eq!(name_ref.ref_count(), 3);
    }

    #[test]
    fn updating_index_with_deleted_names() {
        let mut context = GraphTest::new();

        context.index_uri("file:///foo.rb", "module Foo; end");
        context.index_uri("file:///bar.rb", "Foo");
        context.resolve();

        assert_eq!(context.graph().names().len(), 1);
        assert_eq!(context.graph().names().values().next().unwrap().ref_count(), 2);

        context.delete_uri("file:///foo.rb");
        assert_eq!(context.graph().names().len(), 1);
        assert_eq!(context.graph().names().values().next().unwrap().ref_count(), 1);

        context.delete_uri("file:///bar.rb");
        assert!(context.graph().names().is_empty());
    }

    #[test]
    fn updating_index_with_new_definitions() {
        let mut context = GraphTest::new();

        context.index_uri("file:///foo.rb", "module Foo; end");
        context.resolve();

        assert_eq!(context.graph().definitions.len(), 1);
        let declaration = context.graph().declarations().get(&DeclarationId::from("Foo")).unwrap();
        assert_eq!(declaration.name(), "Foo");
        let document = context.graph().documents.get(&UriId::from("file:///foo.rb")).unwrap();
        assert_eq!(document.uri(), "file:///foo.rb");
        assert_eq!(declaration.definitions().len(), 1);
        assert_eq!(document.definitions().len(), 1);

        context.graph().assert_integrity();
    }

    #[test]
    fn updating_existing_definitions() {
        let mut context = GraphTest::new();

        context.index_uri("file:///foo.rb", "module Foo; end");
        // Update with the same definition but at a different position (with content before it)
        context.index_uri("file:///foo.rb", "\n\n\n\n\n\nmodule Foo; end");
        context.resolve();

        assert_eq!(context.graph().definitions.len(), 1);
        let declaration = context.graph().declarations().get(&DeclarationId::from("Foo")).unwrap();
        assert_eq!(declaration.name(), "Foo");
        assert_eq!(
            context
                .graph()
                .documents()
                .get(&UriId::from("file:///foo.rb"))
                .unwrap()
                .uri(),
            "file:///foo.rb"
        );

        let definitions = context.graph().get("Foo").unwrap();
        assert_eq!(definitions.len(), 1);
        assert_eq!(definitions[0].offset().start(), 6);

        context.graph().assert_integrity();
    }

    #[test]
    fn adding_another_definition_from_a_different_uri() {
        let mut context = GraphTest::new();

        context.index_uri("file:///foo.rb", "module Foo; end");
        context.index_uri("file:///foo2.rb", "\n\n\n\n\nmodule Foo; end");
        context.resolve();

        let definitions = context.graph().get("Foo").unwrap();
        let mut offsets = definitions.iter().map(|d| d.offset().start()).collect::<Vec<_>>();
        offsets.sort_unstable();
        assert_eq!(definitions.len(), 2);
        assert_eq!(vec![0, 5], offsets);

        context.graph().assert_integrity();
    }

    #[test]
    fn adding_a_second_definition_from_the_same_uri() {
        let mut context = GraphTest::new();

        context.index_uri("file:///foo.rb", "module Foo; end");

        // Update with multiple definitions of the same module in one file
        context.index_uri("file:///foo.rb", {
            "
            module Foo; end


            module Foo; end
            "
        });

        context.resolve();

        let definitions = context.graph().get("Foo").unwrap();
        assert_eq!(definitions.len(), 2);

        let mut offsets = definitions
            .iter()
            .map(|d| [d.offset().start(), d.offset().end()])
            .collect::<Vec<_>>();
        offsets.sort_unstable();
        assert_eq!([0, 15], offsets[0]);
        assert_eq!([18, 33], offsets[1]);

        context.graph().assert_integrity();
    }

    #[test]
    fn get_documentation() {
        let mut context = GraphTest::new();

        context.index_uri("file:///foo.rb", {
            "
            # This is a class comment
            # Multi-line comment
            class CommentedClass; end

            # Module comment
            module CommentedModule; end

            class NoCommentClass; end
            "
        });

        context.resolve();

        let definitions = context.graph().get("CommentedClass").unwrap();
        let def = definitions.first().unwrap();
        assert_eq!(
            def.comments().iter().map(Comment::string).collect::<Vec<&String>>(),
            vec!["# This is a class comment", "# Multi-line comment"]
        );

        let definitions = context.graph().get("CommentedModule").unwrap();
        let def = definitions.first().unwrap();
        assert_eq!(
            def.comments().iter().map(Comment::string).collect::<Vec<&String>>(),
            vec!["# Module comment"]
        );

        let definitions = context.graph().get("NoCommentClass").unwrap();
        let def = definitions.first().unwrap();
        assert!(def.comments().is_empty());
    }

    #[test]
    fn members_are_updated_when_definitions_get_deleted() {
        let mut context = GraphTest::new();
        // Initially, have `Foo` defined twice with a member called `Bar`
        context.index_uri("file:///foo.rb", {
            r"
            module Foo
            end
            "
        });
        context.index_uri("file:///foo2.rb", {
            r"
            module Foo
              class Bar; end
            end
            "
        });
        context.resolve();

        {
            if let Declaration::Namespace(Namespace::Module(foo)) =
                context.graph().declarations().get(&DeclarationId::from("Foo")).unwrap()
            {
                assert!(foo.members().contains_key(&StringId::from("Bar")));
            } else {
                panic!("Expected Foo to be a module");
            }
        }

        // Delete `Bar`
        context.index_uri("file:///foo2.rb", {
            r"
            module Foo
            end
            "
        });
        context.resolve();

        if let Declaration::Namespace(Namespace::Module(foo)) =
            context.graph().declarations().get(&DeclarationId::from("Foo")).unwrap()
        {
            assert!(!foo.members().contains_key(&StringId::from("Bar")));
        } else {
            panic!("Expected Foo to be a module");
        }
    }

    #[test]
    fn diagnostics_are_collected() {
        let mut context = GraphTest::new();

        context.index_uri("file:///foo1.rb", {
            r"
            class Foo
            "
        });

        context.index_uri("file:///foo2.rb", {
            r"
            foo = 42
            "
        });

        let mut diagnostics = context
            .graph()
            .diagnostics()
            .iter()
            .map(|d| {
                format!(
                    "{}: {} ({})",
                    d.rule(),
                    d.message(),
                    context.graph().documents().get(d.uri_id()).unwrap().uri()
                )
            })
            .collect::<Vec<_>>();

        diagnostics.sort();

        assert_eq!(
            vec![
                "parse-error: expected an `end` to close the `class` statement (file:///foo1.rb)",
                "parse-error: unexpected end-of-input, assuming it is closing the parent top level context (file:///foo1.rb)",
                "parse-warning: assigned but unused variable - foo (file:///foo2.rb)",
            ],
            diagnostics,
        );
    }
}
