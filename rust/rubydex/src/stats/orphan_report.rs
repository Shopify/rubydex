use std::collections::HashSet;
use std::io::Write;

use crate::model::declaration::Declaration;
use crate::model::definitions::Definition;
use crate::model::graph::Graph;
use crate::model::ids::{DefinitionId, NameId, StringId};
use crate::model::name::{NameRef, ParentScope};

impl Graph {
    /// Writes a report of orphan definitions (definitions not linked to any declaration).
    ///
    /// Format: `type\tconcatenated_name\tlocation` (TSV)
    ///
    /// # Errors
    ///
    /// Returns an error if writing fails.
    pub fn write_orphan_report(&self, writer: &mut impl Write) -> std::io::Result<()> {
        // Collect all definition IDs that are linked to declarations
        let linked_definition_ids: HashSet<&DefinitionId> = self
            .declarations()
            .values()
            .flat_map(Declaration::definitions)
            .collect();

        // Find orphan definitions
        let mut orphans: Vec<_> = self
            .definitions()
            .iter()
            .filter(|(id, _)| !linked_definition_ids.contains(id))
            .collect();

        // Sort by type, then by location for consistent output
        orphans.sort_by(|(_, a), (_, b)| {
            a.kind()
                .cmp(&b.kind())
                .then_with(|| a.uri_id().cmp(b.uri_id()))
                .then_with(|| a.offset().cmp(b.offset()))
        });

        for (_, definition) in orphans {
            let kind = definition.kind();
            let name = match definition.name_id().copied() {
                Some(id) => self.build_concatenated_name_from_name(id),
                None => self.build_concatenated_name_from_lexical_nesting(definition),
            };
            let location = self.definition_location(definition);

            writeln!(writer, "{kind}\t{name}\t{location}")?;
        }

        Ok(())
    }

    /// Walks the Name system's `parent_scope` chain to reconstruct the constant path.
    /// Falls back to `nesting` for enclosing scope context when there is no explicit parent scope.
    ///
    /// Note: this produces a concatenated name by piecing together name parts, not a properly
    /// resolved qualified name.
    pub(crate) fn build_concatenated_name_from_name(&self, name_id: NameId) -> String {
        let Some(name_ref) = self.names().get(&name_id) else {
            return "<unknown>".to_string();
        };
        let simple_name = self.string_id_to_string(*name_ref.str());

        match name_ref.parent_scope() {
            ParentScope::Some(parent_id) | ParentScope::Attached(parent_id) => {
                let parent_name = self.build_concatenated_name_from_name(*parent_id);
                format!("{parent_name}::{simple_name}")
            }
            ParentScope::TopLevel => format!("::{simple_name}"),
            ParentScope::None => {
                let prefix = name_ref
                    .nesting()
                    .as_ref()
                    .map(|nesting_id| self.build_nesting_prefix(*nesting_id))
                    .unwrap_or_default();

                if prefix.is_empty() {
                    simple_name
                } else {
                    format!("{prefix}::{simple_name}")
                }
            }
        }
    }

    /// Resolves the enclosing nesting `NameId` to a string prefix.
    /// For resolved names, uses the declaration's fully qualified name.
    /// For unresolved names, recursively walks the name chain.
    fn build_nesting_prefix(&self, nesting_id: NameId) -> String {
        let Some(name_ref) = self.names().get(&nesting_id) else {
            return String::new();
        };
        match name_ref {
            NameRef::Resolved(resolved) => self
                .declarations()
                .get(resolved.declaration_id())
                .map_or_else(String::new, |decl| decl.name().to_string()),
            NameRef::Unresolved(_) => self.build_concatenated_name_from_name(nesting_id),
        }
    }

    /// Builds a concatenated name for non-constant definitions by walking the `lexical_nesting_id` chain.
    ///
    /// Note: this pieces together name parts from the lexical nesting, not a properly resolved
    /// qualified name.
    pub(crate) fn build_concatenated_name_from_lexical_nesting(&self, definition: &Definition) -> String {
        let simple_name = self.string_id_to_string(self.definition_string_id(definition));

        // Collect enclosing nesting names from inner to outer
        let mut nesting_parts = Vec::new();
        let mut current_nesting = *definition.lexical_nesting_id();

        while let Some(nesting_id) = current_nesting {
            let Some(nesting_def) = self.definitions().get(&nesting_id) else {
                break;
            };
            nesting_parts.push(self.string_id_to_string(self.definition_string_id(nesting_def)));
            current_nesting = *nesting_def.lexical_nesting_id();
        }

        if nesting_parts.is_empty() {
            return simple_name;
        }

        // Reverse to get outer-to-inner order for the prefix
        nesting_parts.reverse();
        let prefix = nesting_parts.join("::");

        let separator = match definition {
            Definition::Method(_)
            | Definition::AttrAccessor(_)
            | Definition::AttrReader(_)
            | Definition::AttrWriter(_)
            | Definition::MethodAlias(_)
            | Definition::InstanceVariable(_) => "#",
            Definition::Class(_)
            | Definition::SingletonClass(_)
            | Definition::Module(_)
            | Definition::Constant(_)
            | Definition::ConstantAlias(_)
            | Definition::GlobalVariable(_)
            | Definition::ClassVariable(_)
            | Definition::GlobalVariableAlias(_) => "::",
        };

        format!("{prefix}{separator}{simple_name}")
    }

    /// Converts a `StringId` to its string value.
    fn string_id_to_string(&self, string_id: StringId) -> String {
        self.strings().get(&string_id).unwrap().to_string()
    }

    /// Get location in the format of `uri#L<line>` for a definition.
    /// The format is clickable in VS Code.
    pub(crate) fn definition_location(&self, definition: &Definition) -> String {
        let uri_id = definition.uri_id();

        let Some(document) = self.documents().get(uri_id) else {
            return format!("{uri_id}:<unknown>");
        };

        let uri = document.uri();
        let line_index = document.line_index();
        let start = line_index.line_col(definition.offset().start().into());
        format!("{uri}#L{}", start.line + 1)
    }
}

#[cfg(test)]
mod tests {
    use crate::model::definitions::DefinitionKind;
    use crate::test_utils::GraphTest;

    #[test]
    fn build_concatenated_name_from_name_for_constants() {
        let cases = vec![
            ("class Foo; end", "Foo"),
            ("module Foo; class Bar; end; end", "Foo::Bar"),
            ("module Foo; module Bar; class Baz; end; end; end", "Foo::Bar::Baz"),
        ];

        for (source, expected_name) in cases {
            let mut context = GraphTest::new();
            context.index_uri("file:///test.rb", source);
            context.resolve();

            let definitions = context.graph().get(expected_name).unwrap();
            let definition = definitions.first().unwrap();
            let name_id = *definition.name_id().unwrap();
            let actual = context.graph().build_concatenated_name_from_name(name_id);

            assert_eq!(actual, expected_name, "For source: {source}");
        }
    }

    #[test]
    fn build_concatenated_name_from_lexical_nesting_for_methods() {
        let cases = vec![
            ("class Foo; def bar; end; end", "Foo#bar()"),
            ("module Foo; class Bar; def baz; end; end; end", "Foo::Bar#baz()"),
            ("def bar; end", "bar()"),
        ];

        for (source, expected_name) in cases {
            let mut context = GraphTest::new();
            // Index without resolution so methods remain orphans
            context.index_uri("file:///test.rb", source);

            let definition = context
                .graph()
                .definitions()
                .values()
                .find(|d| d.kind() == DefinitionKind::Method && d.name_id().is_none())
                .unwrap_or_else(|| panic!("No Method definition without name_id found for source: {source}"));

            let actual = context.graph().build_concatenated_name_from_lexical_nesting(definition);
            assert_eq!(actual, expected_name, "For source: {source}");
        }
    }

    #[test]
    fn build_concatenated_name_from_lexical_nesting_for_instance_variables() {
        let mut context = GraphTest::new();
        context.index_uri("file:///test.rb", "class Foo; def initialize; @ivar = 1; end; end");

        let definition = context
            .graph()
            .definitions()
            .values()
            .find(|d| d.kind() == DefinitionKind::InstanceVariable)
            .unwrap();

        let actual = context.graph().build_concatenated_name_from_lexical_nesting(definition);
        assert_eq!(actual, "Foo::initialize()#@ivar");
    }

    #[test]
    fn definition_location_uses_clickable_uri_fragment() {
        let mut context = GraphTest::new();
        context.index_uri(
            "file:///foo.rb",
            "
            class Foo
              def bar
              end
            end
            ",
        );

        let definition = context
            .graph()
            .definitions()
            .values()
            .find(|d| d.kind() == DefinitionKind::Method)
            .unwrap();

        let actual = context.graph().definition_location(definition);
        assert_eq!(actual, "file:///foo.rb#L2");
    }
}
