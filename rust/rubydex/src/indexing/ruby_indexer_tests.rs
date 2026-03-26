use crate::{
    assert_def_comments_eq, assert_def_mixins_eq, assert_def_name_eq, assert_def_name_offset_eq, assert_def_str_eq,
    assert_def_superclass_ref_eq, assert_definition_at, assert_dependents, assert_local_diagnostics_eq,
    assert_method_has_receiver, assert_name_path_eq, assert_no_local_diagnostics, assert_string_eq,
    model::{
        definitions::{Definition, Parameter, Receiver, Signatures},
        ids::{StringId, UriId},
        visibility::Visibility,
    },
    test_utils::LocalGraphTest,
};

// Method assertions

/// Asserts that a parameter matches the expected kind.
///
/// Usage:
/// - `assert_parameter!(parameter, RequiredPositional, |param| { assert_string_eq!(context, param.str(), "a"); })`
/// - `assert_parameter!(parameter, OptionalPositional, |param| { assert_string_eq!(context, param.str(), "b"); })`
macro_rules! assert_parameter {
    ($expr:expr, $variant:ident, |$param:ident| $body:block) => {
        match $expr {
            Parameter::$variant($param) => $body,
            _ => panic!(
                "parameter kind mismatch: expected `{}`, got `{:?}`",
                stringify!($variant),
                $expr
            ),
        }
    };
}

/// Asserts that a method has a simple (non-overloaded) signature, then runs a closure with it.
///
/// Usage:
/// - `assert_simple_signature!(def, |params| { assert_eq!(params.len(), 2); })`
macro_rules! assert_simple_signature {
    ($def:expr, |$params:ident| $body:block) => {
        match $def.signatures() {
            Signatures::Simple($params) => $body,
            other => panic!("expected Simple signature, got {:?}", other),
        }
    };
}

// Reference assertions

macro_rules! assert_constant_references_eq {
    ($context:expr, $expected_names:expr) => {{
        let mut actual_references = $context
            .graph()
            .constant_references()
            .values()
            .map(|r| {
                let name = $context.graph().names().get(r.name_id()).unwrap();
                (
                    r.offset().start(),
                    $context.graph().strings().get(name.str()).unwrap().as_str(),
                )
            })
            .collect::<Vec<_>>();

        actual_references.sort();

        let actual_names = actual_references.iter().map(|(_, name)| *name).collect::<Vec<_>>();

        assert_eq!(
            $expected_names,
            actual_names.as_slice(),
            "constant references mismatch: expected `{:?}`, got `{:?}`",
            $expected_names,
            actual_names
        );
    }};
}

macro_rules! assert_method_references_eq {
    ($context:expr, $expected_names:expr) => {{
        let mut actual_references = $context
            .graph()
            .method_references()
            .values()
            .map(|m| {
                (
                    m.offset().start(),
                    $context.graph().strings().get(m.str()).unwrap().as_str(),
                )
            })
            .collect::<Vec<_>>();

        actual_references.sort();

        let actual_names = actual_references
            .iter()
            .map(|(_offset, name)| *name)
            .collect::<Vec<_>>();

        assert_eq!(
            $expected_names,
            actual_names.as_slice(),
            "method references mismatch: expected `{:?}`, got `{:?}`",
            $expected_names,
            actual_names
        );
    }};
}

/// Asserts that exactly one method reference with the given name has the expected receiver.
///
/// Panics if there isn't exactly one `MethodRef` with that name.
///
/// Usage:
/// - `assert_method_ref_receiver!(context, "bar", "<Foo>")`
macro_rules! assert_method_ref_receiver {
    ($context:expr, $method_name:expr, $expected_receiver:expr) => {{
        let target = StringId::from($method_name);
        let matches: Vec<_> = $context
            .graph()
            .method_references()
            .values()
            .filter(|method_ref| *method_ref.str() == target)
            .collect();

        assert_eq!(
            matches.len(),
            1,
            "expected exactly one method reference for `{}`, found {}",
            $method_name,
            matches.len()
        );

        let method_ref = matches[0];
        let receiver_id = method_ref
            .receiver()
            .unwrap_or_else(|| panic!("method reference for `{}` has no receiver", $method_name));
        let receiver = $context.graph().names().get(&receiver_id).unwrap();

        assert_eq!(
            StringId::from($expected_receiver),
            *receiver.str(),
            "receiver mismatch for `{}`: expected `{}`, got `{}`",
            $method_name,
            $expected_receiver,
            $context.graph().strings().get(receiver.str()).unwrap().as_str()
        );
    }};
}

macro_rules! assert_promotable {
    ($def:expr) => {{
        assert!(
            $def.flags().is_promotable(),
            "expected definition to be promotable, but it was not"
        );
    }};
}

macro_rules! assert_not_promotable {
    ($def:expr) => {{
        assert!(
            !$def.flags().is_promotable(),
            "expected definition to not be promotable, but it was"
        );
    }};
}

fn index_source(source: &str) -> LocalGraphTest {
    LocalGraphTest::new("file:///foo.rb", source)
}

#[test]
fn index_source_with_errors() {
    let context = index_source({
        "
        class Foo
        "
    });

    assert_local_diagnostics_eq!(
        &context,
        [
            "parse-error: expected an `end` to close the `class` statement (1:1-1:6)",
            "parse-error: unexpected end-of-input, assuming it is closing the parent top level context (1:10-2:1)"
        ]
    );

    // We still index the definition, even though it has errors
    assert_eq!(context.graph().definitions().len(), 1);
    assert_definition_at!(&context, "1:1-2:1", Class, |def| {
        assert_def_name_eq!(&context, def, "Foo");
    });
}

#[test]
fn index_source_with_warnings() {
    let context = index_source({
        "
        foo = 42
        "
    });

    assert_local_diagnostics_eq!(
        &context,
        ["parse-warning: assigned but unused variable - foo (1:1-1:4)"]
    );
}

#[test]
fn index_class_node() {
    let context = index_source({
        "
        class Foo
          class Bar
            class Baz; end
          end
        end
        "
    });

    assert_no_local_diagnostics!(&context);
    assert_eq!(context.graph().definitions().len(), 3);

    assert_definition_at!(&context, "1:1-5:4", Class, |def| {
        assert_def_name_eq!(&context, def, "Foo");
        assert_def_name_offset_eq!(&context, def, "1:7-1:10");
        assert!(def.superclass_ref().is_none());
        assert_eq!(1, def.members().len());
        assert!(def.lexical_nesting_id().is_none());
    });

    assert_definition_at!(&context, "2:3-4:6", Class, |def| {
        assert_def_name_eq!(&context, def, "Bar");
        assert_def_name_offset_eq!(&context, def, "2:9-2:12");
        assert!(def.superclass_ref().is_none());
        assert_eq!(1, def.members().len());

        assert_definition_at!(&context, "1:1-5:4", Class, |parent_nesting| {
            assert_eq!(parent_nesting.id(), def.lexical_nesting_id().unwrap());
            assert_eq!(parent_nesting.members()[0], def.id());
        });
    });

    assert_definition_at!(&context, "3:5-3:19", Class, |def| {
        assert_def_name_eq!(&context, def, "Baz");
        assert_def_name_offset_eq!(&context, def, "3:11-3:14");
        assert!(def.superclass_ref().is_none());
        assert!(def.members().is_empty());

        assert_definition_at!(&context, "2:3-4:6", Class, |parent_nesting| {
            assert_eq!(parent_nesting.id(), def.lexical_nesting_id().unwrap());
            assert_eq!(parent_nesting.members()[0], def.id());
        });
    });
}

#[test]
fn index_class_node_with_qualified_name() {
    let context = index_source({
        "
        class Foo::Bar
          class Baz::Qux
            class ::Quuux; end
          end
        end
        "
    });

    assert_no_local_diagnostics!(&context);
    assert_eq!(context.graph().definitions().len(), 3);

    assert_definition_at!(&context, "1:1-5:4", Class, |def| {
        assert_def_name_eq!(&context, def, "Foo::Bar");
        assert_def_name_offset_eq!(&context, def, "1:12-1:15");
        assert!(def.superclass_ref().is_none());
        assert!(def.lexical_nesting_id().is_none());
        assert_eq!(1, def.members().len());
    });

    assert_definition_at!(&context, "2:3-4:6", Class, |def| {
        assert_def_name_eq!(&context, def, "Baz::Qux");
        assert_def_name_offset_eq!(&context, def, "2:14-2:17");
        assert!(def.superclass_ref().is_none());
        assert_eq!(1, def.members().len());

        assert_definition_at!(&context, "1:1-5:4", Class, |parent_nesting| {
            assert_eq!(parent_nesting.id(), def.lexical_nesting_id().unwrap());
            assert_eq!(parent_nesting.members()[0], def.id());
        });
    });

    assert_definition_at!(&context, "3:5-3:23", Class, |def| {
        assert_def_name_eq!(&context, def, "Quuux");
        assert_def_name_offset_eq!(&context, def, "3:13-3:18");
        assert!(def.superclass_ref().is_none());
        assert!(def.members().is_empty());

        assert_definition_at!(&context, "2:3-4:6", Class, |parent_nesting| {
            assert_eq!(parent_nesting.id(), def.lexical_nesting_id().unwrap());
            assert_eq!(parent_nesting.members()[0], def.id());
        });
    });
}

#[test]
fn index_class_with_dynamic_names() {
    let context = index_source({
        "
        class foo::Bar
        end
        "
    });

    assert_local_diagnostics_eq!(
        &context,
        ["dynamic-constant-reference: Dynamic constant reference (1:7-1:10)"]
    );
    assert!(context.graph().definitions().is_empty());
}

#[test]
fn index_module_node() {
    let context = index_source({
        "
        module Foo
          module Bar
            module Baz; end
          end
        end
        "
    });

    assert_no_local_diagnostics!(&context);
    assert_eq!(context.graph().definitions().len(), 3);

    assert_definition_at!(&context, "1:1-5:4", Module, |def| {
        assert_def_name_eq!(&context, def, "Foo");
        assert_def_name_offset_eq!(&context, def, "1:8-1:11");
        assert_eq!(1, def.members().len());
        assert!(def.lexical_nesting_id().is_none());
    });

    assert_definition_at!(&context, "2:3-4:6", Module, |def| {
        assert_def_name_eq!(&context, def, "Bar");
        assert_def_name_offset_eq!(&context, def, "2:10-2:13");
        assert_eq!(1, def.members().len());

        assert_definition_at!(&context, "1:1-5:4", Module, |parent_nesting| {
            assert_eq!(parent_nesting.id(), def.lexical_nesting_id().unwrap());
            assert_eq!(parent_nesting.members()[0], def.id());
        });
    });

    assert_definition_at!(&context, "3:5-3:20", Module, |def| {
        assert_def_name_eq!(&context, def, "Baz");
        assert_def_name_offset_eq!(&context, def, "3:12-3:15");

        assert_definition_at!(&context, "2:3-4:6", Module, |parent_nesting| {
            assert_eq!(parent_nesting.id(), def.lexical_nesting_id().unwrap());
            assert_eq!(parent_nesting.members()[0], def.id());
        });
    });
}

#[test]
fn index_module_node_with_qualified_name() {
    let context = index_source({
        "
        module Foo::Bar
          module Baz::Qux
            module ::Quuux; end
          end
        end
        "
    });

    assert_no_local_diagnostics!(&context);
    assert_eq!(context.graph().definitions().len(), 3);

    assert_definition_at!(&context, "1:1-5:4", Module, |def| {
        assert_def_name_eq!(&context, def, "Foo::Bar");
        assert_def_name_offset_eq!(&context, def, "1:13-1:16");
        assert_eq!(1, def.members().len());
        assert!(def.lexical_nesting_id().is_none());
    });

    assert_definition_at!(&context, "2:3-4:6", Module, |def| {
        assert_def_name_eq!(&context, def, "Baz::Qux");
        assert_def_name_offset_eq!(&context, def, "2:15-2:18");
        assert_eq!(1, def.members().len());

        assert_definition_at!(&context, "1:1-5:4", Module, |parent_nesting| {
            assert_eq!(parent_nesting.id(), def.lexical_nesting_id().unwrap());
            assert_eq!(parent_nesting.members()[0], def.id());
        });
    });

    assert_definition_at!(&context, "3:5-3:24", Module, |def| {
        assert_def_name_eq!(&context, def, "Quuux");
        assert_def_name_offset_eq!(&context, def, "3:14-3:19");

        assert_definition_at!(&context, "2:3-4:6", Module, |parent_nesting| {
            assert_eq!(parent_nesting.id(), def.lexical_nesting_id().unwrap());
            assert_eq!(parent_nesting.members()[0], def.id());
        });
    });
}

#[test]
fn index_module_with_dynamic_names() {
    let context = index_source({
        "
        module foo::Bar
        end
        "
    });

    assert_local_diagnostics_eq!(
        &context,
        ["dynamic-constant-reference: Dynamic constant reference (1:8-1:11)"]
    );
    assert!(context.graph().definitions().is_empty());
}

#[test]
fn index_constant_write_node() {
    let context = index_source({
        "
        FOO = 1

        class Foo
          FOO = 2
        end
        "
    });

    assert_no_local_diagnostics!(&context);
    assert_eq!(context.graph().definitions().len(), 3);

    assert_definition_at!(&context, "1:1-1:4", Constant, |def| {
        assert_def_name_eq!(&context, def, "FOO");
        assert!(def.lexical_nesting_id().is_none());
    });

    assert_definition_at!(&context, "4:3-4:6", Constant, |def| {
        assert_def_name_eq!(&context, def, "FOO");

        assert_definition_at!(&context, "3:1-5:4", Class, |parent_nesting| {
            assert_eq!(parent_nesting.id(), def.lexical_nesting_id().unwrap());
            assert_eq!(parent_nesting.members()[0], def.id());
        });
    });
}

#[test]
fn index_constant_path_write_node() {
    let context = index_source({
        "
        FOO::BAR = 1

        class Foo
          FOO::BAR = 2
          ::BAZ = 3
        end
        "
    });

    assert_no_local_diagnostics!(&context);
    assert_eq!(context.graph().definitions().len(), 4);

    assert_definition_at!(&context, "1:6-1:9", Constant, |def| {
        assert_def_name_eq!(&context, def, "FOO::BAR");
        assert!(def.lexical_nesting_id().is_none());
    });

    assert_definition_at!(&context, "4:8-4:11", Constant, |def| {
        assert_def_name_eq!(&context, def, "FOO::BAR");

        assert_definition_at!(&context, "3:1-6:4", Class, |parent_nesting| {
            assert_eq!(parent_nesting.id(), def.lexical_nesting_id().unwrap());
            assert_eq!(parent_nesting.members()[0], def.id());
        });
    });

    assert_definition_at!(&context, "5:5-5:8", Constant, |def| {
        assert_def_name_eq!(&context, def, "BAZ");

        assert_definition_at!(&context, "3:1-6:4", Class, |parent_nesting| {
            assert_eq!(parent_nesting.id(), def.lexical_nesting_id().unwrap());
            assert_eq!(parent_nesting.members()[1], def.id());
        });
    });
}

#[test]
fn index_constant_or_write_node() {
    let context = index_source({
        "
        FOO ||= 1

        class Bar
          BAZ ||= 2
        end
        "
    });

    assert_no_local_diagnostics!(&context);
    assert_eq!(context.graph().definitions().len(), 3);

    assert_definition_at!(&context, "1:1-1:4", Constant, |def| {
        assert_def_name_eq!(&context, def, "FOO");
        assert!(def.lexical_nesting_id().is_none());
    });

    assert_definition_at!(&context, "4:3-4:6", Constant, |def| {
        assert_def_name_eq!(&context, def, "BAZ");

        assert_definition_at!(&context, "3:1-5:4", Class, |parent_nesting| {
            assert_eq!(parent_nesting.id(), def.lexical_nesting_id().unwrap());
            assert_eq!(parent_nesting.members()[0], def.id());
        });
    });

    assert_constant_references_eq!(&context, ["FOO", "BAZ"]);
}

#[test]
fn index_constant_path_or_write_node() {
    let context = index_source({
        "
        FOO::BAR ||= 1

        class MyClass
          FOO::BAR ||= 2
          ::BAZ ||= 3
        end
        "
    });

    assert_no_local_diagnostics!(&context);
    assert_eq!(context.graph().definitions().len(), 4);

    assert_definition_at!(&context, "1:6-1:9", Constant, |def| {
        assert_def_name_eq!(&context, def, "FOO::BAR");
        assert!(def.lexical_nesting_id().is_none());
    });

    assert_definition_at!(&context, "4:8-4:11", Constant, |def| {
        assert_def_name_eq!(&context, def, "FOO::BAR");

        assert_definition_at!(&context, "3:1-6:4", Class, |parent_nesting| {
            assert_eq!(parent_nesting.id(), def.lexical_nesting_id().unwrap());
            assert_eq!(parent_nesting.members()[0], def.id());
        });
    });

    assert_definition_at!(&context, "5:5-5:8", Constant, |def| {
        assert_def_name_eq!(&context, def, "BAZ");

        assert_definition_at!(&context, "3:1-6:4", Class, |parent_nesting| {
            assert_eq!(parent_nesting.id(), def.lexical_nesting_id().unwrap());
            assert_eq!(parent_nesting.members()[1], def.id());
        });
    });

    assert_constant_references_eq!(&context, ["FOO", "BAR", "FOO", "BAR", "BAZ"]);
}

#[test]
fn index_constant_multi_write_node() {
    let context = index_source({
        "
        FOO, BAR::BAZ = 1, 2

        class Foo
          FOO, BAR::BAZ, ::BAZ = 3, 4, 5
        end
        "
    });

    assert_no_local_diagnostics!(&context);
    assert_eq!(context.graph().definitions().len(), 6);

    assert_definition_at!(&context, "1:1-1:4", Constant, |def| {
        assert_def_name_eq!(&context, def, "FOO");
        assert!(def.lexical_nesting_id().is_none());
    });

    assert_definition_at!(&context, "1:6-1:14", Constant, |def| {
        assert_def_name_eq!(&context, def, "BAR::BAZ");
    });

    assert_definition_at!(&context, "4:3-4:6", Constant, |def| {
        assert_def_name_eq!(&context, def, "FOO");

        assert_definition_at!(&context, "3:1-5:4", Class, |parent_nesting| {
            assert_eq!(parent_nesting.id(), def.lexical_nesting_id().unwrap());
            assert_eq!(parent_nesting.members()[0], def.id());
        });
    });

    assert_definition_at!(&context, "4:8-4:16", Constant, |def| {
        assert_def_name_eq!(&context, def, "BAR::BAZ");

        assert_definition_at!(&context, "3:1-5:4", Class, |parent_nesting| {
            assert_eq!(parent_nesting.id(), def.lexical_nesting_id().unwrap());
            assert_eq!(parent_nesting.members()[1], def.id());
        });
    });

    assert_definition_at!(&context, "4:18-4:23", Constant, |def| {
        assert_def_name_eq!(&context, def, "BAZ");

        assert_definition_at!(&context, "3:1-5:4", Class, |parent_nesting| {
            assert_eq!(parent_nesting.id(), def.lexical_nesting_id().unwrap());
            assert_eq!(parent_nesting.members()[2], def.id());
        });
    });
}

#[test]
fn index_def_node() {
    let context = index_source({
        "
        def foo; end

        class Foo
          def bar; end
          def self.baz; end
        end

        class Bar
          def Foo.quz; end
        end
        "
    });

    assert_no_local_diagnostics!(&context);
    assert_eq!(context.graph().definitions().len(), 6);

    assert_definition_at!(&context, "1:1-1:13", Method, |def| {
        assert_def_str_eq!(&context, def, "foo()");
        assert_simple_signature!(def, |params| {
            assert_eq!(params.len(), 0);
        });
        assert!(def.receiver().is_none());
        assert!(def.lexical_nesting_id().is_none());
    });

    assert_definition_at!(&context, "3:1-6:4", Class, |foo_class_def| {
        assert_definition_at!(&context, "4:3-4:15", Method, |bar_def| {
            assert_def_str_eq!(&context, bar_def, "bar()");
            assert_simple_signature!(bar_def, |params| {
                assert_eq!(params.len(), 0);
            });
            assert!(bar_def.receiver().is_none());
            assert_eq!(foo_class_def.id(), bar_def.lexical_nesting_id().unwrap());
            assert_eq!(foo_class_def.members()[0], bar_def.id());
        });

        assert_definition_at!(&context, "5:3-5:20", Method, |baz_def| {
            assert_def_str_eq!(&context, baz_def, "baz()");
            assert_simple_signature!(baz_def, |params| {
                assert_eq!(params.len(), 0);
            });
            assert_method_has_receiver!(&context, baz_def, "Foo");
            assert_eq!(foo_class_def.id(), baz_def.lexical_nesting_id().unwrap());
            assert_eq!(foo_class_def.members()[1], baz_def.id());
        });
    });

    assert_definition_at!(&context, "8:1-10:4", Class, |bar_class_def| {
        assert_def_name_eq!(&context, bar_class_def, "Bar");

        assert_definition_at!(&context, "9:3-9:19", Method, |quz_def| {
            assert_def_str_eq!(&context, quz_def, "quz()");
            assert_simple_signature!(quz_def, |params| {
                assert_eq!(params.len(), 0);
            });
            assert_method_has_receiver!(&context, quz_def, "Foo");
            assert_eq!(bar_class_def.id(), quz_def.lexical_nesting_id().unwrap());
        });
    });
}

#[test]
fn do_not_index_def_node_with_dynamic_receiver() {
    let context = index_source({
        "
        def foo.bar; end
        "
    });

    assert_local_diagnostics_eq!(
        &context,
        ["dynamic-singleton-definition: Dynamic receiver for singleton method definition (1:1-1:17)"]
    );
    assert_eq!(context.graph().definitions().len(), 0);
    assert_method_references_eq!(&context, ["foo"]);
}

#[test]
fn index_class_self_block_creates_singleton_class() {
    let context = index_source({
        "
        class Bar; end

        class Foo
          class << self
            def baz; end

            class << Bar
              def self.qux; end
            end

            class << self
              def quz; end
            end
          end
        end
        "
    });

    assert_no_local_diagnostics!(&context);

    // class Bar
    assert_definition_at!(&context, "1:1-1:15", Class, |bar_class| {
        assert_def_name_eq!(&context, bar_class, "Bar");
        assert_def_name_offset_eq!(&context, bar_class, "1:7-1:10");
    });

    // class Foo
    assert_definition_at!(&context, "3:1-15:4", Class, |foo_class| {
        assert_def_name_eq!(&context, foo_class, "Foo");
        assert_def_name_offset_eq!(&context, foo_class, "3:7-3:10");

        // class << self (inside Foo)
        assert_definition_at!(&context, "4:3-14:6", SingletonClass, |foo_singleton| {
            assert_def_name_eq!(&context, foo_singleton, "Foo::<Foo>");
            // name_offset points to "self"
            assert_def_name_offset_eq!(&context, foo_singleton, "4:12-4:16");
            assert_eq!(foo_singleton.lexical_nesting_id(), &Some(foo_class.id()));

            // def baz (inside class << self)
            assert_definition_at!(&context, "5:5-5:17", Method, |baz_method| {
                assert_eq!(baz_method.lexical_nesting_id(), &Some(foo_singleton.id()));
            });

            // class << Bar (inside class << self of Foo)
            assert_definition_at!(&context, "7:5-9:8", SingletonClass, |bar_singleton| {
                assert_def_name_eq!(&context, bar_singleton, "Bar::<Bar>");
                // name_offset points to "Bar"
                assert_def_name_offset_eq!(&context, bar_singleton, "7:14-7:17");
                assert_eq!(bar_singleton.lexical_nesting_id(), &Some(foo_singleton.id()));

                // def self.qux (inside class << Bar)
                assert_definition_at!(&context, "8:7-8:24", Method, |qux_method| {
                    assert_eq!(qux_method.lexical_nesting_id(), &Some(bar_singleton.id()));
                    assert_method_has_receiver!(&context, qux_method, "<Bar>");
                });
            });

            // class << self (nested inside outer class << self)
            assert_definition_at!(&context, "11:5-13:8", SingletonClass, |nested_singleton| {
                assert_def_name_eq!(&context, nested_singleton, "Foo::<Foo>::<<Foo>>");
                // name_offset points to "self"
                assert_def_name_offset_eq!(&context, nested_singleton, "11:14-11:18");
                assert_eq!(nested_singleton.lexical_nesting_id(), &Some(foo_singleton.id()));

                // def quz (inside nested class << self)
                assert_definition_at!(&context, "12:7-12:19", Method, |quz_method| {
                    assert_eq!(quz_method.lexical_nesting_id(), &Some(nested_singleton.id()));
                });
            });
        });
    });
}

#[test]
fn index_singleton_class_definition_in_compact_namespace() {
    let context = index_source({
        "
        class Foo::Bar
          class << self
            def baz; end
          end
        end
        "
    });

    assert_no_local_diagnostics!(&context);

    assert_definition_at!(&context, "1:1-5:4", Class, |class_def| {
        assert_def_name_eq!(&context, class_def, "Foo::Bar");
        assert_definition_at!(&context, "2:3-4:6", SingletonClass, |singleton_class| {
            assert_eq!(singleton_class.lexical_nesting_id(), &Some(class_def.id()));
            assert_definition_at!(&context, "3:5-3:17", Method, |method| {
                assert_eq!(method.lexical_nesting_id(), &Some(singleton_class.id()));
            });
        });
    });

    assert_constant_references_eq!(&context, ["Foo"]);
}

#[test]
fn index_constant_in_singleton_class_definition() {
    let context = index_source({
        "
        class Foo
          class << self
            A = 1
          end
        end
        "
    });

    assert_no_local_diagnostics!(&context);

    assert_definition_at!(&context, "1:1-5:4", Class, |class_def| {
        assert_definition_at!(&context, "2:3-4:6", SingletonClass, |singleton_class| {
            assert_eq!(singleton_class.lexical_nesting_id(), &Some(class_def.id()));
            assert_definition_at!(&context, "3:5-3:6", Constant, |def| {
                assert_def_name_eq!(&context, def, "A");
                assert_eq!(Some(singleton_class.id()), def.lexical_nesting_id().clone());
            });
        });
    });
}

#[test]
fn do_not_index_singleton_class_with_dynamic_expression() {
    let context = index_source({
        "
        class << foo
          def bar; end
        end
        "
    });

    assert_local_diagnostics_eq!(
        &context,
        ["dynamic-singleton-definition: Dynamic singleton class definition (1:1-3:4)"]
    );
    assert_eq!(context.graph().definitions().len(), 0);
}

#[test]
fn index_class_variable_in_singleton_class_definition() {
    let context = index_source({
        "
        class Foo
          class << self
            @@var = 1
          end
        end
        "
    });

    assert_no_local_diagnostics!(&context);

    // During indexing, lexical_nesting_id is the actual enclosing scope (singleton class).
    // The resolution phase handles bypassing singleton classes for class variable ownership.
    assert_definition_at!(&context, "2:3-4:6", SingletonClass, |singleton_class| {
        assert_definition_at!(&context, "3:5-3:10", ClassVariable, |def| {
            assert_def_str_eq!(&context, def, "@@var");
            assert_eq!(Some(singleton_class.id()), *def.lexical_nesting_id());
        });
    });
}

#[test]
fn index_class_variable_in_nested_singleton_class_definition() {
    let context = index_source({
        "
        class Foo
          class << self
            class << self
              @@var = 1
            end
          end
        end
        "
    });

    assert_no_local_diagnostics!(&context);

    // During indexing, lexical_nesting_id is the actual enclosing scope (innermost singleton class).
    // The resolution phase handles bypassing singleton classes for class variable ownership.
    assert_definition_at!(&context, "3:5-5:8", SingletonClass, |nested_singleton| {
        assert_definition_at!(&context, "4:7-4:12", ClassVariable, |def| {
            assert_def_str_eq!(&context, def, "@@var");
            assert_eq!(Some(nested_singleton.id()), *def.lexical_nesting_id());
        });
    });
}

#[test]
fn index_class_variable_in_singleton_method_definition() {
    let context = index_source({
        "
        class Foo
          def self.bar
            @@var = 1
          end
        end
        "
    });

    assert_no_local_diagnostics!(&context);

    assert_definition_at!(&context, "1:1-5:4", Class, |class_def| {
        assert_definition_at!(&context, "3:5-3:10", ClassVariable, |def| {
            assert_def_str_eq!(&context, def, "@@var");
            assert_eq!(Some(class_def.id()), def.lexical_nesting_id().clone());
        });
    });
}

#[test]
fn index_def_node_with_parameters() {
    let context = index_source({
        "
        def foo(a, b = 42, *c, d, e:, g: 42, **i, &j); end
        "
    });

    assert_no_local_diagnostics!(&context);

    assert_definition_at!(&context, "1:1-1:51", Method, |def| {
        assert_simple_signature!(def, |params| {
            assert_eq!(params.len(), 8);

            assert_parameter!(&params[0], RequiredPositional, |param| {
                assert_string_eq!(context, param.str(), "a");
            });

            assert_parameter!(&params[1], OptionalPositional, |param| {
                assert_string_eq!(context, param.str(), "b");
            });

            assert_parameter!(&params[2], RestPositional, |param| {
                assert_string_eq!(context, param.str(), "c");
            });

            assert_parameter!(&params[3], Post, |param| {
                assert_string_eq!(context, param.str(), "d");
            });

            assert_parameter!(&params[4], RequiredKeyword, |param| {
                assert_string_eq!(context, param.str(), "e");
                assert_eq!(context.source_at(param.offset()), "e");
            });

            assert_parameter!(&params[5], OptionalKeyword, |param| {
                assert_string_eq!(context, param.str(), "g");
                assert_eq!(context.source_at(param.offset()), "g");
            });

            assert_parameter!(&params[6], RestKeyword, |param| {
                assert_string_eq!(context, param.str(), "i");
            });

            assert_parameter!(&params[7], Block, |param| {
                assert_string_eq!(context, param.str(), "j");
            });
        });
    });
}

#[test]
fn index_def_node_with_forward_parameters() {
    let context = index_source({
        "
        def foo(...); end
        "
    });

    assert_no_local_diagnostics!(&context);

    assert_definition_at!(&context, "1:1-1:18", Method, |def| {
        assert_simple_signature!(def, |params| {
            assert_eq!(params.len(), 1);
            assert_parameter!(&params[0], Forward, |param| {
                assert_string_eq!(context, param.str(), "...");
            });
        });
    });
}

#[test]
fn index_def_node_with_visibility_top_level() {
    let context = index_source({
        "
        def m1; end

        protected def m2; end

        public

        def m3; end
        "
    });

    assert_no_local_diagnostics!(&context);

    assert_definition_at!(&context, "1:1-1:12", Method, |def| {
        assert_def_str_eq!(&context, def, "m1()");
        assert_eq!(def.visibility(), &Visibility::Private);
    });

    assert_definition_at!(&context, "3:11-3:22", Method, |def| {
        assert_def_str_eq!(&context, def, "m2()");
        assert_eq!(def.visibility(), &Visibility::Protected);
    });

    assert_definition_at!(&context, "7:1-7:12", Method, |def| {
        assert_def_str_eq!(&context, def, "m3()");
        assert_eq!(def.visibility(), &Visibility::Public);
    });
}

#[test]
fn index_module_function() {
    let context = index_source({
        "
        module Foo
          def bar; end

          module_function

          def baz; end
          attr_reader :attribute

          public

          def qux; end

          module_function def boop; end

          def zip; end
        end
        "
    });

    assert_no_local_diagnostics!(&context);

    assert_definition_at!(&context, "2:3-2:15", Method, |def| {
        assert_def_str_eq!(&context, def, "bar()");
        assert_eq!(def.visibility(), &Visibility::Public);
    });

    let definitions = context.all_definitions_at("6:3-6:15");
    assert_eq!(
        definitions.len(),
        2,
        "module_function should create two definitions for baz"
    );

    let instance_method = definitions
        .iter()
        .find(|d| matches!(d, Definition::Method(m) if m.receiver().is_none()))
        .expect("should have instance method definition");
    let Definition::Method(instance_method) = instance_method else {
        panic!()
    };
    assert_def_str_eq!(&context, instance_method, "baz()");
    assert_eq!(instance_method.visibility(), &Visibility::Private);

    let singleton_method = definitions
        .iter()
        .find(|d| matches!(d, Definition::Method(m) if m.receiver().is_some()))
        .expect("should have singleton method definition");
    let Definition::Method(singleton_method) = singleton_method else {
        panic!()
    };
    assert_def_str_eq!(&context, singleton_method, "baz()");
    assert_eq!(singleton_method.visibility(), &Visibility::Public);

    assert_definition_at!(&context, "7:16-7:25", AttrReader, |def| {
        assert_def_str_eq!(&context, def, "attribute()");
        assert_eq!(def.visibility(), &Visibility::Private);
    });

    assert_definition_at!(&context, "11:3-11:15", Method, |def| {
        assert_def_str_eq!(&context, def, "qux()");
        assert_eq!(def.visibility(), &Visibility::Public);
    });

    let definitions = context.all_definitions_at("13:19-13:32");
    assert_eq!(
        definitions.len(),
        2,
        "module_function should create two definitions for boop"
    );

    let instance_method = definitions
        .iter()
        .find(|d| matches!(d, Definition::Method(m) if m.receiver().is_none()))
        .expect("boop: should have instance method definition");
    let Definition::Method(instance_method) = instance_method else {
        panic!()
    };
    assert_def_str_eq!(&context, instance_method, "boop()");
    assert_eq!(instance_method.visibility(), &Visibility::Private);

    let singleton_method = definitions
        .iter()
        .find(|d| matches!(d, Definition::Method(m) if m.receiver().is_some()))
        .expect("boop: should have singleton method definition");
    let Definition::Method(singleton_method) = singleton_method else {
        panic!()
    };
    assert_def_str_eq!(&context, singleton_method, "boop()");
    assert_eq!(singleton_method.visibility(), &Visibility::Public);

    assert_definition_at!(&context, "15:3-15:15", Method, |def| {
        assert_def_str_eq!(&context, def, "zip()");
        assert_eq!(def.visibility(), &Visibility::Public);
    });
}

#[test]
fn index_def_node_with_visibility_nested() {
    let context = index_source({
        "
        protected

        class Foo
          def m1; end

          private

          module Bar
            def m2; end

            private

            def m3; end

            protected
          end

          def m4; end
        end
        "
    });

    assert_no_local_diagnostics!(&context);

    assert_definition_at!(&context, "4:3-4:14", Method, |def| {
        assert_def_str_eq!(&context, def, "m1()");
        assert_eq!(def.visibility(), &Visibility::Public);
    });

    assert_definition_at!(&context, "9:5-9:16", Method, |def| {
        assert_def_str_eq!(&context, def, "m2()");
        assert_eq!(def.visibility(), &Visibility::Public);
    });

    assert_definition_at!(&context, "13:5-13:16", Method, |def| {
        assert_def_str_eq!(&context, def, "m3()");
        assert_eq!(def.visibility(), &Visibility::Private);
    });

    assert_definition_at!(&context, "18:3-18:14", Method, |def| {
        assert_def_str_eq!(&context, def, "m4()");
        assert_eq!(def.visibility(), &Visibility::Private);
    });
}

#[test]
fn index_def_node_singleton_visibility() {
    let context = index_source({
        "
        protected

        def self.m1; end

        protected def self.m2; end

        class Foo
          private

          def self.m3; end
        end
        "
    });

    assert_no_local_diagnostics!(&context);

    assert_definition_at!(&context, "3:1-3:17", Method, |def| {
        assert_def_str_eq!(&context, def, "m1()");
        assert_eq!(def.visibility(), &Visibility::Public);
    });

    assert_definition_at!(&context, "5:11-5:27", Method, |def| {
        assert_def_str_eq!(&context, def, "m2()");
        assert_eq!(def.visibility(), &Visibility::Public);
    });

    assert_definition_at!(&context, "10:3-10:19", Method, |def| {
        assert_def_str_eq!(&context, def, "m3()");
        assert_eq!(def.visibility(), &Visibility::Public);
    });
}

#[test]
fn index_visibility_in_singleton_class() {
    let context = index_source({
        "
        class Foo
          protected

          class << self
            def m1; end

            private

            def m2; end
          end

          def m3; end
        end
        "
    });

    assert_no_local_diagnostics!(&context);

    assert_definition_at!(&context, "5:5-5:16", Method, |def| {
        assert_def_str_eq!(&context, def, "m1()");
        assert_eq!(def.visibility(), &Visibility::Public);
    });

    assert_definition_at!(&context, "9:5-9:16", Method, |def| {
        assert_def_str_eq!(&context, def, "m2()");
        assert_eq!(def.visibility(), &Visibility::Private);
    });

    assert_definition_at!(&context, "12:3-12:14", Method, |def| {
        assert_def_str_eq!(&context, def, "m3()");
        assert_eq!(def.visibility(), &Visibility::Protected);
    });
}

#[test]
fn index_attr_accessor_definition() {
    let context = index_source({
        "
        attr_accessor :foo

        class Foo
          attr_accessor :bar, :baz
        end

        foo.attr_accessor :not_indexed
        "
    });

    assert_no_local_diagnostics!(&context);
    assert_eq!(context.graph().definitions().len(), 4);

    assert_definition_at!(&context, "1:16-1:19", AttrAccessor, |def| {
        assert_def_str_eq!(&context, def, "foo()");
        assert!(def.lexical_nesting_id().is_none());
    });

    assert_definition_at!(&context, "4:18-4:21", AttrAccessor, |def| {
        assert_def_str_eq!(&context, def, "bar()");

        assert_definition_at!(&context, "3:1-5:4", Class, |parent_nesting| {
            assert_eq!(parent_nesting.id(), def.lexical_nesting_id().unwrap());
            assert_eq!(parent_nesting.members()[0], def.id());
        });
    });

    assert_definition_at!(&context, "4:24-4:27", AttrAccessor, |def| {
        assert_def_str_eq!(&context, def, "baz()");

        assert_definition_at!(&context, "3:1-5:4", Class, |parent_nesting| {
            assert_eq!(parent_nesting.id(), def.lexical_nesting_id().unwrap());
            assert_eq!(parent_nesting.members()[1], def.id());
        });
    });
}

#[test]
fn index_attr_reader_definition() {
    let context = index_source({
        "
        attr_reader :foo

        class Foo
          attr_reader :bar, :baz
        end
        "
    });

    assert_no_local_diagnostics!(&context);
    assert_eq!(context.graph().definitions().len(), 4);

    assert_definition_at!(&context, "1:14-1:17", AttrReader, |def| {
        assert_def_str_eq!(&context, def, "foo()");
        assert!(def.lexical_nesting_id().is_none());
    });

    assert_definition_at!(&context, "4:16-4:19", AttrReader, |def| {
        assert_def_str_eq!(&context, def, "bar()");

        assert_definition_at!(&context, "3:1-5:4", Class, |parent_nesting| {
            assert_eq!(parent_nesting.id(), def.lexical_nesting_id().unwrap());
            assert_eq!(parent_nesting.members()[0], def.id());
        });
    });

    assert_definition_at!(&context, "4:22-4:25", AttrReader, |def| {
        assert_def_str_eq!(&context, def, "baz()");

        assert_definition_at!(&context, "3:1-5:4", Class, |parent_nesting| {
            assert_eq!(parent_nesting.id(), def.lexical_nesting_id().unwrap());
            assert_eq!(parent_nesting.members()[1], def.id());
        });
    });
}

#[test]
fn index_attr_writer_definition() {
    let context = index_source({
        "
        attr_writer :foo

        class Foo
          attr_writer :bar, :baz
        end
        "
    });

    assert_no_local_diagnostics!(&context);
    assert_eq!(context.graph().definitions().len(), 4);

    assert_definition_at!(&context, "1:14-1:17", AttrWriter, |def| {
        assert_def_str_eq!(&context, def, "foo()");
        assert!(def.lexical_nesting_id().is_none());
    });

    assert_definition_at!(&context, "4:16-4:19", AttrWriter, |def| {
        assert_def_str_eq!(&context, def, "bar()");

        assert_definition_at!(&context, "3:1-5:4", Class, |parent_nesting| {
            assert_eq!(parent_nesting.id(), def.lexical_nesting_id().unwrap());
            assert_eq!(parent_nesting.members()[0], def.id());
        });
    });

    assert_definition_at!(&context, "4:22-4:25", AttrWriter, |def| {
        assert_def_str_eq!(&context, def, "baz()");

        assert_definition_at!(&context, "3:1-5:4", Class, |parent_nesting| {
            assert_eq!(parent_nesting.id(), def.lexical_nesting_id().unwrap());
            assert_eq!(parent_nesting.members()[1], def.id());
        });
    });
}

#[test]
fn index_attr_definition() {
    let context = index_source({
        r#"
        attr "a1", :a2

        class Foo
          attr "a3", true
          attr :a4, false
          attr :a5, 123
        end
        "#
    });

    assert_no_local_diagnostics!(&context);
    assert_eq!(context.graph().definitions().len(), 6);

    assert_definition_at!(&context, "1:6-1:10", AttrReader, |def| {
        assert_def_str_eq!(&context, def, "a1()");
        assert!(def.lexical_nesting_id().is_none());
    });

    assert_definition_at!(&context, "1:13-1:15", AttrReader, |def| {
        assert_def_str_eq!(&context, def, "a2()");
        assert!(def.lexical_nesting_id().is_none());
    });

    assert_definition_at!(&context, "4:8-4:12", AttrAccessor, |def| {
        assert_def_str_eq!(&context, def, "a3()");

        assert_definition_at!(&context, "3:1-7:4", Class, |parent_nesting| {
            assert_eq!(parent_nesting.id(), def.lexical_nesting_id().unwrap());
            assert_eq!(parent_nesting.members()[0], def.id());
        });
    });

    assert_definition_at!(&context, "5:9-5:11", AttrReader, |def| {
        assert_def_str_eq!(&context, def, "a4()");

        assert_definition_at!(&context, "3:1-7:4", Class, |parent_nesting| {
            assert_eq!(parent_nesting.id(), def.lexical_nesting_id().unwrap());
            assert_eq!(parent_nesting.members()[1], def.id());
        });
    });

    assert_definition_at!(&context, "6:9-6:11", AttrReader, |def| {
        assert_def_str_eq!(&context, def, "a5()");
        assert_definition_at!(&context, "3:1-7:4", Class, |parent_nesting| {
            assert_eq!(parent_nesting.id(), def.lexical_nesting_id().unwrap());
            assert_eq!(parent_nesting.members()[2], def.id());
        });
    });
}

#[test]
fn index_attr_accessor_with_visibility_top_level() {
    let context = index_source({
        "
        attr_accessor :foo

        protected attr_reader :bar

        public

        attr_writer :baz
        "
    });

    assert_no_local_diagnostics!(&context);

    assert_definition_at!(&context, "1:16-1:19", AttrAccessor, |def| {
        assert_def_str_eq!(&context, def, "foo()");
        assert_eq!(def.visibility(), &Visibility::Private);
    });

    assert_definition_at!(&context, "3:24-3:27", AttrReader, |def| {
        assert_def_str_eq!(&context, def, "bar()");
        assert_eq!(def.visibility(), &Visibility::Protected);
    });

    assert_definition_at!(&context, "7:14-7:17", AttrWriter, |def| {
        assert_def_str_eq!(&context, def, "baz()");
        assert_eq!(def.visibility(), &Visibility::Public);
    });
}

#[test]
fn index_attr_accessor_with_visibility_nested() {
    let context = index_source({
        "
        protected

        class Foo
          attr_accessor :foo

          private

          module Bar
            attr_accessor :bar

            private

            attr_reader :baz

            public
          end

          attr_writer :qux
        end
        "
    });

    assert_no_local_diagnostics!(&context);

    assert_definition_at!(&context, "4:18-4:21", AttrAccessor, |def| {
        assert_def_str_eq!(&context, def, "foo()");
        assert_eq!(def.visibility(), &Visibility::Public);
    });

    assert_definition_at!(&context, "9:20-9:23", AttrAccessor, |def| {
        assert_def_str_eq!(&context, def, "bar()");
        assert_eq!(def.visibility(), &Visibility::Public);
    });

    assert_definition_at!(&context, "13:18-13:21", AttrReader, |def| {
        assert_def_str_eq!(&context, def, "baz()");
        assert_eq!(def.visibility(), &Visibility::Private);
    });

    assert_definition_at!(&context, "18:16-18:19", AttrWriter, |def| {
        assert_def_str_eq!(&context, def, "qux()");
        assert_eq!(def.visibility(), &Visibility::Private);
    });
}

#[test]
fn index_global_variable_definition() {
    let context = index_source({
        "
        $foo = 1
        $bar, $baz = 2, 3

        class Foo
          $qux = 2
        end

        $one &= 1
        $two &&= 1
        $three ||= 1
        "
    });

    assert_no_local_diagnostics!(&context);
    assert_eq!(context.graph().definitions().len(), 8);

    assert_definition_at!(&context, "1:1-1:5", GlobalVariable, |def| {
        assert_def_str_eq!(&context, def, "$foo");
        assert!(def.lexical_nesting_id().is_none());
    });

    assert_definition_at!(&context, "2:1-2:5", GlobalVariable, |def| {
        assert_def_str_eq!(&context, def, "$bar");
        assert!(def.lexical_nesting_id().is_none());
    });

    assert_definition_at!(&context, "2:7-2:11", GlobalVariable, |def| {
        assert_def_str_eq!(&context, def, "$baz");
        assert!(def.lexical_nesting_id().is_none());
    });

    assert_definition_at!(&context, "5:3-5:7", GlobalVariable, |def| {
        assert_def_str_eq!(&context, def, "$qux");

        assert_definition_at!(&context, "4:1-6:4", Class, |parent_nesting| {
            assert_eq!(parent_nesting.id(), def.lexical_nesting_id().unwrap());
            assert_eq!(parent_nesting.members()[0], def.id());
        });
    });

    assert_definition_at!(&context, "8:1-8:5", GlobalVariable, |def| {
        assert_def_str_eq!(&context, def, "$one");
        assert!(def.lexical_nesting_id().is_none());
    });

    assert_definition_at!(&context, "9:1-9:5", GlobalVariable, |def| {
        assert_def_str_eq!(&context, def, "$two");
        assert!(def.lexical_nesting_id().is_none());
    });

    assert_definition_at!(&context, "10:1-10:7", GlobalVariable, |def| {
        assert_def_str_eq!(&context, def, "$three");
        assert!(def.lexical_nesting_id().is_none());
    });
}

#[test]
fn index_instance_variable_definition() {
    let context = index_source({
        "
        @foo = 1

        class Foo
          @bar = 2
          @baz, @qux = 3, 4
        end

        @bar &= 5
        @baz &&= 6
        @qux ||= 7

        class Bar
          @foo &= 8
          @bar &&= 9
          @baz ||= 10
        end
        "
    });

    assert_no_local_diagnostics!(&context);

    assert_definition_at!(&context, "1:1-1:5", InstanceVariable, |def| {
        assert_def_str_eq!(&context, def, "@foo");
        assert!(def.lexical_nesting_id().is_none());
    });

    assert_definition_at!(&context, "3:1-6:4", Class, |foo_class_def| {
        assert_definition_at!(&context, "4:3-4:7", InstanceVariable, |def| {
            assert_def_str_eq!(&context, def, "@bar");
            assert_eq!(foo_class_def.id(), def.lexical_nesting_id().unwrap());
            assert_eq!(foo_class_def.members()[0], def.id());
        });

        assert_definition_at!(&context, "5:3-5:7", InstanceVariable, |def| {
            assert_def_str_eq!(&context, def, "@baz");
            assert_eq!(foo_class_def.id(), def.lexical_nesting_id().unwrap());
            assert_eq!(foo_class_def.members()[1], def.id());
        });

        assert_definition_at!(&context, "5:9-5:13", InstanceVariable, |def| {
            assert_def_str_eq!(&context, def, "@qux");
            assert_eq!(foo_class_def.id(), def.lexical_nesting_id().unwrap());
            assert_eq!(foo_class_def.members()[2], def.id());
        });
    });

    assert_definition_at!(&context, "8:1-8:5", InstanceVariable, |def| {
        assert_def_str_eq!(&context, def, "@bar");
        assert!(def.lexical_nesting_id().is_none());
    });

    assert_definition_at!(&context, "9:1-9:5", InstanceVariable, |def| {
        assert_def_str_eq!(&context, def, "@baz");
        assert!(def.lexical_nesting_id().is_none());
    });

    assert_definition_at!(&context, "10:1-10:5", InstanceVariable, |def| {
        assert_def_str_eq!(&context, def, "@qux");
        assert!(def.lexical_nesting_id().is_none());
    });

    assert_definition_at!(&context, "12:1-16:4", Class, |bar_class_def| {
        assert_definition_at!(&context, "13:3-13:7", InstanceVariable, |def| {
            assert_def_str_eq!(&context, def, "@foo");
            assert_eq!(bar_class_def.id(), def.lexical_nesting_id().unwrap());
            assert_eq!(bar_class_def.members()[0], def.id());
        });

        assert_definition_at!(&context, "14:3-14:7", InstanceVariable, |def| {
            assert_def_str_eq!(&context, def, "@bar");
            assert_eq!(bar_class_def.id(), def.lexical_nesting_id().unwrap());
            assert_eq!(bar_class_def.members()[1], def.id());
        });

        assert_definition_at!(&context, "15:3-15:7", InstanceVariable, |def| {
            assert_def_str_eq!(&context, def, "@baz");
            assert_eq!(bar_class_def.id(), def.lexical_nesting_id().unwrap());
            assert_eq!(bar_class_def.members()[2], def.id());
        });
    });
}

#[test]
fn index_class_variable_definition() {
    let context = index_source({
        "
        @@foo = 1

        class Foo
          @@bar = 2
          @@baz, @@qux = 3, 4
        end

        @@bar &= 5
        @@baz &&= 6
        @@qux ||= 7

        class Bar
          @@foo &= 1
          @@bar &&= 2
          @@baz ||= 3

          def set_foo
            @@foo = 4
          end
        end
        "
    });

    // This is actually not allowed in Ruby and will raise a runtime error
    // But we should still index it so we can insert a diagnostic for it
    assert_no_local_diagnostics!(&context);

    assert_definition_at!(&context, "1:1-1:6", ClassVariable, |def| {
        assert_def_str_eq!(&context, def, "@@foo");
        assert!(def.lexical_nesting_id().is_none());
    });

    assert_definition_at!(&context, "3:1-6:4", Class, |foo_class_def| {
        assert_definition_at!(&context, "4:3-4:8", ClassVariable, |def| {
            assert_def_str_eq!(&context, def, "@@bar");
            assert_eq!(foo_class_def.id(), def.lexical_nesting_id().unwrap());
            assert_eq!(foo_class_def.members()[0], def.id());
        });

        assert_definition_at!(&context, "5:3-5:8", ClassVariable, |def| {
            assert_def_str_eq!(&context, def, "@@baz");
            assert_eq!(foo_class_def.id(), def.lexical_nesting_id().unwrap());
            assert_eq!(foo_class_def.members()[1], def.id());
        });

        assert_definition_at!(&context, "5:10-5:15", ClassVariable, |def| {
            assert_def_str_eq!(&context, def, "@@qux");
            assert_eq!(foo_class_def.id(), def.lexical_nesting_id().unwrap());
            assert_eq!(foo_class_def.members()[2], def.id());
        });
    });

    assert_definition_at!(&context, "8:1-8:6", ClassVariable, |def| {
        assert_def_str_eq!(&context, def, "@@bar");
        assert!(def.lexical_nesting_id().is_none());
    });

    assert_definition_at!(&context, "9:1-9:6", ClassVariable, |def| {
        assert_def_str_eq!(&context, def, "@@baz");
        assert!(def.lexical_nesting_id().is_none());
    });

    assert_definition_at!(&context, "10:1-10:6", ClassVariable, |def| {
        assert_def_str_eq!(&context, def, "@@qux");
        assert!(def.lexical_nesting_id().is_none());
    });

    assert_definition_at!(&context, "12:1-20:4", Class, |bar_class_def| {
        assert_definition_at!(&context, "13:3-13:8", ClassVariable, |def| {
            assert_def_str_eq!(&context, def, "@@foo");
            assert_eq!(bar_class_def.id(), def.lexical_nesting_id().unwrap());
            assert_eq!(bar_class_def.members()[0], def.id());
        });

        assert_definition_at!(&context, "14:3-14:8", ClassVariable, |def| {
            assert_def_str_eq!(&context, def, "@@bar");
            assert_eq!(bar_class_def.id(), def.lexical_nesting_id().unwrap());
            assert_eq!(bar_class_def.members()[1], def.id());
        });

        assert_definition_at!(&context, "15:3-15:8", ClassVariable, |def| {
            assert_def_str_eq!(&context, def, "@@baz");
            assert_eq!(bar_class_def.id(), def.lexical_nesting_id().unwrap());
            assert_eq!(bar_class_def.members()[2], def.id());
        });

        // Method `set_foo` is members()[3], class variable inside method is members()[4]
        assert_definition_at!(&context, "18:5-18:10", ClassVariable, |def| {
            assert_def_str_eq!(&context, def, "@@foo");
            assert_eq!(bar_class_def.id(), def.lexical_nesting_id().unwrap());
            assert_eq!(bar_class_def.members()[4], def.id());
        });
    });
}

#[test]
fn index_unresolved_constant_references() {
    let context = index_source({
        r##"
        puts C1
        puts C2::C3::C4
        puts ignored0::IGNORED0
        puts C6.foo
        foo = C7
        C8 << 42
        C9 += 42
        C10 ||= 42
        C11 &&= 42
        C12[C13]
        C14::IGNORED1 = 42 # IGNORED1 is an assignment
        C15::C16 << 42
        C17::C18 += 42
        C19::C20 ||= 42
        C21::C22 &&= 42
        puts "#{C23}"

        ::IGNORED2 = 42 # IGNORED2 is an assignment
        puts "IGNORED3"
        puts :IGNORED4
        "##
    });

    assert_local_diagnostics_eq!(
        &context,
        [
            "dynamic-constant-reference: Dynamic constant reference (3:6-3:14)",
            "parse-warning: assigned but unused variable - foo (5:1-5:4)",
        ]
    );

    assert_constant_references_eq!(
        &context,
        [
            "C1", "C2", "C3", "C4", "<C6>", "C6", "C7", "<C8>", "C8", "C9", "C10", "C11", "<C12>", "C12", "C13", "C14",
            "<C16>", "C15", "C16", "C17", "C18", "C19", "C20", "C21", "C22", "C23"
        ]
    );
}

#[test]
fn index_unresolved_constant_references_from_values() {
    let context = index_source({
        "
        IGNORED1 = C1
        IGNORED2 = [C2::C3]
        C4 << C5
        C6 += C7
        C8 ||= C9
        C10 &&= C11
        C12[C13] = C14
        "
    });

    assert_no_local_diagnostics!(&context);

    assert_constant_references_eq!(
        &context,
        [
            "C1", "C2", "C3", "<C4>", "C4", "C5", "C6", "C7", "C8", "C9", "C10", "C11", "<C12>", "C12", "C13", "C14"
        ]
    );
}

#[test]
fn index_constant_path_and_write_visits_value() {
    let context = index_source({
        "
        C1::C2 &&= C3
        C4::C5 += C6
        C7::C8 ||= C9
        "
    });

    assert_no_local_diagnostics!(&context);

    assert_constant_references_eq!(&context, ["C1", "C2", "C3", "C4", "C5", "C6", "C7", "C8", "C9"]);
}

#[test]
fn index_unresolved_constant_references_for_classes() {
    let context = index_source({
        "
        C1.new

        class IGNORED < ::C2; end
        class IGNORED < C3; end
        class IGNORED < C4::C5; end
        class IGNORED < ::C7::C6; end

        class C8::IGNORED; end
        class ::C9::IGNORED; end
        class C10::C11::IGNORED; end
        "
    });

    assert_no_local_diagnostics!(&context);

    assert_constant_references_eq!(
        &context,
        [
            "<C1>", "C1", "C2", "C3", "C4", "C5", "C6", "C7", "C8", "C9", "C10", "C11"
        ]
    );
}

#[test]
fn index_unresolved_constant_references_for_modules() {
    let context = index_source({
        "
        module X
          include M1
          include M2::M3
          extend M4
          extend M5::M6
          prepend M7
          prepend M8::M9
        end

        M10.include M11
        M12.extend M13
        M14.prepend M15

        module M16::IGNORED; end
        module ::M17::IGNORED; end
        module M18::M19::IGNORED; end

        module M20
          include self
        end

        module M21
          extend self
        end

        module M22
          prepend self
        end
        "
    });

    assert_no_local_diagnostics!(&context);

    assert_constant_references_eq!(
        &context,
        [
            "M1", "M2", "M3", "M4", "M5", "M6", "M7", "M8", "M9", "M10", "M11", "M12", "M13", "M14", "M15", "M16",
            "M17", "M18", "M19", "M20", "M21", "M22",
        ]
    );
}

#[test]
fn index_method_reference_references() {
    let context = index_source({
        "
        m1
        m2(m3)
        m4 m5
        self.m6
        self.m7(m8)
        self.m9 m10
        C.m11
        C.m12(m13)
        C.m14 m15
        m16.m17
        m18.m19(m20)
        m21.m22 m23

        m24.m25.m26

        !m27 # The `!` is collected and will count as one more reference
        m28&.m29
        m30(&m31)
        m32 { m33 }
        m34 do m35 end
        m36[m37] # The `[]` is collected and will count as one more reference

        def foo(&block)
            m38(&block)
        end

        m39(&:m40)
        m41(&m42)
        m43(m44, &m45(m46))
        m47(x: m48, m49:)
        m50(...)
        "
    });

    assert_local_diagnostics_eq!(
        &context,
        ["parse-error: unexpected ... when the parent method is not forwarding (31:5-31:8)"]
    );

    assert_method_references_eq!(
        &context,
        [
            "m1", "m2", "m3", "m4", "m5", "m6", "m7", "m8", "m9", "m10", "m11", "m12", "m13", "m14", "m15", "m16",
            "m17", "m18", "m19", "m20", "m21", "m22", "m23", "m24", "m25", "m26", "!", "m27", "m28", "m29", "m30",
            "m31", "m32", "m33", "m34", "m35", "m36", "[]", "m37", "m38", "m39", "m40", "m41", "m42", "m43", "m44",
            "m45", "m46", "m47", "m48", "m49", "m50"
        ]
    );
}

#[test]
fn index_method_reference_assign_references() {
    let context = index_source({
        "
        self.m1 = m2
        m3.m4.m5 = m6.m7.m8
        self.m9, self.m10 = m11, m12
        "
    });

    assert_no_local_diagnostics!(&context);

    assert_method_references_eq!(
        &context,
        [
            "m1=", "m2", "m3", "m4", "m5=", "m6", "m7", "m8", "m9=", "m10=", "m11", "m12"
        ]
    );
}

#[test]
fn index_method_reference_opassign_references() {
    let context = index_source({
        "
        self.m1 += 42
        self.m2 |= 42
        self.m3 ||= 42
        self.m4 &&= 42
        m5.m6 += m7
        m8.m9 ||= m10
        m11.m12 &&= m13
        "
    });

    assert_no_local_diagnostics!(&context);

    assert_method_references_eq!(
        &context,
        [
            "m1", "m1=", "m2", "m2=", "m3", "m3=", "m4", "m4=", "m5", "m6", "m6=", "m7", "m8", "m9", "m9=", "m10",
            "m11", "m12", "m12=", "m13",
        ]
    );
}

#[test]
fn index_method_reference_operator_references() {
    let context = index_source({
        "
        X != Y
        X % Y
        X & Y
        X && Y
        X * Y
        X ** Y
        X + Y
        X - Y
        X / Y
        X << Y
        X == Y
        X === Y
        X >> Y
        X ^ Y
        X | Y
        X || Y
        X <=> Y
        "
    });

    assert_local_diagnostics_eq!(
        &context,
        [
            "parse-warning: possibly useless use of != in void context (1:1-1:7)",
            "parse-warning: possibly useless use of % in void context (2:1-2:6)",
            "parse-warning: possibly useless use of & in void context (3:1-3:6)",
            "parse-warning: possibly useless use of * in void context (5:1-5:6)",
            "parse-warning: possibly useless use of ** in void context (6:1-6:7)",
            "parse-warning: possibly useless use of + in void context (7:1-7:6)",
            "parse-warning: possibly useless use of - in void context (8:1-8:6)",
            "parse-warning: possibly useless use of / in void context (9:1-9:6)",
            "parse-warning: possibly useless use of == in void context (11:1-11:7)",
            "parse-warning: possibly useless use of ^ in void context (14:1-14:6)",
            "parse-warning: possibly useless use of | in void context (15:1-15:6)",
            "parse-warning: possibly useless use of <=> in void context (17:1-17:8)"
        ]
    );

    assert_method_references_eq!(
        &context,
        [
            "!=", "%", "&", "&&", "*", "**", "+", "-", "/", "<<", "==", "===", ">>", "^", "|", "||", "<=>",
        ]
    );
}

#[test]
fn index_method_reference_lesser_than_operator_references() {
    let context = index_source({
        "
        x < y
        "
    });

    assert_local_diagnostics_eq!(
        &context,
        ["parse-warning: possibly useless use of < in void context (1:1-1:6)"]
    );

    assert_method_references_eq!(&context, ["x", "<", "<=>", "y"]);
}

#[test]
fn index_method_reference_lesser_than_or_equal_to_operator_references() {
    let context = index_source({
        "
        x <= y
        "
    });

    assert_local_diagnostics_eq!(
        &context,
        ["parse-warning: possibly useless use of <= in void context (1:1-1:7)"]
    );

    assert_method_references_eq!(&context, ["x", "<=", "<=>", "y"]);
}

#[test]
fn index_method_reference_greater_than_operator_references() {
    let context = index_source({
        "
        x > y
        "
    });

    assert_local_diagnostics_eq!(
        &context,
        ["parse-warning: possibly useless use of > in void context (1:1-1:6)"]
    );

    assert_method_references_eq!(&context, ["x", "<=>", ">", "y"]);
}

#[test]
fn index_method_reference_constant_receiver() {
    let context = index_source({
        "
        Foo.bar
        "
    });

    assert_no_local_diagnostics!(&context);

    let method_ref = context.graph().method_references().values().next().unwrap();
    assert_eq!(StringId::from("bar"), *method_ref.str());

    let receiver = context.graph().names().get(&method_ref.receiver().unwrap()).unwrap();
    assert_eq!(StringId::from("<Foo>"), *receiver.str());
    assert!(receiver.nesting().is_none());

    let parent_scope = context
        .graph()
        .names()
        .get(&receiver.parent_scope().expect("Should exist"))
        .unwrap();
    assert_eq!(StringId::from("Foo"), *parent_scope.str());
    assert!(parent_scope.nesting().is_none());
    assert!(parent_scope.parent_scope().is_none());
}

#[test]
fn index_method_receiver_at_class_level() {
    let context = index_source({
        "
        class Foo
          self.bar
          baz
        end
        Foo.qux
        "
    });

    assert_no_local_diagnostics!(&context);

    assert_method_ref_receiver!(context, "bar", "<Foo>");
    assert_method_ref_receiver!(context, "baz", "<Foo>");
    assert_method_ref_receiver!(context, "qux", "<Foo>");
}

#[test]
fn index_method_receiver_self_at_module_level() {
    let context = index_source({
        "
        module Foo
          self.bar
        end
        "
    });

    assert_no_local_diagnostics!(&context);
    assert_method_ref_receiver!(context, "bar", "<Foo>");
}

#[test]
fn index_method_receiver_inside_singleton_class() {
    let context = index_source({
        "
        class Foo
          class << self
            self.bar
            baz
          end
        end
        "
    });

    assert_no_local_diagnostics!(&context);
    assert_method_ref_receiver!(context, "bar", "<<Foo>>");
    assert_method_ref_receiver!(context, "baz", "<<Foo>>");
}

#[test]
fn index_method_receiver_at_top_level() {
    let context = index_source({
        "
        self.bar
        "
    });

    assert_no_local_diagnostics!(&context);
    assert_method_ref_receiver!(context, "bar", "Object");
}

#[test]
fn index_method_reference_self_receiver() {
    let context = index_source({
        "
        class Foo
          def bar
            baz
          end

          def baz
          end
        end
        "
    });

    assert_no_local_diagnostics!(&context);

    let method_ref = context.graph().method_references().values().next().unwrap();
    assert_eq!(StringId::from("baz"), *method_ref.str());

    let receiver = context.graph().names().get(&method_ref.receiver().unwrap()).unwrap();
    assert_eq!(StringId::from("Foo"), *receiver.str());
    assert!(receiver.nesting().is_none());
    assert!(receiver.parent_scope().is_none());
}

#[test]
fn index_method_reference_explicit_self_receiver() {
    let context = index_source({
        "
        class Foo
          def bar
            self.baz
          end

          def baz
          end
        end
        "
    });

    assert_no_local_diagnostics!(&context);

    let method_ref = context.graph().method_references().values().next().unwrap();
    assert_eq!(StringId::from("baz"), *method_ref.str());

    let receiver = context.graph().names().get(&method_ref.receiver().unwrap()).unwrap();
    assert_eq!(StringId::from("Foo"), *receiver.str());
    assert!(receiver.nesting().is_none());
    assert!(receiver.parent_scope().is_none());
}

#[test]
fn index_method_reference_self_receiver_in_method_ref_with_receiver() {
    let context = index_source({
        "
        class Foo
          def Bar.bar
            baz
          end
        end
        "
    });

    assert_no_local_diagnostics!(&context);

    let method_ref = context.graph().method_references().values().next().unwrap();
    assert_eq!(StringId::from("baz"), *method_ref.str());

    let receiver = context.graph().names().get(&method_ref.receiver().unwrap()).unwrap();
    assert_eq!(StringId::from("<Bar>"), *receiver.str());
    assert!(receiver.nesting().is_none());

    let parent_scope = context
        .graph()
        .names()
        .get(&receiver.parent_scope().expect("Should exist"))
        .unwrap();
    assert_eq!(StringId::from("Bar"), *parent_scope.str());
    assert!(parent_scope.parent_scope().is_none());

    let nesting = context.graph().names().get(&parent_scope.nesting().unwrap()).unwrap();
    assert_eq!(StringId::from("Foo"), *nesting.str());
    assert!(nesting.nesting().is_none());
    assert!(nesting.parent_scope().is_none());
}

#[test]
fn index_method_reference_self_receiver_in_singleton_method() {
    let context = index_source({
        "
        class Foo
          def self.bar
            baz
          end
        end
        "
    });

    assert_no_local_diagnostics!(&context);

    let method_ref = context.graph().method_references().values().next().unwrap();
    assert_eq!(StringId::from("baz"), *method_ref.str());

    let receiver = context.graph().names().get(&method_ref.receiver().unwrap()).unwrap();
    assert_eq!(StringId::from("<Foo>"), *receiver.str());
    assert!(receiver.nesting().is_none());

    let parent_scope = context
        .graph()
        .names()
        .get(&receiver.parent_scope().expect("Should exist"))
        .unwrap();
    assert_eq!(StringId::from("Foo"), *parent_scope.str());
    assert!(parent_scope.parent_scope().is_none());
    assert!(parent_scope.nesting().is_none());
}

#[test]
fn index_method_reference_empty_message() {
    // Indexing this method reference is necessary for triggering the creation of the singleton class and its
    // ancestor linearization, which then triggers correct completion
    let context = index_source({
        "
        Foo.
        "
    });

    let method_ref = context.graph().method_references().values().next().unwrap();
    assert_eq!(StringId::from(""), *method_ref.str());

    let receiver = context.graph().names().get(&method_ref.receiver().unwrap()).unwrap();
    assert_eq!(StringId::from("<Foo>"), *receiver.str());
    assert!(receiver.nesting().is_none());

    let parent_scope = context
        .graph()
        .names()
        .get(&receiver.parent_scope().expect("Should exist"))
        .unwrap();
    assert_eq!(StringId::from("Foo"), *parent_scope.str());
    assert!(parent_scope.nesting().is_none());
    assert!(parent_scope.parent_scope().is_none());
}

#[test]
fn index_method_reference_singleton_class_receiver() {
    let context = index_source({
        "
        Foo.singleton_class.bar
        "
    });

    assert_no_local_diagnostics!(&context);

    let method_ref = context.graph().method_references().values().next().unwrap();
    assert_eq!(StringId::from("bar"), *method_ref.str());

    let receiver = context.graph().names().get(&method_ref.receiver().unwrap()).unwrap();
    assert_eq!(StringId::from("<<Foo>>"), *receiver.str(),);
    assert!(receiver.nesting().is_none());

    let singleton = context
        .graph()
        .names()
        .get(&receiver.parent_scope().expect("Should exist"))
        .unwrap();
    assert_eq!(StringId::from("<Foo>"), *singleton.str());
    assert!(singleton.nesting().is_none());

    let attached = context
        .graph()
        .names()
        .get(&singleton.parent_scope().expect("Should exist"))
        .unwrap();
    assert_eq!(StringId::from("Foo"), *attached.str());
    assert!(attached.nesting().is_none());
    assert!(attached.parent_scope().is_none());
}

#[test]
fn index_method_reference_greater_than_or_equal_to_operator_references() {
    let context = index_source({
        "
        x >= y
        "
    });

    assert_local_diagnostics_eq!(
        &context,
        ["parse-warning: possibly useless use of >= in void context (1:1-1:7)"]
    );

    assert_method_references_eq!(&context, ["x", "<=>", ">=", "y"]);
}

#[test]
fn index_method_reference_and_node_constant_receiver() {
    let context = index_source({
        "
        Foo && bar
        "
    });

    assert_no_local_diagnostics!(&context);

    let method_ref = context
        .graph()
        .method_references()
        .values()
        .find(|r| *r.str() == StringId::from("&&"))
        .unwrap();

    let receiver = context.graph().names().get(&method_ref.receiver().unwrap()).unwrap();
    assert_eq!(StringId::from("<Foo>"), *receiver.str());
    assert!(receiver.nesting().is_none());

    let parent_scope = context
        .graph()
        .names()
        .get(&receiver.parent_scope().expect("Should exist"))
        .unwrap();
    assert_eq!(StringId::from("Foo"), *parent_scope.str());
    assert!(parent_scope.nesting().is_none());
    assert!(parent_scope.parent_scope().is_none());
}

#[test]
fn index_method_reference_or_node_constant_receiver() {
    let context = index_source({
        "
        Foo || bar
        "
    });

    assert_no_local_diagnostics!(&context);

    let method_ref = context
        .graph()
        .method_references()
        .values()
        .find(|r| *r.str() == StringId::from("||"))
        .unwrap();

    let receiver = context.graph().names().get(&method_ref.receiver().unwrap()).unwrap();
    assert_eq!(StringId::from("<Foo>"), *receiver.str());
    assert!(receiver.nesting().is_none());

    let parent_scope = context
        .graph()
        .names()
        .get(&receiver.parent_scope().expect("Should exist"))
        .unwrap();
    assert_eq!(StringId::from("Foo"), *parent_scope.str());
    assert!(parent_scope.nesting().is_none());
    assert!(parent_scope.parent_scope().is_none());
}

#[test]
fn index_method_reference_alias_references() {
    let context = index_source({
        "
        alias ignored m1
        alias_method :ignored, :m2
        alias_method :ignored, ignored
        "
    });

    assert_no_local_diagnostics!(&context);
    assert_method_references_eq!(&context, ["m1()", "m2()"]);
}

#[test]
fn index_alias_method_ignores_method_nesting() {
    let context = index_source({
        "
        class Foo
          def bar
            alias_method :new_to_s, :to_s
          end
        end
        "
    });

    assert_no_local_diagnostics!(&context);

    assert_definition_at!(&context, "1:1-5:4", Class, |foo| {
        assert_definition_at!(&context, "3:5-3:34", MethodAlias, |alias_method| {
            assert_eq!(foo.id(), alias_method.lexical_nesting_id().unwrap());
        });
    });
}

#[test]
fn index_alias_ignores_method_nesting() {
    let context = index_source({
        "
        class Foo
          def bar
            alias new_to_s to_s
          end
        end
        "
    });

    assert_no_local_diagnostics!(&context);

    assert_definition_at!(&context, "1:1-5:4", Class, |foo| {
        assert_definition_at!(&context, "3:5-3:24", MethodAlias, |alias_method| {
            assert!(alias_method.receiver().is_none());
            assert_eq!(foo.id(), alias_method.lexical_nesting_id().unwrap());
        });
    });
}

#[test]
fn superclasses_are_indexed_as_constant_ref_ids() {
    let context = index_source({
        "
        class Foo < Bar; end
        "
    });

    assert_no_local_diagnostics!(&context);

    assert_definition_at!(&context, "1:1-1:21", Class, |def| {
        assert_def_superclass_ref_eq!(&context, def, "Bar");
    });
}

#[test]
fn constant_path_superclasses() {
    let context = index_source({
        "
        class Foo < Bar::Baz; end
        "
    });

    assert_no_local_diagnostics!(&context);

    let mut refs = context.graph().constant_references().values().collect::<Vec<_>>();
    refs.sort_by_key(|a| (a.offset().start(), a.offset().end()));

    assert_definition_at!(&context, "1:1-1:26", Class, |def| {
        assert_def_superclass_ref_eq!(&context, def, "Bar::Baz");
        assert_def_name_offset_eq!(&context, def, "1:7-1:10");
    });
}

#[test]
fn ignored_super_classes() {
    let context = index_source({
        "
        class Foo < method_call; end
        class Bar < 123; end
        class MyMigration < ActiveRecord::Migration[8.0]; end
        class Baz < foo::Bar; end
        "
    });

    assert_local_diagnostics_eq!(
        &context,
        [
            "dynamic-ancestor: Dynamic superclass (1:13-1:24)",
            "dynamic-ancestor: Dynamic superclass (2:13-2:16)",
            "dynamic-ancestor: Dynamic superclass (3:21-3:49)",
            "dynamic-constant-reference: Dynamic constant reference (4:13-4:16)",
            "dynamic-ancestor: Dynamic superclass (4:13-4:21)",
        ]
    );

    assert_definition_at!(&context, "1:1-1:29", Class, |def| {
        assert!(def.superclass_ref().is_none(),);
    });

    assert_definition_at!(&context, "2:1-2:21", Class, |def| {
        assert!(def.superclass_ref().is_none(),);
    });

    assert_definition_at!(&context, "3:1-3:54", Class, |def| {
        assert!(def.superclass_ref().is_none(),);
    });

    assert_definition_at!(&context, "4:1-4:26", Class, |def| {
        assert!(def.superclass_ref().is_none(),);
    });
}

#[test]
fn index_includes_at_top_level() {
    let context = index_source({
        "
        include Bar, Baz
        include Qux
        "
    });

    assert_no_local_diagnostics!(&context);

    // FIXME: This should be indexed
    assert_eq!(context.graph().definitions().len(), 0);
}

#[test]
fn index_includes_in_classes() {
    let context = index_source({
        "
        class Foo
          include Bar, Baz
          include Qux
        end
        "
    });

    assert_no_local_diagnostics!(&context);

    assert_definition_at!(&context, "1:1-4:4", Class, |def| {
        assert_def_mixins_eq!(&context, def, Include, ["Baz", "Bar", "Qux"]);
    });
}

#[test]
fn index_includes_in_modules() {
    let context = index_source({
        "
        module Foo
          include Bar, Baz
          include Qux
        end
        "
    });

    assert_no_local_diagnostics!(&context);

    assert_definition_at!(&context, "1:1-4:4", Module, |def| {
        assert_def_mixins_eq!(&context, def, Include, ["Baz", "Bar", "Qux"]);
    });
}

#[test]
fn index_prepends_at_top_level() {
    let context = index_source({
        "
        prepend Bar, Baz
        prepend Qux
        "
    });

    assert_no_local_diagnostics!(&context);

    // FIXME: This should be indexed
    assert_eq!(context.graph().definitions().len(), 0);
}

#[test]
fn index_prepends_in_classes() {
    let context = index_source({
        "
        class Foo
          prepend Bar, Baz
          prepend Qux
        end
        "
    });

    assert_no_local_diagnostics!(&context);

    assert_definition_at!(&context, "1:1-4:4", Class, |def| {
        assert_def_mixins_eq!(&context, def, Prepend, ["Baz", "Bar", "Qux"]);
    });
}

#[test]
fn index_prepends_in_modules() {
    let context = index_source({
        "
        module Foo
          prepend Bar, Baz
          prepend Qux
        end
        "
    });

    assert_no_local_diagnostics!(&context);

    assert_definition_at!(&context, "1:1-4:4", Module, |def| {
        assert_def_mixins_eq!(&context, def, Prepend, ["Baz", "Bar", "Qux"]);
    });
}

#[test]
fn index_extends_in_class() {
    let context = index_source({
        "
        class Foo
          extend Bar
          extend Baz
        end
        "
    });

    assert_no_local_diagnostics!(&context);

    assert_definition_at!(&context, "1:1-4:4", Class, |class_def| {
        assert_def_mixins_eq!(&context, class_def, Extend, ["Bar", "Baz"]);
    });
}

#[test]
fn index_mixins_self() {
    let context = index_source({
        "
        module Foo
          include self
          prepend self
          extend self
        end
        "
    });

    assert_no_local_diagnostics!(&context);

    assert_definition_at!(&context, "1:1-5:4", Module, |def| {
        assert_def_mixins_eq!(&context, def, Include, ["Foo"]);
        assert_def_mixins_eq!(&context, def, Prepend, ["Foo"]);
        assert_def_mixins_eq!(&context, def, Extend, ["Foo"]);
    });
}

#[test]
fn index_mixins_with_dynamic_constants() {
    let context = index_source({
        "
        include foo::Bar
        prepend foo::Baz
        extend foo::Qux

        include foo
        prepend 123
        extend 'x'
        "
    });

    assert_local_diagnostics_eq!(
        &context,
        [
            "dynamic-constant-reference: Dynamic constant reference (1:9-1:12)",
            "dynamic-ancestor: Dynamic mixin argument (1:9-1:17)",
            "dynamic-constant-reference: Dynamic constant reference (2:9-2:12)",
            "dynamic-ancestor: Dynamic mixin argument (2:9-2:17)",
            "dynamic-constant-reference: Dynamic constant reference (3:8-3:11)",
            "dynamic-ancestor: Dynamic mixin argument (3:8-3:16)",
            "dynamic-ancestor: Dynamic mixin argument (5:9-5:12)",
            "dynamic-ancestor: Dynamic mixin argument (6:9-6:12)",
            "dynamic-ancestor: Dynamic mixin argument (7:8-7:11)"
        ]
    );
    assert!(context.graph().definitions().is_empty());
}

#[test]
fn index_mixins_self_at_top_level() {
    let context = index_source({
        "
        include self
        prepend self
        extend self
        "
    });

    assert_local_diagnostics_eq!(
        &context,
        [
            "top-level-mixin-self: Top level mixin self (1:9-1:13)",
            "top-level-mixin-self: Top level mixin self (2:9-2:13)",
            "top-level-mixin-self: Top level mixin self (3:8-3:12)"
        ]
    );

    assert_eq!(context.graph().definitions().len(), 0);
}

#[test]
fn index_class_instance_variable() {
    let context = index_source({
        "
        class Foo
          @foo = 0

          class << self
            @bar = 1
          end
        end
        "
    });

    assert_no_local_diagnostics!(&context);

    assert_definition_at!(&context, "1:1-7:4", Class, |foo_class_def| {
        assert_definition_at!(&context, "2:3-2:7", InstanceVariable, |foo_var_def| {
            assert_def_str_eq!(&context, foo_var_def, "@foo");
            assert_eq!(foo_class_def.id(), foo_var_def.lexical_nesting_id().unwrap());
        });

        assert_definition_at!(&context, "4:3-6:6", SingletonClass, |foo_singleton_def| {
            assert_definition_at!(&context, "5:5-5:9", InstanceVariable, |bar_var_def| {
                assert_def_str_eq!(&context, bar_var_def, "@bar");
                assert_eq!(foo_singleton_def.id(), bar_var_def.lexical_nesting_id().unwrap());
            });
        });
    });
}

#[test]
fn index_instance_variable_inside_methods_stay_instance_variable() {
    let context = index_source({
        "
        class Foo
          def initialize
            @bar = 1
          end

          def self.class_method
            @baz = 2
          end

          class << self
            def singleton_method
              @qux = 3
            end
          end
        end
        "
    });

    assert_no_local_diagnostics!(&context);

    assert_definition_at!(&context, "3:5-3:9", InstanceVariable, |def| {
        assert_def_str_eq!(&context, def, "@bar");
    });

    assert_definition_at!(&context, "7:5-7:9", InstanceVariable, |def| {
        assert_def_str_eq!(&context, def, "@baz");
    });

    assert_definition_at!(&context, "12:7-12:11", InstanceVariable, |def| {
        assert_def_str_eq!(&context, def, "@qux");
    });
}

#[test]
fn index_instance_variable_in_method_with_non_self_receiver() {
    let context = index_source({
        "
        class Foo
          def String.bar
            @var = 123
          end
        end
        "
    });

    assert_no_local_diagnostics!(&context);

    // The instance variable is associated with the singleton class of String.
    // During indexing, we can't know what String resolves to because we haven't
    // resolved constants yet. The lexical nesting is the method definition.
    assert_definition_at!(&context, "1:1-5:4", Class, |_foo_class_def| {
        assert_definition_at!(&context, "2:3-4:6", Method, |method_def| {
            assert_definition_at!(&context, "3:5-3:9", InstanceVariable, |var_def| {
                assert_def_str_eq!(&context, var_def, "@var");
                // The lexical nesting of the ivar is the method
                assert_eq!(method_def.id(), var_def.lexical_nesting_id().unwrap());
            });
        });
    });
}

#[test]
fn index_alias_methods_nested() {
    let context = index_source({
        "
        class Foo
          alias foo bar
          alias :baz :qux
        end
        "
    });

    assert_no_local_diagnostics!(&context);

    assert_definition_at!(&context, "1:1-4:4", Class, |foo_class_def| {
        assert_definition_at!(&context, "2:3-2:16", MethodAlias, |def| {
            let new_name = context.graph().strings().get(def.new_name_str_id()).unwrap();
            let old_name = context.graph().strings().get(def.old_name_str_id()).unwrap();
            assert_eq!(new_name.as_str(), "foo()");
            assert_eq!(old_name.as_str(), "bar()");
            assert!(def.receiver().is_none());
            assert_eq!(foo_class_def.id(), def.lexical_nesting_id().unwrap());
        });

        assert_definition_at!(&context, "3:3-3:18", MethodAlias, |def| {
            let new_name = context.graph().strings().get(def.new_name_str_id()).unwrap();
            let old_name = context.graph().strings().get(def.old_name_str_id()).unwrap();
            assert_eq!(new_name.as_str(), "baz()");
            assert_eq!(old_name.as_str(), "qux()");
            assert!(def.receiver().is_none());
            assert_eq!(foo_class_def.id(), def.lexical_nesting_id().unwrap());
        });
    });
}

#[test]
fn index_alias_methods_top_level() {
    let context = index_source({
        "
        alias foo bar
        alias :baz :qux
        "
    });

    assert_no_local_diagnostics!(&context);

    assert_definition_at!(&context, "1:1-1:14", MethodAlias, |def| {
        let new_name = context.graph().strings().get(def.new_name_str_id()).unwrap();
        let old_name = context.graph().strings().get(def.old_name_str_id()).unwrap();
        assert_eq!(new_name.as_str(), "foo()");
        assert_eq!(old_name.as_str(), "bar()");
        assert!(def.receiver().is_none());
        assert!(def.lexical_nesting_id().is_none());
    });

    assert_definition_at!(&context, "2:1-2:16", MethodAlias, |def| {
        let new_name = context.graph().strings().get(def.new_name_str_id()).unwrap();
        let old_name = context.graph().strings().get(def.old_name_str_id()).unwrap();
        assert_eq!(new_name.as_str(), "baz()");
        assert_eq!(old_name.as_str(), "qux()");

        assert!(def.lexical_nesting_id().is_none());
    });
}

#[test]
fn index_module_alias_method() {
    let context = index_source({
        r#"
        alias_method :foo_symbol, :bar_symbol
        alias_method "foo_string", "bar_string"

        class Foo
          alias_method :baz, :qux
        end

        alias_method :baz, ignored
        alias_method ignored, :qux
        alias_method ignored, ignored
        "#
    });

    assert_no_local_diagnostics!(&context);

    assert_definition_at!(&context, "1:1-1:38", MethodAlias, |def| {
        let new_name = context.graph().strings().get(def.new_name_str_id()).unwrap();
        let old_name = context.graph().strings().get(def.old_name_str_id()).unwrap();
        assert_eq!(new_name.as_str(), "foo_symbol()");
        assert_eq!(old_name.as_str(), "bar_symbol()");
        assert!(def.receiver().is_none());
        assert!(def.lexical_nesting_id().is_none());
    });

    assert_definition_at!(&context, "2:1-2:40", MethodAlias, |def| {
        let new_name = context.graph().strings().get(def.new_name_str_id()).unwrap();
        let old_name = context.graph().strings().get(def.old_name_str_id()).unwrap();
        assert_eq!(new_name.as_str(), "foo_string()");
        assert_eq!(old_name.as_str(), "bar_string()");
        assert!(def.receiver().is_none());
        assert!(def.lexical_nesting_id().is_none());
    });

    assert_definition_at!(&context, "4:1-6:4", Class, |foo_class_def| {
        assert_definition_at!(&context, "5:3-5:26", MethodAlias, |def| {
            let new_name = context.graph().strings().get(def.new_name_str_id()).unwrap();
            let old_name = context.graph().strings().get(def.old_name_str_id()).unwrap();
            assert_eq!(new_name.as_str(), "baz()");
            assert_eq!(old_name.as_str(), "qux()");
            assert!(def.receiver().is_none());
            assert_eq!(foo_class_def.id(), def.lexical_nesting_id().unwrap());
        });
    });
}

#[test]
fn index_alias_method_with_self_receiver_maps_to_none() {
    let context = index_source({
        "
        class Foo
          self.alias_method :bar, :baz
        end
        "
    });

    assert_no_local_diagnostics!(&context);

    assert_definition_at!(&context, "2:3-2:31", MethodAlias, |def| {
        assert!(def.receiver().is_none());
    });
}

#[test]
fn index_alias_method_with_constant_receiver() {
    let context = index_source({
        "
        class Foo; end
        Foo.alias_method :bar, :baz
        "
    });

    assert_no_local_diagnostics!(&context);

    assert_definition_at!(&context, "2:1-2:28", MethodAlias, |def| {
        assert_string_eq!(&context, def.new_name_str_id(), "bar()");
        assert_string_eq!(&context, def.old_name_str_id(), "baz()");
        assert_method_has_receiver!(&context, def, "Foo");
    });
}

#[test]
fn index_alias_method_in_singleton_class_has_no_receiver() {
    let context = index_source({
        "
        class Foo
          def self.find; end

          class << self
            alias_method :find_old, :find
          end
        end
        "
    });

    assert_no_local_diagnostics!(&context);

    assert_definition_at!(&context, "1:1-7:4", Class, |_foo| {
        assert_definition_at!(&context, "4:3-6:6", SingletonClass, |singleton| {
            assert_definition_at!(&context, "5:5-5:34", MethodAlias, |def| {
                assert_string_eq!(&context, def.new_name_str_id(), "find_old()");
                assert_string_eq!(&context, def.old_name_str_id(), "find()");
                assert!(def.receiver().is_none());
                assert_eq!(singleton.id(), def.lexical_nesting_id().unwrap());
            });
        });
    });
}

#[test]
fn index_alias_keyword_in_singleton_class_has_no_receiver() {
    // Same as above: `alias` inside `class << self` has no receiver.
    let context = index_source({
        "
        class Foo
          def self.find; end

          class << self
            alias find_old find
          end
        end
        "
    });

    assert_no_local_diagnostics!(&context);

    assert_definition_at!(&context, "4:3-6:6", SingletonClass, |singleton| {
        assert_definition_at!(&context, "5:5-5:24", MethodAlias, |def| {
            assert!(def.receiver().is_none());
            assert_eq!(singleton.id(), def.lexical_nesting_id().unwrap());
        });
    });
}

#[test]
fn index_alias_method_with_nested_constant_receiver() {
    let context = index_source({
        "
        module A
          class B
            def original; end
          end
        end

        A::B.alias_method :new_name, :original
        "
    });

    assert_no_local_diagnostics!(&context);

    assert_definition_at!(&context, "7:1-7:39", MethodAlias, |def| {
        assert_string_eq!(&context, def.new_name_str_id(), "new_name()");
        assert_method_has_receiver!(&context, def, "B");
        assert!(def.lexical_nesting_id().is_none());
    });
}

#[test]
fn index_alias_method_with_dynamic_receiver_not_indexed() {
    let context = index_source({
        "
        class Foo
          def original; end
        end

        foo.alias_method :new_name, :original
        "
    });

    assert_no_local_diagnostics!(&context);

    let alias_count = context
        .graph()
        .definitions()
        .values()
        .filter(|def| matches!(def, Definition::MethodAlias(_)))
        .count();
    assert_eq!(0, alias_count);
}

#[test]
fn index_alias_global_variables() {
    let context = index_source({
        "
        alias $foo $bar

        class Foo
          alias $baz $qux
        end
        "
    });

    assert_no_local_diagnostics!(&context);

    assert_definition_at!(&context, "1:1-1:16", GlobalVariableAlias, |def| {
        let new_name = context.graph().strings().get(def.new_name_str_id()).unwrap();
        let old_name = context.graph().strings().get(def.old_name_str_id()).unwrap();
        assert_eq!(new_name.as_str(), "$foo");
        assert_eq!(old_name.as_str(), "$bar");

        assert!(def.lexical_nesting_id().is_none());
    });

    assert_definition_at!(&context, "3:1-5:4", Class, |foo_class_def| {
        assert_definition_at!(&context, "4:3-4:18", GlobalVariableAlias, |def| {
            let new_name = context.graph().strings().get(def.new_name_str_id()).unwrap();
            let old_name = context.graph().strings().get(def.old_name_str_id()).unwrap();
            assert_eq!(new_name.as_str(), "$baz");
            assert_eq!(old_name.as_str(), "$qux");

            assert_eq!(foo_class_def.id(), def.lexical_nesting_id().unwrap());
        });
    });
}

#[test]
fn index_module_new() {
    let context = index_source({
        "
        module Foo
          Bar = Module.new do
            include Baz

            def qux
              @var = 123
            end
            attr_reader :hello
          end
        end
        "
    });
    assert_no_local_diagnostics!(&context);

    assert_definition_at!(&context, "1:1-10:4", Module, |foo| {
        assert_definition_at!(&context, "2:3-9:6", Module, |bar| {
            assert_definition_at!(&context, "5:5-7:8", Method, |qux| {
                assert_definition_at!(&context, "6:7-6:11", InstanceVariable, |var| {
                    assert_definition_at!(&context, "8:18-8:23", AttrReader, |hello| {
                        assert_def_name_eq!(&context, bar, "Bar");
                        assert_eq!(foo.id(), bar.lexical_nesting_id().unwrap());
                        assert_eq!(foo.members()[0], bar.id());

                        assert_eq!(bar.members()[0], qux.id());
                        assert_eq!(bar.members()[1], var.id());
                        assert_eq!(bar.members()[2], hello.id());

                        // We expect the `Baz` constant name to NOT be associated with `Bar` because `Module.new` does not
                        // produce a new lexical scope
                        let include = bar.mixins().first().unwrap();
                        let name = context
                            .graph()
                            .names()
                            .get(
                                context
                                    .graph()
                                    .constant_references()
                                    .get(include.constant_reference_id())
                                    .unwrap()
                                    .name_id(),
                            )
                            .unwrap();

                        assert_eq!(StringId::from("Baz"), *name.str());
                        assert!(name.parent_scope().is_none());

                        let nesting_name = context.graph().names().get(&name.nesting().unwrap()).unwrap();
                        assert_eq!(StringId::from("Foo"), *nesting_name.str());
                    });
                });
            });
        });
    });
}

#[test]
fn index_module_new_with_constant_path() {
    let context = index_source({
        "
        module Foo
          Zip::Bar = Module.new do
            include Baz

            def qux
              @var = 123
            end
            attr_reader :hello
          end
        end
        "
    });
    assert_no_local_diagnostics!(&context);

    assert_definition_at!(&context, "1:1-10:4", Module, |foo| {
        assert_definition_at!(&context, "2:3-9:6", Module, |bar| {
            assert_definition_at!(&context, "5:5-7:8", Method, |qux| {
                assert_definition_at!(&context, "6:7-6:11", InstanceVariable, |var| {
                    assert_definition_at!(&context, "8:18-8:23", AttrReader, |hello| {
                        assert_def_name_eq!(&context, bar, "Zip::Bar");
                        assert_eq!(foo.id(), bar.lexical_nesting_id().unwrap());
                        assert_eq!(foo.members()[0], bar.id());

                        assert_eq!(bar.members()[0], qux.id());
                        assert_eq!(bar.members()[1], var.id());
                        assert_eq!(bar.members()[2], hello.id());

                        // We expect the `Baz` constant name to NOT be associated with `Bar` because `Module.new` does not
                        // produce a new lexical scope
                        let include = bar.mixins().first().unwrap();
                        let name = context
                            .graph()
                            .names()
                            .get(
                                context
                                    .graph()
                                    .constant_references()
                                    .get(include.constant_reference_id())
                                    .unwrap()
                                    .name_id(),
                            )
                            .unwrap();

                        assert_eq!(StringId::from("Baz"), *name.str());
                        assert!(name.parent_scope().is_none());

                        let nesting_name = context.graph().names().get(&name.nesting().unwrap()).unwrap();
                        assert_eq!(StringId::from("Foo"), *nesting_name.str());
                    });
                });
            });
        });
    });
}

#[test]
fn index_class_new() {
    let context = index_source({
        "
        module Foo
          Bar = Class.new(Parent) do
            include Baz

            def qux
              @var = 123
            end
            attr_reader :hello
          end
        end
        "
    });
    assert_no_local_diagnostics!(&context);

    assert_definition_at!(&context, "1:1-10:4", Module, |foo| {
        assert_definition_at!(&context, "2:3-9:6", Class, |bar| {
            assert_definition_at!(&context, "5:5-7:8", Method, |qux| {
                assert_definition_at!(&context, "6:7-6:11", InstanceVariable, |var| {
                    assert_definition_at!(&context, "8:18-8:23", AttrReader, |hello| {
                        assert_def_name_eq!(&context, bar, "Bar");
                        assert_eq!(foo.id(), bar.lexical_nesting_id().unwrap());
                        assert_eq!(foo.members()[0], bar.id());

                        assert_eq!(bar.members()[0], qux.id());
                        assert_eq!(bar.members()[1], var.id());
                        assert_eq!(bar.members()[2], hello.id());

                        assert_def_superclass_ref_eq!(&context, bar, "Parent");

                        // We expect the `Baz` constant name to NOT be associated with `Bar` because `Module.new` does not
                        // produce a new lexical scope
                        let include = bar.mixins().first().unwrap();
                        let name = context
                            .graph()
                            .names()
                            .get(
                                context
                                    .graph()
                                    .constant_references()
                                    .get(include.constant_reference_id())
                                    .unwrap()
                                    .name_id(),
                            )
                            .unwrap();

                        assert_eq!(StringId::from("Baz"), *name.str());
                        assert!(name.parent_scope().is_none());

                        let nesting_name = context.graph().names().get(&name.nesting().unwrap()).unwrap();
                        assert_eq!(StringId::from("Foo"), *nesting_name.str());
                    });
                });
            });
        });
    });
}

#[test]
fn index_class_new_no_parent() {
    let context = index_source({
        "
        module Foo
          Bar = Class.new do
            include Baz

            def qux
              @var = 123
            end
            attr_reader :hello
          end
        end
        "
    });
    assert_no_local_diagnostics!(&context);

    assert_definition_at!(&context, "1:1-10:4", Module, |foo| {
        assert_definition_at!(&context, "2:3-9:6", Class, |bar| {
            assert_definition_at!(&context, "5:5-7:8", Method, |qux| {
                assert_definition_at!(&context, "6:7-6:11", InstanceVariable, |var| {
                    assert_definition_at!(&context, "8:18-8:23", AttrReader, |hello| {
                        assert_def_name_eq!(&context, bar, "Bar");
                        assert_eq!(foo.id(), bar.lexical_nesting_id().unwrap());
                        assert_eq!(foo.members()[0], bar.id());

                        assert_eq!(bar.members()[0], qux.id());
                        assert_eq!(bar.members()[1], var.id());
                        assert_eq!(bar.members()[2], hello.id());

                        // We expect the `Baz` constant name to NOT be associated with `Bar` because `Module.new` does not
                        // produce a new lexical scope
                        let include = bar.mixins().first().unwrap();
                        let name = context
                            .graph()
                            .names()
                            .get(
                                context
                                    .graph()
                                    .constant_references()
                                    .get(include.constant_reference_id())
                                    .unwrap()
                                    .name_id(),
                            )
                            .unwrap();

                        assert_eq!(StringId::from("Baz"), *name.str());
                        assert!(name.parent_scope().is_none());

                        let nesting_name = context.graph().names().get(&name.nesting().unwrap()).unwrap();
                        assert_eq!(StringId::from("Foo"), *nesting_name.str());
                    });
                });
            });
        });
    });
}

#[test]
fn index_class_new_with_constant_path() {
    let context = index_source({
        "
        module Foo
          Zip::Bar = Class.new(Parent) do
            include Baz

            def qux
              @var = 123
            end
            attr_reader :hello
          end
        end
        "
    });
    assert_no_local_diagnostics!(&context);

    assert_definition_at!(&context, "1:1-10:4", Module, |foo| {
        assert_definition_at!(&context, "2:3-9:6", Class, |bar| {
            assert_definition_at!(&context, "5:5-7:8", Method, |qux| {
                assert_definition_at!(&context, "6:7-6:11", InstanceVariable, |var| {
                    assert_definition_at!(&context, "8:18-8:23", AttrReader, |hello| {
                        assert_def_name_eq!(&context, bar, "Zip::Bar");
                        assert_eq!(foo.id(), bar.lexical_nesting_id().unwrap());
                        assert_eq!(foo.members()[0], bar.id());

                        assert_eq!(bar.members()[0], qux.id());
                        assert_eq!(bar.members()[1], var.id());
                        assert_eq!(bar.members()[2], hello.id());

                        assert_def_superclass_ref_eq!(&context, bar, "Parent");

                        // We expect the `Baz` constant name to NOT be associated with `Bar` because `Module.new` does not
                        // produce a new lexical scope
                        let include = bar.mixins().first().unwrap();
                        let name = context
                            .graph()
                            .names()
                            .get(
                                context
                                    .graph()
                                    .constant_references()
                                    .get(include.constant_reference_id())
                                    .unwrap()
                                    .name_id(),
                            )
                            .unwrap();

                        assert_eq!(StringId::from("Baz"), *name.str());
                        assert!(name.parent_scope().is_none());

                        let nesting_name = context.graph().names().get(&name.nesting().unwrap()).unwrap();
                        assert_eq!(StringId::from("Foo"), *nesting_name.str());
                    });
                });
            });
        });
    });
}

#[test]
fn index_top_level_class_and_module_new() {
    let context = index_source({
        "
        module Foo
          Bar = ::Class.new do
          end

          Baz = ::Module.new do
          end
        end
        "
    });
    assert_no_local_diagnostics!(&context);

    assert_definition_at!(&context, "1:1-7:4", Module, |foo| {
        assert_definition_at!(&context, "2:3-3:6", Class, |bar| {
            assert_definition_at!(&context, "5:3-6:6", Module, |baz| {
                assert_def_name_eq!(&context, bar, "Bar");
                assert_def_name_eq!(&context, baz, "Baz");
                assert_eq!(foo.id(), bar.lexical_nesting_id().unwrap());
                assert_eq!(foo.id(), baz.lexical_nesting_id().unwrap());
                assert_eq!(foo.members()[0], bar.id());
                assert_eq!(foo.members()[1], baz.id());
            });
        });
    });
}

#[test]
fn index_anonymous_class_and_module_new() {
    let context = index_source({
        "
        module Foo
          Class.new do
            def bar; end
          end

          Module.new do
            def baz; end
          end
        end
        "
    });
    assert_no_local_diagnostics!(&context);

    assert_definition_at!(&context, "1:1-9:4", Module, |foo| {
        assert_definition_at!(&context, "2:3-4:6", Class, |anonymous| {
            assert_eq!(foo.id(), anonymous.lexical_nesting_id().unwrap());

            assert_definition_at!(&context, "3:5-3:17", Method, |bar| {
                assert_eq!(anonymous.id(), bar.lexical_nesting_id().unwrap());
            });
        });

        assert_definition_at!(&context, "6:3-8:6", Module, |anonymous| {
            assert_eq!(foo.id(), anonymous.lexical_nesting_id().unwrap());

            assert_definition_at!(&context, "7:5-7:17", Method, |baz| {
                assert_eq!(anonymous.id(), baz.lexical_nesting_id().unwrap());
            });
        });
    });
}

#[test]
fn index_nested_class_and_module_new() {
    let context = index_source({
        "
        module Foo
          Class.new do
            Module.new do
            end
          end
        end
        "
    });
    assert_no_local_diagnostics!(&context);

    assert_definition_at!(&context, "1:1-6:4", Module, |foo| {
        assert_definition_at!(&context, "2:3-5:6", Class, |anonymous_class| {
            assert_eq!(foo.id(), anonymous_class.lexical_nesting_id().unwrap());

            assert_definition_at!(&context, "3:5-4:8", Module, |anonymous_module| {
                assert_eq!(foo.id(), anonymous_module.lexical_nesting_id().unwrap());
            });
        });
    });
}

#[test]
fn index_named_module_nested_inside_anonymous() {
    let context = index_source({
        "
        module Foo
          Class.new do
            module Bar
            end
          end
        end
        "
    });
    assert_no_local_diagnostics!(&context);

    assert_definition_at!(&context, "1:1-6:4", Module, |foo| {
        assert_definition_at!(&context, "2:3-5:6", Class, |anonymous_class| {
            assert_eq!(foo.id(), anonymous_class.lexical_nesting_id().unwrap());

            assert_definition_at!(&context, "3:5-4:8", Module, |bar| {
                assert_eq!(foo.id(), bar.lexical_nesting_id().unwrap());
            });
        });
    });
}

#[test]
fn index_anonymous_namespace_mixins() {
    let context = index_source({
        "
        module Foo
          Class.new do
            include Bar
          end
        end
        "
    });
    assert_no_local_diagnostics!(&context);

    assert_definition_at!(&context, "1:1-5:4", Module, |foo| {
        assert_definition_at!(&context, "2:3-4:6", Class, |anonymous_class| {
            assert_eq!(foo.id(), anonymous_class.lexical_nesting_id().unwrap());

            assert_def_mixins_eq!(&context, anonymous_class, Include, ["Bar"]);
        });
    });
}

#[test]
fn index_singleton_method_in_class_new() {
    let context = index_source({
        "
        module Foo
          A = Class.new do
            def self.bar
            end
          end
        end
        "
    });
    assert_no_local_diagnostics!(&context);

    assert_definition_at!(&context, "3:5-4:8", Method, |bar| {
        let Receiver::SelfReceiver(def_id) = bar.receiver().as_ref().unwrap() else {
            panic!("Expected SelfReceiver for def self.bar in Class.new");
        };
        let def = context.graph().definitions().get(def_id).unwrap();
        let name_id = def.name_id().expect("Owner definition should have a name_id");
        let name_ref = context.graph().names().get(name_id).unwrap();
        assert_eq!(StringId::from("A"), *name_ref.str());

        let nesting_name = context.graph().names().get(&name_ref.nesting().unwrap()).unwrap();
        assert_eq!(StringId::from("Foo"), *nesting_name.str());
    });
}

#[test]
fn index_class_variable_in_class_new() {
    let context = index_source({
        "
        module Foo
          A = Class.new do
            def bar
              @@var = 123
            end
          end
        end
        "
    });
    assert_no_local_diagnostics!(&context);

    assert_definition_at!(&context, "1:1-7:4", Module, |foo| {
        assert_definition_at!(&context, "4:7-4:12", ClassVariable, |var| {
            assert_eq!(foo.id(), var.lexical_nesting_id().unwrap());
        });
    });
}

#[test]
fn index_singleton_method_in_anonymous_namespace() {
    let context = index_source({
        "
        module Foo
          Class.new do
            def self.bar
            end
          end
        end
        "
    });
    assert_no_local_diagnostics!(&context);

    assert_definition_at!(&context, "3:5-4:8", Method, |bar| {
        let Receiver::SelfReceiver(def_id) = bar.receiver().as_ref().unwrap() else {
            panic!("Expected SelfReceiver for def self.bar in anonymous Class.new");
        };
        let def = context.graph().definitions().get(def_id).unwrap();
        let name_id = def.name_id().expect("Owner definition should have a name_id");
        let name_ref = context.graph().names().get(name_id).unwrap();
        let uri_id = UriId::from("file:///foo.rb");
        assert_eq!(StringId::from(&format!("{uri_id}:13<anonymous>")), *name_ref.str());
        assert!(name_ref.nesting().is_none());
        assert!(name_ref.parent_scope().is_none());
    });
}

#[test]
fn index_nested_method_definitions() {
    let context = index_source({
        "
        class Foo
          def bar
            def baz; end
          end
        end
        "
    });
    assert_no_local_diagnostics!(&context);

    assert_definition_at!(&context, "1:1-5:4", Class, |foo| {
        assert_definition_at!(&context, "2:3-4:6", Method, |bar| {
            assert_definition_at!(&context, "3:5-3:17", Method, |baz| {
                assert_eq!(foo.id(), bar.lexical_nesting_id().unwrap());
                assert_eq!(foo.id(), baz.lexical_nesting_id().unwrap());
            });
        });
    });
}

#[test]
fn index_constant_alias_simple() {
    let context = index_source({
        "
        module Foo; end
        ALIAS1 = Foo
        ALIAS2 ||= Foo
        "
    });

    assert_no_local_diagnostics!(&context);

    assert_definition_at!(&context, "2:1-2:7", ConstantAlias, |def| {
        assert_def_name_eq!(&context, def, "ALIAS1");
        assert_name_path_eq!(&context, "Foo", *def.target_name_id());
    });
    assert_definition_at!(&context, "3:1-3:7", ConstantAlias, |def| {
        assert_def_name_eq!(&context, def, "ALIAS2");
        assert_name_path_eq!(&context, "Foo", *def.target_name_id());
    });
}

#[test]
fn index_constant_alias_to_path() {
    let context = index_source({
        "
        module Foo
          module Bar; end
        end
        ALIAS = Foo::Bar
        "
    });

    assert_no_local_diagnostics!(&context);

    assert_definition_at!(&context, "4:1-4:6", ConstantAlias, |def| {
        assert_def_name_eq!(&context, def, "ALIAS");
        assert_name_path_eq!(&context, "Foo::Bar", *def.target_name_id());
    });

    assert_constant_references_eq!(&context, ["Foo", "Bar"]);
}

#[test]
fn index_constant_alias_nested() {
    let context = index_source({
        "
        module Foo; end
        module Bar
          MyFoo = Foo
        end
        "
    });

    assert_no_local_diagnostics!(&context);

    assert_definition_at!(&context, "2:1-4:4", Module, |bar_module_def| {
        assert_definition_at!(&context, "3:3-3:8", ConstantAlias, |def| {
            assert_def_name_eq!(&context, def, "MyFoo");
            assert_eq!(bar_module_def.id(), def.lexical_nesting_id().unwrap());
        });
    });
}

#[test]
fn index_scoped_constant_alias() {
    let context = index_source({
        "
        module Foo; end
        module Bar; end
        Bar::ALIAS = Foo
        "
    });

    assert_no_local_diagnostics!(&context);

    assert_definition_at!(&context, "3:6-3:11", ConstantAlias, |def| {
        assert_def_name_eq!(&context, def, "Bar::ALIAS");
    });
}

#[test]
fn index_chained_constant_alias() {
    let context = index_source({
        "
        module Target; end
        A = B = Target
        "
    });

    assert_no_local_diagnostics!(&context);

    assert_definition_at!(&context, "2:1-2:2", ConstantAlias, |def| {
        assert_def_name_eq!(&context, def, "A");
        assert_name_path_eq!(&context, "Target", *def.target_name_id());
    });
    assert_definition_at!(&context, "2:5-2:6", ConstantAlias, |def| {
        assert_def_name_eq!(&context, def, "B");
        assert_name_path_eq!(&context, "Target", *def.target_name_id());
    });

    assert_constant_references_eq!(&context, ["Target"]);
}

#[test]
fn index_constant_alias_to_top_level_constant() {
    let context = index_source({
        "
        module Foo; end
        ALIAS = ::Foo
        "
    });

    assert_no_local_diagnostics!(&context);

    assert_definition_at!(&context, "2:1-2:6", ConstantAlias, |def| {
        assert_def_name_eq!(&context, def, "ALIAS");
        assert_name_path_eq!(&context, "Foo", *def.target_name_id());
    });
}

#[test]
fn index_constant_alias_chain() {
    let context = index_source({
        "
        module Foo; end
        ALIAS1 = Foo
        ALIAS2 = ALIAS1
        "
    });

    assert_no_local_diagnostics!(&context);

    assert_definition_at!(&context, "2:1-2:7", ConstantAlias, |def| {
        assert_def_name_eq!(&context, def, "ALIAS1");
        assert_name_path_eq!(&context, "Foo", *def.target_name_id());
    });
    assert_definition_at!(&context, "3:1-3:7", ConstantAlias, |def| {
        assert_def_name_eq!(&context, def, "ALIAS2");
        assert_name_path_eq!(&context, "ALIAS1", *def.target_name_id());
    });
}

// Comments

#[test]
fn index_comments_attached_to_definitions() {
    let context = index_source({
        "
        # Single comment
        class Single; end

        # Multi-line comment 1
        # Multi-line comment 2
        # Multi-line comment 3
        module Multi; end

        # Comment 1
        #
        # Comment 2
        class EmptyCommentLine; end

        # Comment directly above (no gap)
        NoGap = 42

        #: ()
        #| -> void
        def foo; end

        # Comment with blank line

        class BlankLine; end

        # Too far away


        class NoComment; end
        "
    });

    assert_no_local_diagnostics!(&context);

    assert_definition_at!(&context, "2:1-2:18", Class, |def| {
        assert_def_name_eq!(&context, def, "Single");
        assert_def_comments_eq!(&context, def, ["# Single comment"]);
    });

    assert_definition_at!(&context, "7:1-7:18", Module, |def| {
        assert_def_name_eq!(&context, def, "Multi");
        assert_def_comments_eq!(
            &context,
            def,
            [
                "# Multi-line comment 1",
                "# Multi-line comment 2",
                "# Multi-line comment 3"
            ]
        );
    });

    assert_definition_at!(&context, "12:1-12:28", Class, |def| {
        assert_def_name_eq!(&context, def, "EmptyCommentLine");
        assert_def_comments_eq!(&context, def, ["# Comment 1", "#", "# Comment 2"]);
    });

    assert_definition_at!(&context, "15:1-15:6", Constant, |def| {
        assert_def_name_eq!(&context, def, "NoGap");
        assert_def_comments_eq!(&context, def, ["# Comment directly above (no gap)"]);
    });

    assert_definition_at!(&context, "19:1-19:13", Method, |def| {
        assert_def_str_eq!(&context, def, "foo()");
        assert_def_comments_eq!(&context, def, ["#: ()", "#| -> void"]);
    });

    assert_definition_at!(&context, "23:1-23:21", Class, |def| {
        assert_def_name_eq!(&context, def, "BlankLine");
        assert_def_comments_eq!(&context, def, ["# Comment with blank line"]);
    });

    assert_definition_at!(&context, "28:1-28:21", Class, |def| {
        assert_def_name_eq!(&context, def, "NoComment");
        assert!(def.comments().is_empty());
    });
}

#[test]
fn index_comments_indented_and_nested() {
    let context = index_source({
        "
        # Outer class
        class Outer
          # Inner class at 2 spaces
          class Inner
            # Deep class at 4 spaces
            class Deep; end
          end

          # Another inner class
          # with multiple lines
          class AnotherInner; end
        end
        "
    });

    assert_no_local_diagnostics!(&context);

    assert_definition_at!(&context, "2:1-12:4", Class, |def| {
        assert_def_name_eq!(&context, def, "Outer");
        assert_def_comments_eq!(&context, def, ["# Outer class"]);
    });

    assert_definition_at!(&context, "4:3-7:6", Class, |def| {
        assert_def_name_eq!(&context, def, "Inner");
        assert_def_comments_eq!(&context, def, ["# Inner class at 2 spaces"]);
    });

    assert_definition_at!(&context, "6:5-6:20", Class, |def| {
        assert_def_name_eq!(&context, def, "Deep");
        assert_def_comments_eq!(&context, def, ["# Deep class at 4 spaces"]);
    });

    assert_definition_at!(&context, "11:3-11:26", Class, |def| {
        assert_def_name_eq!(&context, def, "AnotherInner");
        assert_def_comments_eq!(&context, def, ["# Another inner class", "# with multiple lines"]);
    });
}

#[test]
fn index_comments_with_tags() {
    let context = index_source({
        "
        # @deprecated
        class Deprecated; end

        class NotDeprecated; end

        # Multi-line comment
        # @deprecated Use something else
        def deprecated_method; end

        # Not @deprecated
        def not_deprecated_method; end
        "
    });

    assert!(context.definition_at("2:1-2:22").is_deprecated());
    assert!(!context.definition_at("4:1-4:25").is_deprecated());
    assert!(context.definition_at("8:1-8:27").is_deprecated());
    assert!(!context.definition_at("11:1-11:31").is_deprecated());
}

#[test]
fn index_comments_attr_accessor() {
    let context = index_source({
        "
        class Foo
          # Comment
          attr_reader :foo

          # Comment 1
          # Comment 2
          # Comment 3
          attr_writer :bar

          # Comment 1
          # Comment 2
          # Comment 3
          attr_accessor :baz, :qux

          # Comment
          attr :quux, true
        end
        "
    });

    assert_no_local_diagnostics!(&context);

    assert_definition_at!(&context, "3:16-3:19", AttrReader, |def| {
        assert_def_comments_eq!(&context, def, ["# Comment"]);
    });

    assert_definition_at!(&context, "8:16-8:19", AttrWriter, |def| {
        assert_def_comments_eq!(&context, def, ["# Comment 1", "# Comment 2", "# Comment 3"]);
    });

    assert_definition_at!(&context, "13:18-13:21", AttrAccessor, |def| {
        assert_def_comments_eq!(&context, def, ["# Comment 1", "# Comment 2", "# Comment 3"]);
    });

    assert_definition_at!(&context, "13:24-13:27", AttrAccessor, |def| {
        assert_def_comments_eq!(&context, def, ["# Comment 1", "# Comment 2", "# Comment 3"]);
    });

    assert_definition_at!(&context, "16:9-16:13", AttrAccessor, |def| {
        assert_def_comments_eq!(&context, def, ["# Comment"]);
    });
}

#[test]
fn index_comments_on_top_of_signature() {
    let context = index_source({
        "
        class Foo
          # Bar docs
          # are here
          sig { returns(Integer) }
          attr_reader :bar

          # Baz docs
          # are in this other place
          sig do
            params(x: Integer).void
          end
          def baz(x); end
        end
        "
    });

    assert_no_local_diagnostics!(&context);

    assert_definition_at!(&context, "5:16-5:19", AttrReader, |def| {
        assert_def_comments_eq!(&context, def, ["# Bar docs", "# are here"]);
    });

    assert_definition_at!(&context, "12:3-12:18", Method, |def| {
        assert_def_comments_eq!(&context, def, ["# Baz docs", "# are in this other place"]);
    });
}

#[test]
fn index_comments_on_top_of_multiple_attribute_signature() {
    let context = index_source({
        "
        class Foo
          # Docs
          sig { returns(Integer) }
          attr_reader :bar, :baz
        end
        "
    });

    assert_no_local_diagnostics!(&context);

    assert_definition_at!(&context, "4:16-4:19", AttrReader, |def| {
        assert_def_comments_eq!(&context, def, ["# Docs"]);
    });

    assert_definition_at!(&context, "4:22-4:25", AttrReader, |def| {
        assert_def_comments_eq!(&context, def, ["# Docs"]);
    });
}

#[test]
fn index_comments_on_sig_without_runtime() {
    let context = index_source({
        "
        class Foo
          # Docs
          T::Sig::WithoutRuntime.sig { returns(Integer) }
          def bar; end
        end
        "
    });

    assert_no_local_diagnostics!(&context);

    assert_definition_at!(&context, "4:3-4:15", Method, |def| {
        assert_def_comments_eq!(&context, def, ["# Docs"]);
    });
}

#[test]
fn index_comments_blank_line_between_annotation_and_def() {
    let context = index_source({
        "
        class Foo
          # Docs
          sig { returns(Integer) }

          def bar; end
        end
        "
    });

    assert_no_local_diagnostics!(&context);

    assert_definition_at!(&context, "5:3-5:15", Method, |def| {
        assert!(def.comments().is_empty());
    });
}

#[test]
fn index_double_line_between_comment_and_annotation() {
    let context = index_source({
        "
        class Foo
          # Docs for bar


          sig { params(x: Integer).void }
          def bar(x); end
        end
        "
    });

    assert_no_local_diagnostics!(&context);

    assert_definition_at!(&context, "6:3-6:18", Method, |def| {
        assert!(def.comments().is_empty());
    });
}

#[test]
fn index_line_between_comment_and_annotation() {
    let context = index_source({
        "
        class Foo
          # Docs for bar

          sig { params(x: Integer).void }
          def bar(x); end
        end
        "
    });

    assert_no_local_diagnostics!(&context);

    assert_definition_at!(&context, "5:3-5:18", Method, |def| {
        assert_def_comments_eq!(&context, def, ["# Docs for bar"]);
    });
}

#[test]
fn index_anything_between_comment_and_annotation() {
    let context = index_source({
        "
        class Foo
          # Docs for bar
          sig { params(x: Integer).void }
          something_else
          def bar(x); end
        end
        "
    });

    assert_no_local_diagnostics!(&context);

    assert_definition_at!(&context, "5:3-5:18", Method, |def| {
        assert!(def.comments().is_empty());
    });
}

#[test]
fn index_comments_annotation_does_not_leak_through_other_code() {
    let context = index_source({
        "
        class Foo
          # Should not leak
          sig { returns(Integer) }
          include SomeModule

          # Docs for bar
          def bar; end
        end
        "
    });

    assert_no_local_diagnostics!(&context);

    assert_definition_at!(&context, "7:3-7:15", Method, |def| {
        assert_def_comments_eq!(&context, def, ["# Docs for bar"]);
    });
}

#[test]
fn index_comments_decorator_above_private_def() {
    let context = index_source({
        "
        class Foo
          # Docs for foo
          sig { params(x: Integer).void }
          private def foo(x); end

          # Docs for bar
          sig { returns(Integer) }
          private attr_reader :bar
        end
        "
    });

    assert_no_local_diagnostics!(&context);

    assert_definition_at!(&context, "4:11-4:26", Method, |def| {
        assert_def_comments_eq!(&context, def, ["# Docs for foo"]);
    });

    assert_definition_at!(&context, "8:24-8:27", AttrReader, |def| {
        assert_def_comments_eq!(&context, def, ["# Docs for bar"]);
    });
}

#[test]
fn index_comments_visibility() {
    let context = index_source({
        "
        class Foo
          # Comment
          private def foo; end

          # Comment
          protected def bar; end

          # Comment
          public def baz; end

          # Comment
          private attr_reader :qux
        end
        "
    });

    assert_definition_at!(&context, "3:11-3:23", Method, |def| {
        assert_def_comments_eq!(&context, def, ["# Comment"]);
    });

    assert_definition_at!(&context, "6:13-6:25", Method, |def| {
        assert_def_comments_eq!(&context, def, ["# Comment"]);
    });

    assert_definition_at!(&context, "9:10-9:22", Method, |def| {
        assert_def_comments_eq!(&context, def, ["# Comment"]);
    });

    assert_definition_at!(&context, "12:24-12:27", AttrReader, |def| {
        assert_def_comments_eq!(&context, def, ["# Comment"]);
    });
}

#[test]
fn method_call_operators() {
    let context = index_source({
        "
        Foo.bar ||= {}
        Foo.bar += {}
        Foo.bar &&= {}
        "
    });

    assert_no_local_diagnostics!(&context);
    // We expect two constant references for `Foo` and `<Foo>` on each singleton method call
    assert_eq!(6, context.graph().constant_references().len());
}

#[test]
fn invoking_new_creates_singleton_reference() {
    let context = index_source(
        r"
        class Foo; end
        Foo.new.bar
        ",
    );

    assert_no_local_diagnostics!(&context);
    // We expect two constant references for `Foo` and `<Foo>` due to the new call
    assert_eq!(2, context.graph().constant_references().len());
}

#[test]
fn class_new_creates_singleton_reference() {
    let context = index_source(
        r"
        CONST = Class.new
        ",
    );

    assert_no_local_diagnostics!(&context);
    // We expect two constant references for `Class` and `<Class>` due to the new call
    assert_eq!(2, context.graph().constant_references().len());
}

#[test]
fn module_new_creates_singleton_reference() {
    let context = index_source(
        r"
        CONST = Module.new
        ",
    );

    assert_no_local_diagnostics!(&context);
    // We expect two constant references for `Module` and `<Module>` due to the new call
    assert_eq!(2, context.graph().constant_references().len());
}

#[test]
fn constant_with_call_value_is_promotable() {
    let context = index_source("Foo = some_call");

    assert_definition_at!(&context, "1:1-1:4", Constant, |def| {
        assert_promotable!(def);
    });
}

#[test]
fn constant_with_literal_value_is_not_promotable() {
    let context = index_source("FOO = 42");

    assert_definition_at!(&context, "1:1-1:4", Constant, |def| {
        assert_not_promotable!(def);
    });
}

#[test]
fn constant_with_operator_call_is_not_promotable() {
    let context = index_source("FOO = 1 + 2");

    assert_definition_at!(&context, "1:1-1:4", Constant, |def| {
        assert_not_promotable!(def);
    });
}

#[test]
fn constant_with_dot_call_is_promotable() {
    let context = index_source("Foo = Bar.new");

    assert_definition_at!(&context, "1:1-1:4", Constant, |def| {
        assert_promotable!(def);
    });
}

#[test]
fn constant_with_colon_colon_call_is_promotable() {
    let context = index_source("Foo = Bar::new");

    assert_definition_at!(&context, "1:1-1:4", Constant, |def| {
        assert_promotable!(def);
    });
}

#[test]
fn index_private_constant_calls() {
    let context = index_source({
        r#"
        module Foo
          BAR = 42
          BAZ = 43
          FOO = 44

          private_constant :BAR, :BAZ
          private_constant "FOO"

          class Qux
            BAR = 42
            BAZ = 43

            Foo.public_constant :BAR
            Foo.public_constant "BAZ"
          end

          self.private_constant :Qux
        end

        Foo.public_constant :BAR
        "#
    });

    assert_no_local_diagnostics!(&context);

    assert_definition_at!(&context, "6:21-6:24", ConstantVisibility, |def| {
        assert_def_name_eq!(&context, def, "BAR");
        assert_eq!(def.visibility(), &Visibility::Private);
    });
    assert_definition_at!(&context, "6:27-6:30", ConstantVisibility, |def| {
        assert_def_name_eq!(&context, def, "BAZ");
        assert_eq!(def.visibility(), &Visibility::Private);
    });
    assert_definition_at!(&context, "7:20-7:25", ConstantVisibility, |def| {
        assert_def_name_eq!(&context, def, "FOO");
        assert_eq!(def.visibility(), &Visibility::Private);
    });
    assert_definition_at!(&context, "13:26-13:29", ConstantVisibility, |def| {
        assert_def_name_eq!(&context, def, "Foo::BAR");
        assert_eq!(def.visibility(), &Visibility::Public);
    });
    assert_definition_at!(&context, "14:25-14:30", ConstantVisibility, |def| {
        assert_def_name_eq!(&context, def, "Foo::BAZ");
        assert_eq!(def.visibility(), &Visibility::Public);
    });
    assert_definition_at!(&context, "17:26-17:29", ConstantVisibility, |def| {
        assert_def_name_eq!(&context, def, "Qux");
        assert_eq!(def.visibility(), &Visibility::Private);
    });
    assert_definition_at!(&context, "20:22-20:25", ConstantVisibility, |def| {
        assert_def_name_eq!(&context, def, "Foo::BAR");
        assert_eq!(def.visibility(), &Visibility::Public);
    });
}

#[test]
fn index_private_constant_calls_diagnostics() {
    let context = index_source({
        "
        private_constant :NOT_INDEXED
        self.private_constant :NOT_INDEXED
        foo.private_constant :NOT_INDEXED # not indexed, dynamic receiver

        module Foo
          private_constant NOT_INDEXED, not_indexed # not indexed, not a symbol
          private_constant # not indexed, no arguments

          def self.qux
            private_constant :Bar # not indexed, dynamic
          end

          def foo
            private_constant :Bar # not indexed, dynamic
          end
        end
        "
    });

    assert_local_diagnostics_eq!(
        &context,
        vec![
            "invalid-private-constant: Private constant called at top level (1:1-1:30)",
            "invalid-private-constant: Private constant called at top level (2:1-2:35)",
            "invalid-private-constant: Dynamic receiver for private constant (3:1-3:34)",
            "invalid-private-constant: Private constant called with non-symbol argument (6:20-6:31)",
        ]
    );

    assert_eq!(context.graph().definitions().len(), 3); // Foo, Foo::Qux, Foo#foo
}

mod name_dependent_tests {
    use super::*;

    #[test]
    fn track_dependency_chain() {
        let context = index_source(
            "
            module Bar; end
            CONST = 1
            CONST2 = CONST

            module Foo
              class Bar::Baz
                CONST
              end

              CONST2
            end
            ",
        );

        assert_dependents!(&context, "Bar", [ChildName("Baz")]);
        assert_dependents!(&context, "Foo", [NestedName("Baz"), NestedName("CONST2")]);
        assert_dependents!(&context, "Bar::Baz", [Definition("Baz"), NestedName("CONST")]);
    }

    #[test]
    fn multi_level_chain() {
        let context = index_source(
            "
            module Foo
              module Bar
                module Baz
                end
              end
            end
            ",
        );

        assert_dependents!(&context, "Foo", [NestedName("Bar")]);
        assert_dependents!(&context, "Bar", [NestedName("Baz")]);
    }

    #[test]
    fn singleton_class() {
        let context = index_source(
            "
            class Foo
              class << self
                def bar; end
              end
            end
            ",
        );

        assert_dependents!(&context, "Foo", [ChildName("<Foo>")]);
    }

    #[test]
    fn nested_vs_compact() {
        let context = index_source(
            "
            module Foo
              class Bar; end
              class Foo::Baz; end
            end
            ",
        );

        assert_dependents!(&context, "Foo", [NestedName("Bar"), ChildName("Baz")]);
    }
}
