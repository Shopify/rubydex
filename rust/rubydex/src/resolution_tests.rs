use super::Resolver;
use crate::{
    assert_alias_targets_contain, assert_ancestors_eq, assert_constant_alias_target_eq, assert_constant_reference_to,
    assert_constant_reference_unresolved, assert_declaration_definitions_count_eq, assert_declaration_does_not_exist,
    assert_declaration_exists, assert_declaration_kind_eq, assert_declaration_references_count_eq, assert_descendants,
    assert_diagnostics_eq, assert_instance_variables_eq, assert_members_eq, assert_no_constant_alias_target,
    assert_no_diagnostics, assert_no_members, assert_owner_eq, assert_singleton_class_eq,
    diagnostic::Rule,
    model::{declaration::Ancestors, ids::DeclarationId, name::NameRef},
    test_utils::GraphTest,
};

mod constant_resolution_tests {
    use super::*;

    #[test]
    fn resolving_top_level_references() {
        let mut context = GraphTest::new();
        context.index_uri("file:///bar.rb", {
            r"
            class Bar; end

            ::Bar
            Bar
            "
        });
        context.index_uri("file:///foo.rb", {
            r"
            module Foo
              ::Bar
            end
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context, &[Rule::ParseWarning]);

        assert_constant_reference_to!(context, "Bar", "file:///bar.rb:3:3-3:6");
        assert_constant_reference_to!(context, "Bar", "file:///bar.rb:4:1-4:4");
        assert_constant_reference_to!(context, "Bar", "file:///foo.rb:2:5-2:8");
    }

    #[test]
    fn resolving_nested_reference() {
        let mut context = GraphTest::new();
        context.index_uri("file:///bar.rb", {
            r"
            module Foo
              CONST = 123

              class Bar
                CONST
                Foo::CONST
              end
            end
            "
        });
        context.resolve();

        assert_constant_reference_to!(context, "Foo::CONST", "file:///bar.rb:5:5-5:10");
        assert_constant_reference_to!(context, "Foo::CONST", "file:///bar.rb:6:10-6:15");
    }

    #[test]
    fn resolving_nested_reference_that_refer_to_top_level_constant() {
        let mut context = GraphTest::new();
        context.index_uri("file:///bar.rb", {
            r"
            class Baz; end

            module Foo
              class Bar
                Baz
              end
            end
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context);

        assert_constant_reference_to!(context, "Baz", "file:///bar.rb:5:5-5:8");
    }

    #[test]
    fn resolving_constant_path_references_at_top_level() {
        let mut context = GraphTest::new();
        context.index_uri("file:///bar.rb", {
            r"
            module Foo
              class Bar; end
            end

            Foo::Bar
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context, &[Rule::ParseWarning]);

        assert_constant_reference_to!(context, "Foo::Bar", "file:///bar.rb:5:6-5:9");
    }

    #[test]
    fn resolving_reference_for_non_existing_declaration() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
              Foo
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context, &[Rule::ParseWarning]);
        assert_constant_reference_unresolved!(context, "Foo");
    }

    #[test]
    fn resolution_for_top_level_references() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            module Foo
              class ::Bar
                class Baz
                end
              end
            end
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context);

        assert_no_members!(context, "Foo");
        assert_owner_eq!(context, "Foo", "Object");

        assert_members_eq!(context, "Bar", ["Baz"]);
        assert_owner_eq!(context, "Bar", "Object");

        assert_no_members!(context, "Bar::Baz");
        assert_owner_eq!(context, "Bar::Baz", "Bar");
    }
}

mod constant_alias_tests {
    use super::*;

    #[test]
    fn resolving_constant_alias_to_module() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            module Foo
              CONST = 123
            end

            ALIAS = Foo
            ALIAS::CONST
            "
        });
        context.resolve();
        assert_no_diagnostics!(&context, &[Rule::ParseWarning]);

        assert_constant_alias_target_eq!(context, "ALIAS", "Foo");
        assert_constant_reference_to!(context, "Foo::CONST", "file:///foo.rb:6:8-6:13");
    }

    #[test]
    fn resolving_constant_alias_to_nested_module() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            module Foo
              module Bar
                CONST = 123
              end
            end

            ALIAS = Foo::Bar
            ALIAS::CONST
            "
        });
        context.resolve();
        assert_no_diagnostics!(&context, &[Rule::ParseWarning]);

        assert_constant_alias_target_eq!(context, "ALIAS", "Foo::Bar");
        assert_constant_reference_to!(context, "Foo::Bar::CONST", "file:///foo.rb:8:8-8:13");
    }

    #[test]
    fn resolving_constant_alias_inside_module() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            module Foo
              CONST = 123
            end

            module Bar
              MyFoo = Foo
              MyFoo::CONST
            end
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context);

        assert_constant_alias_target_eq!(context, "Bar::MyFoo", "Foo");
        assert_constant_reference_to!(context, "Foo::CONST", "file:///foo.rb:7:10-7:15");
    }

    #[test]
    fn resolving_constant_alias_in_superclass() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            class Foo
              CONST = 123
            end

            class Bar < Foo
            end

            ALIAS = Bar
            ALIAS::CONST
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context, &[Rule::ParseWarning]);

        assert_constant_reference_to!(context, "Foo::CONST", "file:///foo.rb:9:8-9:13");
    }

    #[test]
    fn resolving_chained_constant_aliases() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            module Foo
              CONST = 123
            end

            ALIAS1 = Foo
            ALIAS2 = ALIAS1
            ALIAS2::CONST
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context, &[Rule::ParseWarning]);

        assert_constant_alias_target_eq!(context, "ALIAS1", "Foo");
        assert_constant_alias_target_eq!(context, "ALIAS2", "ALIAS1");
        assert_constant_reference_to!(context, "Foo::CONST", "file:///foo.rb:7:9-7:14");
    }

    #[test]
    fn resolving_constant_alias_to_non_existent_target() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            ALIAS_1 = NonExistent
            ALIAS_2 = ALIAS_1
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context);

        assert_constant_alias_target_eq!(context, "ALIAS_2", "ALIAS_1");
        assert_no_constant_alias_target!(context, "ALIAS_1");
    }

    #[test]
    fn resolving_constant_alias_to_value_in_constant_path() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            VALUE = 1
            ALIAS = VALUE
            ALIAS::NOPE
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context, &[Rule::ParseWarning]);

        assert_constant_alias_target_eq!(context, "ALIAS", "VALUE");

        // NOPE can't be created because ALIAS points to a value constant, not a namespace
        assert_declaration_does_not_exist!(context, "VALUE::NOPE");
    }

    #[test]
    fn resolving_constant_alias_defined_before_target() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            ALIAS = Foo
            module Foo
              CONST = 1
            end
            ALIAS::CONST
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context, &[Rule::ParseWarning]);

        assert_constant_alias_target_eq!(context, "ALIAS", "Foo");
        assert_constant_reference_to!(context, "Foo::CONST", "file:///foo.rb:5:8-5:13");
    }

    #[test]
    fn resolving_constant_alias_to_value() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            class Foo
              CONST = 1
            end
            class Bar
              CONST = Foo::CONST
            end
            BAZ = Bar::CONST
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context);

        assert_constant_alias_target_eq!(context, "BAZ", "Bar::CONST");
        assert_constant_alias_target_eq!(context, "Bar::CONST", "Foo::CONST");
    }

    #[test]
    fn resolving_circular_constant_aliases() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            A = B
            B = C
            C = A
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context);

        assert_constant_alias_target_eq!(context, "A", "B");
        assert_constant_alias_target_eq!(context, "B", "C");
        assert_constant_alias_target_eq!(context, "C", "A");
    }

    #[test]
    fn resolving_circular_constant_aliases_cross_namespace() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            module A
              X = B::Y
            end
            module B
              Y = A::X
            end

            A::X::SOMETHING = 1
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context);

        assert_declaration_exists!(context, "A::X");
        assert_declaration_exists!(context, "B::Y");

        // SOMETHING can't be created because the circular alias can't resolve to a namespace
        assert_declaration_does_not_exist!(context, "A::X::SOMETHING");
    }

    #[test]
    fn resolving_constant_alias_ping_pong() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            module Left
              module Deep
                VALUE = 'left'
              end
            end

            module Right
              module Deep
                VALUE = 'right'
              end
            end

            Left::RIGHT_REF = Right
            Right::LEFT_REF = Left

            Left::RIGHT_REF::Deep::VALUE
            Left::RIGHT_REF::LEFT_REF::Deep::VALUE
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context, &[Rule::ParseWarning]);

        assert_constant_alias_target_eq!(context, "Left::RIGHT_REF", "Right");
        assert_constant_alias_target_eq!(context, "Right::LEFT_REF", "Left");

        // Left::RIGHT_REF::Deep::VALUE
        assert_constant_reference_to!(context, "Right::Deep", "file:///foo.rb:16:18-16:22");
        assert_constant_reference_to!(context, "Right::Deep::VALUE", "file:///foo.rb:16:24-16:29");
        // Left::RIGHT_REF::LEFT_REF::Deep::VALUE
        assert_constant_reference_to!(context, "Left::Deep", "file:///foo.rb:17:28-17:32");
        assert_constant_reference_to!(context, "Left::Deep::VALUE", "file:///foo.rb:17:34-17:39");
    }

    #[test]
    fn resolving_constant_alias_self_referential() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            module M
              SELF_REF = M

              class Thing
                CONST = 1
              end
            end

            M::SELF_REF::Thing::CONST
            M::SELF_REF::SELF_REF::Thing::CONST
            M::SELF_REF::SELF_REF::SELF_REF::Thing::CONST
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context, &[Rule::ParseWarning]);

        assert_constant_alias_target_eq!(context, "M::SELF_REF", "M");

        // All 3 paths resolve to M::Thing::CONST
        assert_declaration_references_count_eq!(context, "M::Thing::CONST", 3);
        assert_declaration_references_count_eq!(context, "M::Thing", 3);

        // M::SELF_REF::Thing::CONST
        assert_constant_reference_to!(context, "M::Thing", "file:///foo.rb:9:14-9:19");
        assert_constant_reference_to!(context, "M::Thing::CONST", "file:///foo.rb:9:21-9:26");
        // M::SELF_REF::SELF_REF::Thing::CONST
        assert_constant_reference_to!(context, "M::Thing", "file:///foo.rb:10:24-10:29");
        assert_constant_reference_to!(context, "M::Thing::CONST", "file:///foo.rb:10:31-10:36");
        // M::SELF_REF::SELF_REF::SELF_REF::Thing::CONST
        assert_constant_reference_to!(context, "M::Thing", "file:///foo.rb:11:34-11:39");
        assert_constant_reference_to!(context, "M::Thing::CONST", "file:///foo.rb:11:41-11:46");
    }

    #[test]
    fn resolving_constant_alias_with_multiple_definitions() {
        let mut context = GraphTest::new();
        context.index_uri("file:///a.rb", {
            r"
            module A; end
            FOO = A
            "
        });
        context.index_uri("file:///b.rb", {
            r"
            module B; end
            FOO = B
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context);

        // FOO should have 2 definitions pointing to different targets
        assert_declaration_definitions_count_eq!(context, "FOO", 2);

        assert_alias_targets_contain!(context, "FOO", "A", "B");
    }

    #[test]
    fn resolving_constant_alias_with_multiple_targets() {
        let mut context = GraphTest::new();
        context.index_uri("file:///a.rb", {
            r"
            module A
              CONST_A = 1
            end
            FOO = A
            "
        });
        context.index_uri("file:///b.rb", {
            r"
            module B
              CONST_B = 2
            end
            FOO = B
            "
        });
        context.index_uri("file:///usage.rb", {
            r"
            FOO::CONST_A
            FOO::CONST_B
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context, &[Rule::ParseWarning]);

        assert_constant_reference_to!(context, "A::CONST_A", "file:///usage.rb:1:6-1:13");
        assert_constant_reference_to!(context, "B::CONST_B", "file:///usage.rb:2:6-2:13");
    }

    #[test]
    fn resolving_constant_alias_multi_target_with_circular() {
        let mut context = GraphTest::new();
        context.index_uri("file:///a.rb", {
            r"
            module A
              CONST = 1
            end
            ALIAS = A
            "
        });
        context.index_uri("file:///b.rb", "ALIAS = ALIAS");
        context.index_uri("file:///usage.rb", "ALIAS::CONST");
        context.resolve();

        assert_no_diagnostics!(&context, &[Rule::ParseWarning]);

        // ALIAS should have two targets: A and ALIAS (self-reference)
        assert_alias_targets_contain!(context, "ALIAS", "A", "ALIAS");

        // ALIAS::CONST should still resolve to A::CONST through the valid path
        assert_constant_reference_to!(context, "A::CONST", "file:///usage.rb:1:8-1:13");
    }

    #[test]
    fn multi_target_alias_constant_added_to_primary_owner() {
        let mut context = GraphTest::new();
        context.index_uri("file:///modules.rb", {
            r"
            module Foo; end
            module Bar; end
            "
        });
        context.index_uri("file:///alias1.rb", {
            r"
            ALIAS ||= Foo
            "
        });
        context.index_uri("file:///alias2.rb", {
            r"
            ALIAS ||= Bar
            "
        });
        context.index_uri("file:///const.rb", {
            r"
            ALIAS::CONST = 123
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context);

        assert_members_eq!(context, "Foo", ["CONST"]);
        assert_no_members!(context, "Bar");
    }

    #[test]
    fn resolving_class_through_constant_alias() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            module Outer
              class Inner
              end
            end

            ALIAS = Outer
            Outer::NESTED = Outer::Inner

            class ALIAS::NESTED
              ADDED_CONST = 1
            end

            Outer::Inner::ADDED_CONST
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context, &[Rule::ParseWarning]);

        assert_constant_alias_target_eq!(context, "ALIAS", "Outer");
        assert_constant_alias_target_eq!(context, "Outer::NESTED", "Outer::Inner");

        // ADDED_CONST should be in Outer::Inner (the resolved target)
        assert_declaration_exists!(context, "Outer::Inner::ADDED_CONST");

        assert_declaration_references_count_eq!(context, "Outer::Inner::ADDED_CONST", 1);
    }

    #[test]
    fn resolving_class_definition_through_constant_alias() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            module Outer
              CONST = 1
            end

            ALIAS = Outer

            class ALIAS::NewClass
              CLASS_CONST = 2
            end

            Outer::NewClass::CLASS_CONST
            ALIAS::NewClass::CLASS_CONST
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context, &[Rule::ParseWarning]);

        assert_constant_alias_target_eq!(context, "ALIAS", "Outer");

        // NewClass should be declared under Outer, not ALIAS
        assert_declaration_exists!(context, "Outer::NewClass");
        assert_declaration_exists!(context, "Outer::NewClass::CLASS_CONST");

        // Outer::NewClass::CLASS_CONST
        assert_constant_reference_to!(context, "Outer::NewClass", "file:///foo.rb:11:8-11:16");
        assert_constant_reference_to!(context, "Outer::NewClass::CLASS_CONST", "file:///foo.rb:11:18-11:29");
        // ALIAS::NewClass::CLASS_CONST
        assert_constant_reference_to!(context, "Outer::NewClass", "file:///foo.rb:12:8-12:16");
        assert_constant_reference_to!(context, "Outer::NewClass::CLASS_CONST", "file:///foo.rb:12:18-12:29");
    }

    #[test]
    fn resolving_constant_reference_through_chained_aliases() {
        let mut context = GraphTest::new();
        context.index_uri("file:///defs.rb", {
            r"
            module Foo
              CONST = 1
            end
            ALIAS1 = Foo
            ALIAS2 = ALIAS1
            "
        });
        context.index_uri("file:///usage.rb", "ALIAS2::CONST");
        context.resolve();

        assert_no_diagnostics!(&context, &[Rule::ParseWarning]);

        assert_constant_alias_target_eq!(context, "ALIAS1", "Foo");
        assert_constant_alias_target_eq!(context, "ALIAS2", "ALIAS1");

        assert_constant_reference_to!(context, "Foo::CONST", "file:///usage.rb:1:9-1:14");
    }

    #[test]
    fn resolving_constant_reference_through_top_level_alias_target() {
        let mut context = GraphTest::new();
        context.index_uri("file:///defs.rb", {
            r"
            module Foo
              CONST = 1
            end
            ALIAS = ::Foo
            "
        });
        context.index_uri("file:///usage.rb", "ALIAS::CONST");
        context.resolve();

        assert_no_diagnostics!(&context, &[Rule::ParseWarning]);

        assert_constant_reference_to!(context, "Foo::CONST", "file:///usage.rb:1:8-1:13");
    }

    // Regression test: defining singleton method on alias triggers get_or_create_singleton_class
    #[test]
    fn resolving_singleton_method_on_alias_does_not_panic() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            class Foo; end
            ALIAS = Foo
            def ALIAS.singleton_method; end
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context);
    }

    #[test]
    fn resolving_instance_variable_on_alias_does_not_panic() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            class Foo; end
            ALIAS = Foo
            def ALIAS.singleton_method
              @ivar = 123
            end
            "
        });
        context.resolve();
        assert_no_diagnostics!(&context);
    }

    #[test]
    fn method_call_on_namespace_alias() {
        // When a method call occurs in a constant alias to a namespace, the singleton class has to be created for the
        // target namespace and not for the alias
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            class Foo
              def self.bar; end
            end

            ALIAS = Foo
            ALIAS.bar
            "
        });

        context.resolve();
        assert_no_diagnostics!(&context);
        assert_declaration_exists!(context, "Foo::<Foo>");
        assert_declaration_does_not_exist!(context, "ALIAS::<ALIAS>");
    }

    #[test]
    fn method_def_on_namespace_alias() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            class Foo
            end

            ALIAS = Foo

            def ALIAS.bar
            end
            "
        });

        context.resolve();
        assert_no_diagnostics!(&context);
        assert_declaration_exists!(context, "Foo::<Foo>");
        assert_declaration_exists!(context, "Foo::<Foo>#bar()");
        assert_declaration_does_not_exist!(context, "ALIAS::<ALIAS>");
    }

    #[test]
    fn re_opening_constant_alias_as_class() {
        let mut context = GraphTest::new();
        context.index_uri("file:///alias.rb", {
            r"
            module Foo
              class Bar; end
            end

            Baz = Foo::Bar
            "
        });
        context.index_uri("file:///reopen.rb", {
            r"
            CONST = 1

            class Baz
              class Other
                CONST
              end
            end
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context);
        assert_declaration_exists!(context, "Baz");
        assert_constant_reference_to!(context, "CONST", "file:///reopen.rb:5:5-5:10");
    }

    #[test]
    fn constant_alias_reopened_as_class_with_nested_inheritance() {
        let mut context = GraphTest::new();
        context.index_uri("file:///a.rb", {
            r"
            module Foo
              Bar = ::Object
            end

            module Foo
              class Bar
                class Baz < Something
                end
              end
            end
            "
        });
        context.resolve();

        assert_declaration_exists!(context, "Foo::Bar");
    }

    #[test]
    fn superclass_through_alias() {
        let mut context = GraphTest::new();
        context.index_uri("file:///a.rb", {
            r"
            class Base; end
            AliasedBase = Base
            class Foo < AliasedBase; end
            "
        });
        context.resolve();
        assert_no_diagnostics!(&context);
        assert_ancestors_eq!(context, "Foo", ["Foo", "Base", "Object", "Kernel", "BasicObject"]);
    }

    #[test]
    fn mixin_through_alias() {
        let mut context = GraphTest::new();
        context.index_uri("file:///a.rb", {
            r"
            module M; end
            AliasM = M
            class Foo
              include AliasM
            end
            "
        });
        context.resolve();
        assert_no_diagnostics!(&context);
        assert_ancestors_eq!(context, "Foo", ["Foo", "M", "Object", "Kernel", "BasicObject"]);
    }

    #[test]
    fn including_unresolved_alias() {
        let mut context = GraphTest::new();
        context.index_uri("file:///a.rb", {
            r"
            module Foo; end
            Foo::Bar = Bar

            module Baz
              include Foo::Bar
            end
            "
        });

        context.resolve();
        assert_ancestors_eq!(context, "Baz", ["Baz"]);
    }

    #[test]
    fn prepending_unresolved_alias() {
        let mut context = GraphTest::new();
        context.index_uri("file:///a.rb", {
            r"
            module Foo; end
            Foo::Bar = Bar

            module Baz
              prepend Foo::Bar
            end
            "
        });

        context.resolve();
        assert_ancestors_eq!(context, "Baz", ["Baz"]);
    }

    #[test]
    fn inheriting_unresolved_alias() {
        let mut context = GraphTest::new();
        context.index_uri("file:///a.rb", {
            r"
            module Foo; end
            Foo::Bar = Bar

            class Baz < Foo::Bar
            end
            "
        });

        context.resolve();
        assert_ancestors_eq!(context, "Baz", ["Baz", "Object", "Kernel", "BasicObject"]);
    }

    #[test]
    fn re_opening_unresolved_alias() {
        let mut context = GraphTest::new();
        context.index_uri("file:///a.rb", {
            r"
            module Foo; end
            Foo::Bar = Bar

            module Foo::Bar
              CONST = 123
              @class_ivar = 123
              @@class_var = 789

              attr_reader :some_attr

              def self.class_method; end

              def initialize
                @instance_ivar = 456
              end
            end
            "
        });

        context.resolve();
        assert_declaration_does_not_exist!(context, "Foo::Bar::CONST");
        assert_declaration_does_not_exist!(context, "Foo::Bar::<Bar>#@class_ivar");
        assert_declaration_does_not_exist!(context, "Foo::Bar#@instance_ivar");
        assert_declaration_does_not_exist!(context, "Foo::Bar#@@class_var");
        assert_declaration_does_not_exist!(context, "Foo::Bar#some_attr()");
        assert_declaration_does_not_exist!(context, "Foo::Bar::<Bar>#class_method()");
        assert_declaration_does_not_exist!(context, "Foo::Bar#initialize()");
    }

    #[test]
    fn re_opening_namespace_alias() {
        let mut context = GraphTest::new();
        context.index_uri("file:///a.rb", {
            r"
            module Foo; end
            ALIAS = Foo

            module ALIAS
              CONST = 123
              @class_ivar = 123
              @@class_var = 789

              attr_reader :some_attr

              def self.class_method; end

              def initialize
                @instance_ivar = 456
              end

              def bar; end
              alias new_bar bar
            end
            "
        });

        context.resolve();
        assert_declaration_exists!(context, "Foo::CONST");
        assert_declaration_exists!(context, "Foo::<Foo>#@class_ivar");
        assert_declaration_exists!(context, "Foo#@instance_ivar");
        assert_declaration_exists!(context, "Foo#@@class_var");
        assert_declaration_exists!(context, "Foo#some_attr()");
        assert_declaration_exists!(context, "Foo::<Foo>#class_method()");
        assert_declaration_exists!(context, "Foo#initialize()");
        assert_declaration_exists!(context, "Foo#new_bar()");
    }
}

mod superclass_tests {
    use super::*;

    #[test]
    fn linearizing_super_classes() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            class Foo; end
            class Bar < Foo; end
            class Baz < Bar; end
            class Qux < Baz; end
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context);

        assert_ancestors_eq!(
            context,
            "Qux",
            ["Qux", "Baz", "Bar", "Foo", "Object", "Kernel", "BasicObject"]
        );
    }

    #[test]
    fn descendants_are_tracked_for_parent_classes() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            class Foo
              CONST = 123
            end

            class Bar < Foo; end

            class Baz < Bar
              CONST
            end

            class Qux < Bar
              CONST
            end
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context);

        assert_descendants!(context, "Foo", ["Bar"]);
        assert_descendants!(context, "Bar", ["Baz", "Qux"]);
    }

    #[test]
    fn linearizing_circular_super_classes() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            class Foo < Bar; end
            class Bar < Baz; end
            class Baz < Foo; end
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context);

        assert_ancestors_eq!(context, "Foo", ["Foo", "Bar", "Baz", "Object"]);
    }

    #[test]
    fn resolving_a_constant_inherited_from_the_super_class() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            class Foo
              CONST = 123
            end

            class Bar < Foo
              CONST
            end
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context);

        assert_constant_reference_to!(context, "Foo::CONST", "file:///foo.rb:6:3-6:8");
    }

    #[test]
    fn does_not_loop_forever_on_non_existing_parents() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            class Bar < Foo
              CONST
            end
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context);

        let declaration = context.graph().declarations().get(&DeclarationId::from("Bar")).unwrap();
        assert!(matches!(
            declaration.as_namespace().unwrap().clone_ancestors(),
            Ancestors::Partial(_)
        ));
    }

    #[test]
    fn resolving_inherited_constant_dependent_on_complex_parent() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            module Foo
              module Bar
                class Baz
                  CONST = 123
                end
              end
            end
            class Qux < Foo::Bar::Baz
              CONST
            end
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context);

        assert_constant_reference_to!(context, "Foo::Bar::Baz::CONST", "file:///foo.rb:9:3-9:8");
    }

    #[test]
    fn linearizing_parent_classes_with_parent_scope() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            module Foo
              class Bar
              end
            end
            class Baz < Foo::Bar
            end
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context);

        assert_ancestors_eq!(context, "Baz", ["Baz", "Foo::Bar", "Object", "Kernel", "BasicObject"]);
    }

    #[test]
    fn references_with_parent_scope_search_inheritance() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            module Foo
              module Bar; end
            end

            class Baz
              include Foo
            end

            Baz::Bar
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context, &[Rule::ParseWarning]);

        assert_constant_reference_to!(context, "Foo::Bar", "file:///foo.rb:9:6-9:9");
    }

    #[test]
    fn ancestors_for_unresolved_parent_class() {
        let mut context = GraphTest::new();
        context.index_uri(
            "file:///foo.rb",
            "
            class Foo < Bar; end
            ",
        );
        context.resolve();

        assert_ancestors_eq!(
            context,
            "Foo",
            ["Foo", Partial("Bar"), "Object", "Kernel", "BasicObject"]
        );
        assert!(matches!(
            context
                .graph()
                .declarations()
                .get(&DeclarationId::from("Foo"))
                .unwrap()
                .as_namespace()
                .unwrap()
                .ancestors(),
            Ancestors::Partial(_)
        ));
    }
}

mod include_tests {
    use super::*;

    #[test]
    fn resolving_constant_references_involved_in_includes() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            module Foo; end
            module Bar
              include Foo
            end
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context);

        assert_ancestors_eq!(context, "Bar", ["Bar", "Foo"]);
    }

    #[test]
    fn resolving_include_using_inherited_constant() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            module Foo
              module Bar; end
            end
            class Baz
              include Foo
              include Bar
            end
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context);

        assert_ancestors_eq!(
            context,
            "Baz",
            ["Baz", "Foo::Bar", "Foo", "Object", "Kernel", "BasicObject"]
        );
    }

    #[test]
    fn linearizing_included_modules() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            module Foo; end
            module Bar
              prepend Foo
            end
            class Baz
              prepend Bar
            end
            class Qux < Baz; end
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context);

        assert_ancestors_eq!(context, "Foo", ["Foo"]);
        assert_ancestors_eq!(context, "Bar", ["Foo", "Bar"]);
        assert_ancestors_eq!(
            context,
            "Qux",
            ["Qux", "Foo", "Bar", "Baz", "Object", "Kernel", "BasicObject"]
        );
    }

    #[test]
    fn include_on_dynamic_namespace_definitions() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            module B; end
            A = Struct.new do
              include B
            end

            C = Class.new do
              include B
            end

            D = Module.new do
              include B
            end
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context);

        assert_ancestors_eq!(context, "B", ["B"]);
        // TODO: this is a temporary hack to avoid crashing on `Struct.new`, `Class.new` and `Module.new`
        //assert_ancestors_eq!(context, "A", Vec::<&str>::new());
        assert_ancestors_eq!(context, "C", ["C", "B", "Object", "Kernel", "BasicObject"]);
        assert_ancestors_eq!(context, "D", ["D", "B"]);
    }

    #[test]
    fn cyclic_include() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            module Foo
              include Foo
            end
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context);

        assert_ancestors_eq!(context, "Foo", ["Foo"]);
    }

    #[test]
    fn duplicate_includes() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            module Foo
            end

            module Bar
              include Foo
              include Foo
            end
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context);

        assert_ancestors_eq!(context, "Bar", ["Bar", "Foo"]);
    }

    #[test]
    fn indirect_duplicate_includes() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            module A; end

            module B
              include A
            end

            module C
              include A
            end

            module Foo
              include B
              include C
            end
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context);

        assert_ancestors_eq!(context, "A", ["A"]);
        assert_ancestors_eq!(context, "B", ["B", "A"]);
        assert_ancestors_eq!(context, "C", ["C", "A"]);
        assert_ancestors_eq!(context, "Foo", ["Foo", "C", "B", "A"]);
    }

    #[test]
    fn includes_involving_parent_scopes() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            module A
              module B
                module C; end
              end
            end

            module D
              include A::B::C
            end

            module Foo
              include D
              include A::B::C
            end

            module Bar
              include A::B::C
              include D
            end
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context);

        assert_ancestors_eq!(context, "Foo", ["Foo", "D", "A::B::C"]);
        assert_ancestors_eq!(context, "Bar", ["Bar", "D", "A::B::C"]);
    }

    #[test]
    fn duplicate_includes_in_parents() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            module A; end

            module B
              include A
            end

            class Parent
              include B
            end

            class Child < Parent
              include B
            end
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context);

        assert_ancestors_eq!(
            context,
            "Child",
            ["Child", "Parent", "B", "A", "Object", "Kernel", "BasicObject"]
        );
    }

    #[test]
    fn included_modules_involved_in_definitions() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            module Foo
              module Bar; end
            end

            module Baz
              include Foo

              class Bar::Qux
              end
            end
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context);

        assert_members_eq!(context, "Foo::Bar", ["Qux"]);
        assert_owner_eq!(context, "Foo::Bar", "Foo");

        assert_no_members!(context, "Foo::Bar::Qux");
        assert_owner_eq!(context, "Foo::Bar::Qux", "Foo::Bar");
    }

    #[test]
    fn multiple_mixins_in_same_include() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            module A; end
            module B; end

            class Foo
              include A, B
            end
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context);

        assert_ancestors_eq!(context, "Foo", ["Foo", "A", "B", "Object", "Kernel", "BasicObject"]);
    }

    #[test]
    fn descendants_are_tracked_for_includes() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            module Foo; end
            module Bar
              include Foo
            end
            module Baz
              include Bar
            end
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context);

        assert_descendants!(context, "Bar", ["Baz"]);
        assert_descendants!(context, "Foo", ["Bar", "Baz"]);
    }
}

mod prepend_tests {
    use super::*;

    #[test]
    fn resolving_constant_references_involved_in_prepends() {
        let mut context = GraphTest::new();

        // To linearize the ancestors of `Bar`, we need to resolve `Foo` first. However, during that resolution, we need
        // to check `Bar`'s ancestor chain before checking the top level (which is where we'll find `Foo`). In these
        // scenarios, we need to realize the dependency and skip ancestors
        context.index_uri("file:///foo.rb", {
            r"
            module Foo; end
            module Bar
              prepend Foo
            end
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context);

        assert_ancestors_eq!(context, "Bar", ["Foo", "Bar"]);
    }

    #[test]
    fn resolving_prepend_using_inherited_constant() {
        let mut context = GraphTest::new();
        // Prepending `Foo` makes `Bar` available, which we can then prepend as well. This requires resolving constants
        // with partially linearized ancestors
        context.index_uri("file:///foo.rb", {
            r"
            module Foo
              module Bar; end
            end
            class Baz
              prepend Foo
              prepend Bar
            end
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context);

        assert_ancestors_eq!(
            context,
            "Baz",
            ["Foo::Bar", "Foo", "Baz", "Object", "Kernel", "BasicObject"]
        );
    }

    #[test]
    fn linearizing_prepended_modules() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            module Foo; end
            module Bar
              prepend Foo
            end
            class Baz
              prepend Bar
            end
            class Qux < Baz; end
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context);

        assert_ancestors_eq!(context, "Foo", ["Foo"]);
        assert_ancestors_eq!(context, "Bar", ["Foo", "Bar"]);
        assert_ancestors_eq!(
            context,
            "Qux",
            ["Qux", "Foo", "Bar", "Baz", "Object", "Kernel", "BasicObject"]
        );
    }

    #[test]
    fn prepend_on_dynamic_namespace_definitions() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            module B; end
            A = Struct.new do
              prepend B
            end

            C = Class.new do
              prepend B
            end

            D = Module.new do
              prepend B
            end
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context);

        assert_ancestors_eq!(context, "B", ["B"]);
        // TODO: this is a temporary hack to avoid crashing on `Struct.new`, `Class.new` and `Module.new`
        //assert_ancestors_eq!(context, "A", Vec::<&str>::new());
        assert_ancestors_eq!(context, "C", ["B", "C", "Object", "Kernel", "BasicObject"]);
        assert_ancestors_eq!(context, "D", ["B", "D"]);
    }

    #[test]
    fn prepends_track_descendants() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            module Foo; end
            module Bar
              prepend Foo
            end
            class Baz
              prepend Bar
            end
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context);

        assert_descendants!(context, "Foo", ["Bar", "Baz"]);
        assert_descendants!(context, "Bar", ["Baz"]);
    }

    #[test]
    fn cyclic_prepend() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            module Foo
              prepend Foo
            end
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context);

        assert_ancestors_eq!(context, "Foo", ["Foo"]);
    }

    #[test]
    fn duplicate_prepends() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            module Foo
            end

            module Bar
              prepend Foo
              prepend Foo
            end
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context);

        assert_ancestors_eq!(context, "Bar", ["Foo", "Bar"]);
    }

    #[test]
    fn indirect_duplicate_prepends() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            module A; end

            module B
              prepend A
            end

            module C
              prepend A
            end

            module Foo
              prepend B
              prepend C
            end
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context);

        assert_ancestors_eq!(context, "A", ["A"]);
        assert_ancestors_eq!(context, "B", ["A", "B"]);
        assert_ancestors_eq!(context, "C", ["A", "C"]);
        assert_ancestors_eq!(context, "Foo", ["A", "C", "B", "Foo"]);
    }

    #[test]
    fn multiple_mixins_in_same_prepend() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            module A; end
            module B; end

            class Foo
              prepend A, B
            end
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context);

        assert_ancestors_eq!(context, "Foo", ["A", "B", "Foo", "Object", "Kernel", "BasicObject"]);
    }

    #[test]
    fn prepends_involving_parent_scopes() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            module A
              module B
                module C; end
              end
            end

            module D
              prepend A::B::C
            end

            module Foo
              prepend D
              prepend A::B::C
            end

            module Bar
              prepend A::B::C
              prepend D
            end
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context);

        assert_ancestors_eq!(context, "Foo", ["A::B::C", "D", "Foo"]);
        assert_ancestors_eq!(context, "Bar", ["A::B::C", "D", "Bar"]);
    }

    #[test]
    fn duplicate_prepends_in_parents() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            module A; end

            module B
              prepend A
            end

            class Parent
              prepend B
            end

            class Child < Parent
              prepend B
            end
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context);

        assert_ancestors_eq!(
            context,
            "Child",
            ["A", "B", "Child", "A", "B", "Parent", "Object", "Kernel", "BasicObject"]
        );
    }

    #[test]
    fn prepended_modules_involved_in_definitions() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            module Foo
              module Bar; end
            end

            module Baz
              prepend Foo

              class Bar::Qux
              end
            end
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context);

        assert_members_eq!(context, "Foo::Bar", ["Qux"]);
        assert_owner_eq!(context, "Foo::Bar", "Foo");

        assert_no_members!(context, "Foo::Bar::Qux");
        assert_owner_eq!(context, "Foo::Bar::Qux", "Foo::Bar");
    }
}

mod mixin_dedup_tests {
    use super::*;

    #[test]
    fn duplicate_includes_and_prepends() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            module A; end

            class Foo
              prepend A
              include A
            end

            class Bar
              include A
              prepend A
            end
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context);

        assert_ancestors_eq!(context, "Foo", ["A", "Foo", "Object", "Kernel", "BasicObject"]);
        assert_ancestors_eq!(context, "Bar", ["A", "Bar", "A", "Object", "Kernel", "BasicObject"]);
    }

    #[test]
    fn duplicate_indirect_includes_and_prepends() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            module A; end
            module B
              include A
            end
            module C
              prepend A
            end

            class Foo
              include C
              prepend B
              include A
            end

            class Bar
              include A
              prepend B
              include C
            end

            class Baz
              prepend B
              include C
              prepend A
            end

            class Qux
              prepend A
              include C
              prepend B
            end
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context);

        assert_ancestors_eq!(
            context,
            "Foo",
            ["B", "A", "Foo", "A", "C", "Object", "Kernel", "BasicObject"]
        );
        assert_ancestors_eq!(
            context,
            "Bar",
            ["B", "A", "Bar", "C", "A", "Object", "Kernel", "BasicObject"]
        );
        assert_ancestors_eq!(
            context,
            "Baz",
            ["B", "A", "Baz", "C", "Object", "Kernel", "BasicObject"]
        );
        assert_ancestors_eq!(
            context,
            "Qux",
            ["B", "A", "Qux", "C", "Object", "Kernel", "BasicObject"]
        );
    }

    #[test]
    fn duplicate_includes_and_prepends_through_parents() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            module A; end

            class Parent
              include A
            end

            class Foo < Parent
              prepend A
            end

            class Bar < Parent
              include A
            end
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context);

        assert_ancestors_eq!(
            context,
            "Foo",
            ["A", "Foo", "Parent", "A", "Object", "Kernel", "BasicObject"]
        );
        assert_ancestors_eq!(
            context,
            "Bar",
            ["Bar", "Parent", "A", "Object", "Kernel", "BasicObject"]
        );
    }
}

mod object_ancestors_tests {
    use super::*;

    #[test]
    fn ancestors_with_missing_core() {
        let mut context = GraphTest::new();
        context.index_uri(
            "file:///foo.rb",
            "
            module Bar; end

            class Foo
              include Bar
            end
            ",
        );
        context.resolve();

        assert_ancestors_eq!(context, "Foo", ["Foo", "Bar", "Object", "Kernel", "BasicObject"]);
        assert_descendants!(context, "Bar", ["Foo"]);
    }

    #[test]
    fn ancestor_patches_to_object_are_correctly_processed() {
        let mut context = GraphTest::new();
        context.index_uri(
            "file:///foo.rb",
            "
            module Foo; end

            module Kernel
              include Foo
            end
            ",
        );
        context.resolve();

        assert_ancestors_eq!(context, "Object", ["Object", "Kernel", "Foo", "BasicObject"]);
    }

    #[test]
    fn basic_object_ancestors() {
        let mut context = GraphTest::new();
        context.index_uri(
            "file:///foo.rb",
            "
            class Foo < BasicObject
            end
            ",
        );
        context.resolve();

        assert_ancestors_eq!(context, "Foo", ["Foo", "BasicObject"]);
    }

    #[test]
    fn basic_object_ancestors_including_kernel() {
        let mut context = GraphTest::new();
        context.index_uri(
            "file:///foo.rb",
            "
            class Foo < BasicObject
              include Kernel
            end
            ",
        );
        context.resolve();

        assert_ancestors_eq!(context, "Foo", ["Foo", "Kernel", "BasicObject"]);
    }

    #[test]
    fn constant_resolution_inside_basic_object() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            class String; end

            class Foo < BasicObject
              String
            end
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context);
        assert_constant_reference_unresolved!(context, "String");
    }

    #[test]
    fn top_level_scope_searches_object_ancestors() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            module Kernel
              FOUND_ME = true
            end

            class Object
              include Kernel
            end

            class Foo
              ::FOUND_ME
            end
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context);
        assert_constant_reference_to!(context, "Kernel::FOUND_ME", "file:///foo.rb:10:5-10:13");
    }

    #[test]
    fn top_level_script_constant_resolution_searches_object_ancestors() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            module Kernel
              FOUND_ME = true
            end

            class Object
              include Kernel
            end

            FOUND_ME
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context, &[Rule::ParseWarning]);
        assert_constant_reference_to!(context, "Kernel::FOUND_ME", "file:///foo.rb:9:1-9:9");
    }

    #[test]
    fn module_own_ancestors_take_priority_over_object_fallback() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            module MyConstants
              CONST = 'mine'
            end

            module Kernel
              CONST = 'kernel'
            end

            class Object
              include Kernel
            end

            module Foo
              include MyConstants
              CONST
            end
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context);
        assert_constant_reference_to!(context, "MyConstants::CONST", "file:///foo.rb:15:3-15:8");
    }

    #[test]
    fn object_inherited_constant_inside_module() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            module Kernel
              FOUND_ME = true
            end

            class Object
              include Kernel
            end

            module Foo
              # This is valid because of Object inheritance
              FOUND_ME
            end

            Foo::FOUND_ME # this is not
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context, &[Rule::ParseWarning]);
        assert_constant_reference_to!(context, "Kernel::FOUND_ME", "file:///foo.rb:11:3-11:11");
        assert_constant_reference_unresolved!(context, "FOUND_ME", "file:///foo.rb:14:6-14:14");
    }
}

mod singleton_ancestors_tests {
    use super::*;

    #[test]
    fn singleton_ancestors_for_classes() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            module Foo; end
            module Qux; end
            module Zip; end
            class Bar; end

            class Baz < Bar
              extend Foo

              class << self
                include Qux

                class << self
                  include Zip
                end
              end
            end
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context);

        assert_ancestors_eq!(
            context,
            "Baz::<Baz>",
            [
                "Baz::<Baz>",
                "Qux",
                "Foo",
                "Bar::<Bar>",
                "Object::<Object>",
                "BasicObject::<BasicObject>",
                "Class",
                "Module",
                "Object",
                "Kernel",
                "BasicObject"
            ]
        );

        assert_ancestors_eq!(
            context,
            "Baz::<Baz>::<<Baz>>",
            [
                "Baz::<Baz>::<<Baz>>",
                "Zip",
                "Bar::<Bar>::<<Bar>>",
                "Object::<Object>::<<Object>>",
                "BasicObject::<BasicObject>::<<BasicObject>>",
                "Class::<Class>",
                "Module::<Module>",
                "Object::<Object>",
                "BasicObject::<BasicObject>",
                "Class",
                "Module",
                "Object",
                "Kernel",
                "BasicObject"
            ]
        );
    }

    #[test]
    fn singleton_ancestors_for_modules() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            module Foo; end
            module Qux; end
            module Zip; end
            class Bar; end

            module Baz
              extend Foo

              class << self
                include Qux

                class << self
                  include Zip
                end
              end
            end
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context);

        assert_ancestors_eq!(
            context,
            "Baz::<Baz>",
            ["Baz::<Baz>", "Qux", "Foo", "Module", "Object", "Kernel", "BasicObject"]
        );
        assert_ancestors_eq!(
            context,
            "Baz::<Baz>::<<Baz>>",
            [
                "Baz::<Baz>::<<Baz>>",
                "Zip",
                "Module::<Module>",
                "Object::<Object>",
                "BasicObject::<BasicObject>",
                "Class",
                "Module",
                "Object",
                "Kernel",
                "BasicObject"
            ]
        );
    }

    #[test]
    fn singleton_ancestors_with_inherited_parent_modules() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            module Foo; end
            module Qux; end
            class Bar
              class << self
                include Foo
                prepend Qux
              end
            end

            class Baz < Bar
              class << self
                class << self
                end
              end
            end
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context);

        assert_ancestors_eq!(
            context,
            "Bar::<Bar>",
            [
                "Qux",
                "Bar::<Bar>",
                "Foo",
                "Object::<Object>",
                "BasicObject::<BasicObject>",
                "Class",
                "Module",
                "Object",
                "Kernel",
                "BasicObject"
            ]
        );

        assert_ancestors_eq!(
            context,
            "Baz::<Baz>",
            [
                "Baz::<Baz>",
                "Qux",
                "Bar::<Bar>",
                "Foo",
                "Object::<Object>",
                "BasicObject::<BasicObject>",
                "Class",
                "Module",
                "Object",
                "Kernel",
                "BasicObject"
            ]
        );
        assert_ancestors_eq!(
            context,
            "Baz::<Baz>::<<Baz>>",
            [
                "Baz::<Baz>::<<Baz>>",
                "Bar::<Bar>::<<Bar>>",
                "Object::<Object>::<<Object>>",
                "BasicObject::<BasicObject>::<<BasicObject>>",
                "Class::<Class>",
                "Module::<Module>",
                "Object::<Object>",
                "BasicObject::<BasicObject>",
                "Class",
                "Module",
                "Object",
                "Kernel",
                "BasicObject"
            ]
        );
    }

    #[test]
    fn singleton_ancestor_chain_cascades_through_intermediate_class() {
        let mut context = GraphTest::new();
        context.index_uri(
            "file:///foo.rb",
            r"
            class Foo
              def self.foo; end
            end
            class Bar < Foo
            end
            class Baz < Bar
              def self.baz; end
            end
            ",
        );
        context.resolve();

        assert_ancestors_eq!(
            context,
            "Baz::<Baz>",
            [
                "Baz::<Baz>",
                "Bar::<Bar>",
                "Foo::<Foo>",
                "Object::<Object>",
                "BasicObject::<BasicObject>",
                "Class",
                "Module",
                "Object",
                "Kernel",
                "BasicObject"
            ]
        );
    }

    #[test]
    fn extend_creates_singleton_class() {
        let mut context = GraphTest::new();
        context.index_uri(
            "file:///foo.rb",
            "
            module Bar; end

            class Foo
              extend Bar
            end
            ",
        );
        context.resolve();

        assert_declaration_exists!(context, "Foo::<Foo>");
        assert_ancestors_eq!(
            context,
            "Foo::<Foo>",
            [
                "Foo::<Foo>",
                "Bar",
                "Object::<Object>",
                "BasicObject::<BasicObject>",
                "Class",
                "Module",
                "Object",
                "Kernel",
                "BasicObject"
            ]
        );
    }

    #[test]
    fn extend_creates_singleton_class_with_existing_singleton_method() {
        let mut context = GraphTest::new();
        context.index_uri(
            "file:///foo.rb",
            "
            module Bar; end

            class Foo
              extend Bar

              def self.baz; end
            end
            ",
        );
        context.resolve();

        assert_declaration_exists!(context, "Foo::<Foo>");
        assert_ancestors_eq!(
            context,
            "Foo::<Foo>",
            [
                "Foo::<Foo>",
                "Bar",
                "Object::<Object>",
                "BasicObject::<BasicObject>",
                "Class",
                "Module",
                "Object",
                "Kernel",
                "BasicObject"
            ]
        );
    }

    #[test]
    fn extend_creates_singleton_class_on_module() {
        let mut context = GraphTest::new();
        context.index_uri(
            "file:///foo.rb",
            "
            module Bar; end

            module Foo
              extend Bar
            end
            ",
        );
        context.resolve();

        assert_declaration_exists!(context, "Foo::<Foo>");
        assert_ancestors_eq!(
            context,
            "Foo::<Foo>",
            ["Foo::<Foo>", "Bar", "Module", "Object", "Kernel", "BasicObject"]
        );
    }
}

mod method_tests {
    use super::*;

    #[test]
    fn resolution_for_method_with_receiver() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            class Foo
              def self.bar; end

              class << self
                def self.nested_bar; end
              end
            end

            class Bar
              def Foo.baz; end

              def self.qux; end
            end
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context);

        assert_members_eq!(context, "Foo::<Foo>", ["bar()", "baz()"]);
        assert_owner_eq!(context, "Foo::<Foo>", "Foo");

        assert_members_eq!(context, "Foo::<Foo>::<<Foo>>", ["nested_bar()"]);
        assert_owner_eq!(context, "Foo::<Foo>::<<Foo>>", "Foo::<Foo>");

        assert_members_eq!(context, "Bar::<Bar>", ["qux()"]);
        assert_owner_eq!(context, "Bar::<Bar>", "Bar");
    }

    #[test]
    fn resolution_for_self_method_with_same_name_instance_method() {
        let mut context = GraphTest::new();
        context.index_uri(
            "file:///foo.rb",
            r"
            class Foo
              def self.run; end
              def run; end
            end
            ",
        );
        context.resolve();

        assert_no_diagnostics!(&context);

        assert_members_eq!(context, "Foo", ["run()"]);
        assert_members_eq!(context, "Foo::<Foo>", ["run()"]);
    }

    #[test]
    fn resolution_for_self_method_alias_with_same_name_instance_method() {
        let mut context = GraphTest::new();
        context.index_rbs_uri(
            "file:///foo.rbs",
            r"
            class Foo
              def self.run: () -> void
              def run: () -> void
              alias self.execute self.run
              alias execute run
            end
            ",
        );
        context.resolve();

        assert_no_diagnostics!(&context);

        assert_members_eq!(context, "Foo::<Foo>", ["execute()", "run()"]);
        assert_members_eq!(context, "Foo", ["execute()", "run()"]);
    }

    #[test]
    fn resolving_method_defined_inside_method() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            class Foo
              def setup
                def inner_method; end
              end
            end
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context);

        // inner_method should be owned by Foo, not by setup
        assert_members_eq!(context, "Foo", ["inner_method()", "setup()"]);
    }

    #[test]
    fn resolving_attr_accessors_inside_method() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            class Foo
              def self.setup
                attr_reader :reader_attr
                attr_writer :writer_attr
                attr_accessor :accessor_attr
              end
            end
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context);

        assert_members_eq!(context, "Foo::<Foo>", ["setup()"]);

        // All attr_* should be owned by Foo, not by setup
        assert_members_eq!(context, "Foo", ["accessor_attr()", "reader_attr()", "writer_attr()"]);
    }
}

mod method_alias_tests {
    use super::*;

    #[test]
    fn resolving_method_alias() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            class Foo
              def foo; end

              alias bar foo
            end
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context);

        assert_members_eq!(context, "Foo", ["bar()", "foo()"]);
    }

    #[test]
    fn resolving_method_alias_with_self_receiver() {
        // SelfReceiver resolves to instance methods (the class directly), not the singleton
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            class Foo
              def original; end
              self.alias_method :aliased, :original
            end
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context);

        assert_members_eq!(context, "Foo", ["aliased()", "original()"]);
    }

    #[test]
    fn resolving_alias_method_in_singleton_class_lands_on_singleton() {
        // `class << self; alias_method ...; end` — alias lands on singleton via lexical nesting
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            class Foo
              def self.find; end

              class << self
                alias_method :find_old, :find
              end
            end
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context);

        assert_members_eq!(context, "Foo::<Foo>", ["find()", "find_old()"]);
    }

    #[test]
    fn resolving_self_alias_method_is_equivalent_to_bare_alias_method() {
        // `self.alias_method` and bare `alias_method` resolve identically (instance methods)
        let mut context = GraphTest::new();
        context.index_uri("file:///with_self.rb", {
            r"
            class WithSelf
              def original; end
              self.alias_method :aliased, :original
            end
            "
        });
        context.index_uri("file:///without_self.rb", {
            r"
            class WithoutSelf
              def original; end
              alias_method :aliased, :original
            end
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context);

        // Both resolve identically: alias lands on instance methods
        assert_members_eq!(context, "WithSelf", ["aliased()", "original()"]);
        assert_members_eq!(context, "WithoutSelf", ["aliased()", "original()"]);
    }

    #[test]
    fn resolving_method_alias_with_constant_receiver() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            class Bar
              def to_s; end
            end

            class Foo
              Bar.alias_method(:new_to_s, :to_s)
            end
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context);

        // Bar.alias_method places the alias on Bar's instance methods
        assert_no_members!(context, "Foo");
        assert_members_eq!(context, "Bar", ["new_to_s()", "to_s()"]);
    }

    #[test]
    fn resolving_global_variable_alias() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            $foo = 123
            alias $bar $foo
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context);

        assert_members_eq!(
            context,
            "Object",
            ["$bar", "$foo", "BasicObject", "Class", "Kernel", "Module", "Object"]
        );
    }

    #[test]
    fn resolving_global_variable_alias_inside_method() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            class Foo
              def setup
                alias $bar $baz
              end
            end
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context);

        // Global variable aliases should still be owned by Object, regardless of where defined
        assert_members_eq!(
            context,
            "Object",
            ["$bar", "BasicObject", "Class", "Foo", "Kernel", "Module", "Object"]
        );
    }
}

mod variable_tests {
    use super::*;

    #[test]
    fn resolution_for_class_variable_in_nested_singleton_class() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            class Foo
              class << self
                @@bar = 123

                class << self
                  @@baz = 456
                end
              end
            end
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context);

        assert_members_eq!(context, "Foo", ["@@bar", "@@baz"]);
        assert_owner_eq!(context, "Foo", "Object");
    }

    #[test]
    fn resolution_for_class_variable_in_method() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            class Foo
              def bar
                @@baz = 456
              end
            end
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context);

        assert_members_eq!(context, "Foo", ["@@baz", "bar()"]);
    }

    #[test]
    fn resolution_for_class_variable_only_follows_lexical_nesting() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            class Foo; end
            class Bar
              def Foo.demo
                @@cvar1 = 1
              end

              class << Foo
                def demo2
                  @@cvar2 = 1
                end
              end
            end
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context);

        assert_no_members!(context, "Foo");
        assert_members_eq!(context, "Bar", ["@@cvar1", "@@cvar2"]);
    }

    #[test]
    fn resolution_for_class_variable_at_top_level() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            @@var = 123
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context);

        // TODO: this should push an error diagnostic
        assert_declaration_does_not_exist!(context, "Object::@@var");
    }

    #[test]
    fn resolution_for_instance_and_class_instance_variables() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            class Foo
              @foo = 0

              def initialize
                @bar = 1
              end

              def self.baz
                @baz = 2
              end

              class << self
                def qux
                  @qux = 3
                end

                def self.nested
                  @nested = 4
                end
              end
            end
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context);

        assert_instance_variables_eq!(context, "Foo", ["@bar"]);
        // @qux in `class << self; def qux` - self is Foo when called, so @qux belongs to Foo's singleton class
        assert_instance_variables_eq!(context, "Foo::<Foo>", ["@baz", "@foo", "@qux"]);
        assert_instance_variables_eq!(context, "Foo::<Foo>::<<Foo>>", ["@nested"]);
    }

    #[test]
    fn resolution_for_instance_variables_with_dynamic_method_owner() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            class Foo
            end

            class Bar
              def Foo.bar
                @foo = 0
              end

              class << Foo
                def Bar.baz
                  @baz = 1
                end
              end
            end
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context);

        assert_instance_variables_eq!(context, "Foo::<Foo>", ["@foo"]);
        assert_instance_variables_eq!(context, "Bar::<Bar>", ["@baz"]);
    }

    #[test]
    fn resolution_for_class_instance_variable_in_compact_namespace() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            class Bar; end

            class Foo
              class Bar::Baz
                @baz = 1
              end
            end
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context);

        // The class is `Bar::Baz`, so its singleton class is `Bar::Baz::<Baz>`
        assert_instance_variables_eq!(context, "Bar::Baz::<Baz>", ["@baz"]);
    }

    #[test]
    fn resolution_for_instance_variable_in_singleton_class_body() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            class Foo
              class << self
                @bar = 1

                class << self
                  @baz = 2
                end
              end
            end
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context);

        assert_instance_variables_eq!(context, "Foo::<Foo>::<<Foo>>", ["@bar"]);
        assert_instance_variables_eq!(context, "Foo::<Foo>::<<Foo>>::<<<Foo>>>", ["@baz"]);
    }

    #[test]
    fn resolution_for_instance_variable_in_constant_receiver_method() {
        let mut context = GraphTest::new();
        context.index_uri(
            "file:///foo.rb",
            r"
            class Foo; end

            def Foo.bar
              @bar = 1
            end
            ",
        );
        context.resolve();

        assert_no_diagnostics!(&context);

        assert_declaration_exists!(context, "Foo::<Foo>#bar()");
        assert_instance_variables_eq!(context, "Foo::<Foo>", ["@bar"]);
    }

    #[test]
    fn resolution_for_top_level_instance_variable() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            @foo = 0
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context);

        // Top-level instance variables belong to `<main>`, not `Object`.
        // We can't represent `<main>` yet, so no declaration is created.
        assert_declaration_does_not_exist!(context, "Object::@foo");
    }

    #[test]
    fn resolution_for_instance_variable_with_unresolved_receiver() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            class Foo
              def foo.bar
                @baz = 0
              end
            end
            "
        });
        context.resolve();

        assert_diagnostics_eq!(
            &context,
            ["dynamic-singleton-definition: Dynamic receiver for singleton method definition (2:3-4:6)",]
        );

        // Instance variable in method with unresolved receiver should not create a declaration
        assert_declaration_does_not_exist!(context, "Object::@baz");
        assert_declaration_does_not_exist!(context, "Foo::@baz");
    }
}

mod declaration_creation_tests {
    use super::*;

    #[test]
    fn resolution_creates_global_declaration() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            module Foo
              class Bar
              end
            end

            class Foo::Baz
            end
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context);

        assert_members_eq!(context, "Foo", ["Bar", "Baz"]);
        assert_owner_eq!(context, "Foo", "Object");

        assert_no_members!(context, "Foo::Bar");
        assert_owner_eq!(context, "Foo::Bar", "Foo");

        assert_no_members!(context, "Foo::Baz");
        assert_owner_eq!(context, "Foo::Baz", "Foo");
    }

    #[test]
    fn resolution_for_non_constant_declarations() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            class Foo
              def initialize
                @name = 123
              end
            end
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context);

        assert_members_eq!(context, "Foo", ["@name", "initialize()"]);
        assert_owner_eq!(context, "Foo", "Object");
    }

    #[test]
    fn resolution_for_ambiguous_namespace_definitions() {
        // Like many examples of Ruby code that is ambiguous to static analysis, this example is ambiguous due to
        // require order. If `foo.rb` is loaded first, then `Bar` doesn't exist, Ruby crashes and we should emit an
        // error or warning for a non existing constant.
        //
        // If `bar.rb` is loaded first, then `Bar` resolves to top level `Bar` and `Bar::Baz` is defined, completely
        // escaping the `Foo` nesting.
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            module Foo
              class Bar::Baz
              end
            end
            "
        });
        context.index_uri("file:///bar.rb", {
            r"
            module Bar
            end
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context);

        assert_no_members!(context, "Foo");
        assert_owner_eq!(context, "Foo", "Object");

        assert_members_eq!(context, "Bar", ["Baz"]);
        assert_owner_eq!(context, "Bar", "Object");
    }

    #[test]
    fn expected_name_depth_order() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            module Foo
              module Bar
                module Baz
                end

                module ::Top
                  class AfterTop
                  end
                end
              end

              module Qux::Zip
                module Zap
                  class Zop::Boop
                  end
                end
              end
            end
            "
        });

        let depths = Resolver::compute_name_depths(context.graph().names());
        let mut names = context
            .graph()
            .names()
            .iter()
            .filter(|(_, n)| {
                !["Kernel", "BasicObject", "Object", "Module", "Class"]
                    .contains(&context.graph().strings().get(n.str()).unwrap().as_str())
            })
            .collect::<Vec<_>>();
        assert_eq!(10, names.len());

        names.sort_by_key(|(id, _)| depths.get(id).unwrap());

        assert_eq!(
            [
                "Top", "Foo", "Bar", "Qux", "AfterTop", "Baz", "Zip", "Zap", "Zop", "Boop"
            ],
            names
                .iter()
                .map(|(_, n)| context.graph().strings().get(n.str()).unwrap().as_str())
                .collect::<Vec<_>>()
                .as_slice()
        );
    }
}

mod singleton_class_tests {
    use super::*;

    #[test]
    fn resolution_for_singleton_class() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            class Foo
              class << self
                def bar; end
                BAZ = 123
              end
            end
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context);

        assert_no_members!(context, "Foo");
        assert_owner_eq!(context, "Foo", "Object");
        assert_singleton_class_eq!(context, "Foo", "Foo::<Foo>");

        assert_members_eq!(context, "Foo::<Foo>", ["BAZ", "bar()"]);
        assert_owner_eq!(context, "Foo::<Foo>", "Foo");
    }

    #[test]
    fn resolution_for_nested_singleton_class() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            class Foo
              class << self
                class << self
                  def baz; end
                end
              end
            end
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context);

        assert_no_members!(context, "Foo");
        assert_singleton_class_eq!(context, "Foo", "Foo::<Foo>");

        assert_no_members!(context, "Foo::<Foo>");
        assert_singleton_class_eq!(context, "Foo::<Foo>", "Foo::<Foo>::<<Foo>>");

        assert_members_eq!(context, "Foo::<Foo>::<<Foo>>", ["baz()"]);
        assert_owner_eq!(context, "Foo::<Foo>::<<Foo>>", "Foo::<Foo>");
    }

    #[test]
    fn resolution_for_singleton_class_of_external_constant() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            class Foo; end
            class Bar
              class << Foo
                def baz; end

                class Baz; end
              end
            end
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context);

        assert_no_members!(context, "Foo");
        assert_owner_eq!(context, "Foo", "Object");
        assert_singleton_class_eq!(context, "Foo", "Foo::<Foo>");

        assert_no_members!(context, "Bar");
        assert_owner_eq!(context, "Bar", "Object");

        assert_members_eq!(context, "Foo::<Foo>", ["Baz", "baz()"]);
        assert_owner_eq!(context, "Foo::<Foo>", "Foo");
    }

    #[test]
    fn singleton_class_is_set() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            class Foo
              class << self
              end
            end
            "
        });

        context.resolve();

        assert_no_diagnostics!(&context);

        assert_declaration_exists!(context, "Foo::<Foo>");
        assert_singleton_class_eq!(context, "Foo", "Foo::<Foo>");
    }

    #[test]
    fn incomplete_method_calls_automatically_trigger_singleton_creation() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            class Foo
            end

            Foo.
            "
        });
        context.resolve();
        assert_no_diagnostics!(&context, &[Rule::ParseError]);

        assert_declaration_references_count_eq!(context, "Foo::<Foo>", 1);
        assert_ancestors_eq!(
            context,
            "Foo::<Foo>",
            [
                "Foo::<Foo>",
                "Object::<Object>",
                "BasicObject::<BasicObject>",
                "Class",
                "Module",
                "Object",
                "Kernel",
                "BasicObject"
            ]
        );
    }

    #[test]
    fn singleton_class_calls_create_nested_singletons() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            class Foo
            end

            Foo.singleton_class.singleton_class.to_s
            "
        });
        context.resolve();
        assert_no_diagnostics!(&context);

        assert_declaration_references_count_eq!(context, "Foo::<Foo>::<<Foo>>::<<<Foo>>>", 1);
        assert_ancestors_eq!(
            context,
            "Foo::<Foo>::<<Foo>>::<<<Foo>>>",
            [
                "Foo::<Foo>::<<Foo>>::<<<Foo>>>",
                "Object::<Object>::<<Object>>::<<<Object>>>",
                "BasicObject::<BasicObject>::<<BasicObject>>::<<<BasicObject>>>",
                "Class::<Class>::<<Class>>",
                "Module::<Module>::<<Module>>",
                "Object::<Object>::<<Object>>",
                "BasicObject::<BasicObject>::<<BasicObject>>",
                "Class::<Class>",
                "Module::<Module>",
                "Object::<Object>",
                "BasicObject::<BasicObject>",
                "Class",
                "Module",
                "Object",
                "Kernel",
                "BasicObject"
            ]
        );
    }

    #[test]
    fn singleton_class_on_a_scoped_constant() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            module Foo
              class Bar
              end
            end

            Foo::Bar.singleton_class.to_s
            "
        });
        context.resolve();
        assert_no_diagnostics!(&context);

        assert_declaration_references_count_eq!(context, "Foo::Bar::<Bar>::<<Bar>>", 1);
        assert_ancestors_eq!(
            context,
            "Foo::Bar::<Bar>::<<Bar>>",
            [
                "Foo::Bar::<Bar>::<<Bar>>",
                "Object::<Object>::<<Object>>",
                "BasicObject::<BasicObject>::<<BasicObject>>",
                "Class::<Class>",
                "Module::<Module>",
                "Object::<Object>",
                "BasicObject::<BasicObject>",
                "Class",
                "Module",
                "Object",
                "Kernel",
                "BasicObject"
            ]
        );
    }

    #[test]
    fn singleton_class_on_a_self_call() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            class Foo
              class << self
                def bar
                  singleton_class.baz
                end
              end
            end
            "
        });
        context.resolve();
        assert_no_diagnostics!(&context);

        assert_declaration_references_count_eq!(context, "Foo::<Foo>::<<Foo>>", 1);
        assert_ancestors_eq!(
            context,
            "Foo::<Foo>::<<Foo>>",
            [
                "Foo::<Foo>::<<Foo>>",
                "Object::<Object>::<<Object>>",
                "BasicObject::<BasicObject>::<<BasicObject>>",
                "Class::<Class>",
                "Module::<Module>",
                "Object::<Object>",
                "BasicObject::<BasicObject>",
                "Class",
                "Module",
                "Object",
                "Kernel",
                "BasicObject"
            ]
        );
    }

    #[test]
    fn resolves_sibling_constant_inside_singleton_class_method_body() {
        // Constant referenced from inside a method defined in `class << self` must resolve against
        // the lexical scope that encloses the singleton class block, not stop at the singleton class.
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            module A
              module B
                class Sibling; end

                class Main
                  class << self
                    def does_not_resolve_here
                      Sibling
                    end
                  end
                end
              end
            end
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context);
        assert_constant_reference_to!(context, "A::B::Sibling", "file:///foo.rb:8:11-8:18");
    }

    #[test]
    fn resolves_sibling_constant_inside_nested_singleton_class() {
        // Nested `class << self` inside a nested class: lookup must still walk outward through
        // every enclosing lexical scope to find a sibling defined far above.
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            module A
              module B
                class Sibling; end

                class Main
                  class Inner
                    class << self
                      def m
                        Sibling
                      end
                    end
                  end
                end
              end
            end
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context);
        assert_constant_reference_to!(context, "A::B::Sibling", "file:///foo.rb:9:13-9:20");
    }

    #[test]
    fn resolves_sibling_constant_directly_in_singleton_class_body() {
        // Constant referenced directly in the `class << self` body (not inside a method) — e.g.
        // passed as an argument to a class-level DSL call — must also resolve lexically.
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            module A
              module B
                class Sibling; end

                class Main
                  class << self
                    Sibling
                  end
                end
              end
            end
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context);
        assert_constant_reference_to!(context, "A::B::Sibling", "file:///foo.rb:7:9-7:16");
    }

    #[test]
    fn singleton_class_lexical_scope_still_resolves_sibling_from_other_scopes() {
        // Sanity / non-regression: a sibling constant must continue to resolve from every other
        // scope where it already worked (instance method body, class body, top level).
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            module A
              module B
                class Sibling; end

                class Main
                  Sibling

                  def instance_method
                    Sibling
                  end
                end

                Sibling
              end
            end
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context, &[Rule::ParseWarning]);
        assert_constant_reference_to!(context, "A::B::Sibling", "file:///foo.rb:6:7-6:14");
        assert_constant_reference_to!(context, "A::B::Sibling", "file:///foo.rb:9:9-9:16");
        assert_constant_reference_to!(context, "A::B::Sibling", "file:///foo.rb:13:5-13:12");
    }

    #[test]
    fn singleton_class_scope_does_not_over_resolve_unknown_constant() {
        // Sanity: a constant that genuinely does not exist must remain unresolved even with the
        // fix in place — the fix must not invent resolutions by walking too far.
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            module A
              class Main
                class << self
                  def m
                    NotDefined
                  end
                end
              end
            end
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context, &[Rule::ParseWarning]);
        assert_constant_reference_unresolved!(context, "NotDefined", "file:///foo.rb:5:9-5:19");
    }
}

mod fqn_and_naming_tests {
    use super::*;

    #[test]
    fn distinct_declarations_with_conflicting_string_ids() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            class Foo
              def Array(); end
              class Array; end
            end
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context);

        // Both entries exist as unique members
        assert_members_eq!(context, "Foo", ["Array", "Array()"]);

        // Both declarations exist with unique IDs
        assert_declaration_exists!(context, "Foo::Array");
        assert_declaration_exists!(context, "Foo#Array()");
    }

    #[test]
    fn fully_qualified_names_are_unique() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            module Foo
              class Bar
                CONST = 1
                @class_ivar = 2

                attr_reader :baz
                attr_writer :qux
                attr_accessor :zip

                def instance_m
                  @@class_var = 3
                end

                def self.singleton_m
                  $global_var = 4
                end

                def Foo.another_singleton_m; end

                class << self
                  OTHER_CONST = 5
                  @other_class_ivar = 6
                  @@other_class_var = 7

                  def other_instance_m
                    @my_class_var = 8
                  end

                  def self.other_singleton_m
                    $other_global_var = 9
                  end
                end
              end
            end
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context);

        // In the same order of appearence
        assert_declaration_exists!(context, "Foo");
        assert_declaration_exists!(context, "Foo::Bar");
        assert_declaration_exists!(context, "Foo::Bar::CONST");
        assert_declaration_exists!(context, "Foo::Bar::<Bar>#@class_ivar");
        assert_declaration_exists!(context, "Foo::Bar#baz()");
        // TODO: needs the fix for attributes
        // assert_declaration_exists!(context, "Foo::Bar#qux=()");
        assert_declaration_exists!(context, "Foo::Bar#zip()");
        // TODO: needs the fix for attributes
        // assert_declaration_exists!(context, "Foo::Bar#zip=()");
        assert_declaration_exists!(context, "Foo::Bar#instance_m()");
        assert_declaration_exists!(context, "Foo::Bar#@@class_var");
        assert_declaration_exists!(context, "Foo::Bar::<Bar>#singleton_m()");
        assert_declaration_exists!(context, "$global_var");
        assert_declaration_exists!(context, "Foo::<Foo>#another_singleton_m()");
        assert_declaration_exists!(context, "Foo::Bar::<Bar>::OTHER_CONST");
        assert_declaration_exists!(context, "Foo::Bar::<Bar>::<<Bar>>#@other_class_ivar");
        assert_declaration_exists!(context, "Foo::Bar#@@other_class_var");
        assert_declaration_exists!(context, "Foo::Bar::<Bar>#other_instance_m()");
        assert_declaration_exists!(context, "Foo::Bar::<Bar>#@my_class_var");
        assert_declaration_exists!(context, "Foo::Bar::<Bar>::<<Bar>>#other_singleton_m()");
        assert_declaration_exists!(context, "$other_global_var");
    }

    #[test]
    fn test_nested_same_names() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
              module Foo; end

              module Bar
                Foo

                module Foo
                  FOO = 42
                end
              end
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context, &[Rule::ParseWarning]);

        // FIXME: this is wrong, the reference is not to `Bar::Foo`, but to `Foo`
        assert_constant_reference_to!(context, "Bar::Foo", "file:///foo.rb:4:3-4:6");

        assert_ancestors_eq!(context, "Foo", &["Foo"]);
        assert_ancestors_eq!(context, "Bar::Foo", &["Bar::Foo"]);

        assert_no_members!(context, "Foo");
        assert_members_eq!(context, "Bar::Foo", ["FOO"]);
    }
}

mod todo_tests {
    use super::*;

    #[test]
    fn resolution_does_not_loop_infinitely_on_non_existing_constants() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            class Foo::Bar
              class Baz
              end
            end
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context);

        assert_declaration_kind_eq!(context, "Foo", "<TODO>");

        assert_members_eq!(
            context,
            "Object",
            vec!["BasicObject", "Class", "Foo", "Kernel", "Module", "Object"]
        );
        assert_members_eq!(context, "Foo", vec!["Bar"]);
        assert_members_eq!(context, "Foo::Bar", vec!["Baz"]);
        assert_no_members!(context, "Foo::Bar::Baz");
    }

    #[test]
    fn resolve_missing_declaration_to_todo() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            class Foo::Bar
              include Foo::Baz

              def bar; end
            end

            module Foo::Baz
              def baz; end
            end
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context);

        assert_declaration_kind_eq!(context, "Foo", "<TODO>");

        assert_members_eq!(
            context,
            "Object",
            vec!["BasicObject", "Class", "Foo", "Kernel", "Module", "Object"]
        );
        assert_members_eq!(context, "Foo", vec!["Bar", "Baz"]);
        assert_members_eq!(context, "Foo::Bar", vec!["bar()"]);
        assert_members_eq!(context, "Foo::Baz", vec!["baz()"]);
    }

    #[test]
    fn qualified_name_inside_nesting_resolves_when_discovered_incrementally() {
        let mut context = GraphTest::new();
        context.index_uri("file:///baz.rb", {
            r"
            module Foo
              class Bar::Baz
                def qux; end
              end
            end
            "
        });
        context.resolve();

        // Bar is unknown — a Todo is created at the top level, not "Foo::Bar"
        assert_declaration_kind_eq!(context, "Bar", "<TODO>");
        assert_declaration_does_not_exist!(context, "Foo::Bar");

        context.index_uri("file:///bar.rb", {
            r"
            module Bar
            end
            "
        });
        context.resolve();

        // After discovering top-level Bar, the Todo should be promoted and Baz re-homed.
        assert_no_diagnostics!(&context);
        assert_declaration_kind_eq!(context, "Bar", "Module");
        assert_members_eq!(context, "Bar", vec!["Baz"]);
        assert_members_eq!(context, "Bar::Baz", vec!["qux()"]);
        assert_declaration_does_not_exist!(context, "Foo::Bar");
    }

    #[test]
    fn promoted_to_real_namespace() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            class Foo::Bar
              def bar; end
            end

            class Foo
              def foo; end
            end
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context);

        // Foo was initially created as a Todo (from class Foo::Bar), then promoted to Class
        assert_declaration_kind_eq!(context, "Foo", "Class");

        assert_members_eq!(
            context,
            "Object",
            vec!["BasicObject", "Class", "Foo", "Kernel", "Module", "Object"]
        );
        assert_members_eq!(context, "Foo", vec!["Bar", "foo()"]);
        assert_members_eq!(context, "Foo::Bar", vec!["bar()"]);
    }

    #[test]
    fn promoted_to_real_namespace_incrementally() {
        let mut context = GraphTest::new();
        context.index_uri("file:///bar.rb", {
            r"
            class Foo::Bar
              def bar; end
            end
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context);
        assert_declaration_kind_eq!(context, "Foo", "<TODO>");

        context.index_uri("file:///foo.rb", {
            r"
            class Foo
              def foo; end
            end
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context);

        // Foo was promoted from Todo to Class after the second resolution
        assert_declaration_kind_eq!(context, "Foo", "Class");

        assert_members_eq!(
            context,
            "Object",
            vec!["BasicObject", "Class", "Foo", "Kernel", "Module", "Object"]
        );
        assert_members_eq!(context, "Foo", vec!["Bar", "foo()"]);
        assert_members_eq!(context, "Foo::Bar", vec!["bar()"]);
    }

    #[test]
    fn two_levels_unknown() {
        // class A::B::C — neither A nor B exist. Both should become Todos, C is a Class.
        let mut context = GraphTest::new();
        context.index_uri("file:///a.rb", {
            r"
            class A::B::C
              def foo; end
            end
            "
        });
        context.resolve();

        assert_declaration_kind_eq!(context, "A", "<TODO>");
        assert_declaration_kind_eq!(context, "A::B", "<TODO>");
        assert_declaration_kind_eq!(context, "A::B::C", "Class");
        assert_members_eq!(
            context,
            "Object",
            vec!["A", "BasicObject", "Class", "Kernel", "Module", "Object"]
        );
        assert_members_eq!(context, "A", vec!["B"]);
        assert_members_eq!(context, "A::B", vec!["C"]);
        assert_members_eq!(context, "A::B::C", vec!["foo()"]);
    }

    #[test]
    fn three_levels_unknown() {
        // class A::B::C::D — A, B, C are all unknown. Tests recursion beyond depth 2.
        let mut context = GraphTest::new();
        context.index_uri("file:///a.rb", {
            r"
            class A::B::C::D
              def foo; end
            end
            "
        });
        context.resolve();

        assert_declaration_kind_eq!(context, "A", "<TODO>");
        assert_declaration_kind_eq!(context, "A::B", "<TODO>");
        assert_declaration_kind_eq!(context, "A::B::C", "<TODO>");
        assert_declaration_kind_eq!(context, "A::B::C::D", "Class");
        assert_members_eq!(
            context,
            "Object",
            vec!["A", "BasicObject", "Class", "Kernel", "Module", "Object"]
        );
        assert_members_eq!(context, "A", vec!["B"]);
        assert_members_eq!(context, "A::B", vec!["C"]);
        assert_members_eq!(context, "A::B::C", vec!["D"]);
        assert_members_eq!(context, "A::B::C::D", vec!["foo()"]);
    }

    #[test]
    fn partially_unresolvable() {
        // A exists but B doesn't — A resolves to a real Module, B becomes a Todo under A.
        let mut context = GraphTest::new();
        context.index_uri("file:///a.rb", {
            r"
            module A; end
            class A::B::C
              def foo; end
            end
            "
        });
        context.resolve();

        assert_declaration_kind_eq!(context, "A", "Module");
        assert_declaration_kind_eq!(context, "A::B", "<TODO>");
        assert_declaration_kind_eq!(context, "A::B::C", "Class");
        assert_members_eq!(context, "A", vec!["B"]);
        assert_members_eq!(context, "A::B", vec!["C"]);
        assert_members_eq!(context, "A::B::C", vec!["foo()"]);
    }

    #[test]
    fn shared_by_sibling_classes() {
        // Two classes share the same unknown parent chain. The Todos for A and B should
        // be created once and reused, with both C and D as members of B.
        let mut context = GraphTest::new();
        context.index_uri("file:///a.rb", {
            r"
            class A::B::C
              def c_method; end
            end

            class A::B::D
              def d_method; end
            end
            "
        });
        context.resolve();

        assert_declaration_kind_eq!(context, "A", "<TODO>");
        assert_declaration_kind_eq!(context, "A::B", "<TODO>");
        assert_declaration_kind_eq!(context, "A::B::C", "Class");
        assert_declaration_kind_eq!(context, "A::B::D", "Class");
        assert_members_eq!(
            context,
            "Object",
            vec!["A", "BasicObject", "Class", "Kernel", "Module", "Object"]
        );
        assert_members_eq!(context, "A", vec!["B"]);
        assert_members_eq!(context, "A::B", vec!["C", "D"]);
        assert_members_eq!(context, "A::B::C", vec!["c_method()"]);
        assert_members_eq!(context, "A::B::D", vec!["d_method()"]);
    }

    #[test]
    fn promoted_incrementally() {
        // Index class A::B::C first (creates Todos), then provide real definitions.
        // All Todos should be promoted to real namespaces.
        //
        // Note: we don't have true incremental resolution yet — each resolve() call
        // clears all declarations and re-resolves from scratch. This test verifies that
        // the promotion works when both files are present during the second resolution pass,
        // not that Todos are surgically updated in place.
        let mut context = GraphTest::new();
        context.index_uri("file:///c.rb", {
            r"
            class A::B::C
              def foo; end
            end
            "
        });
        context.resolve();

        assert_declaration_kind_eq!(context, "A", "<TODO>");
        assert_declaration_kind_eq!(context, "A::B", "<TODO>");
        assert_declaration_kind_eq!(context, "A::B::C", "Class");

        context.index_uri("file:///a.rb", {
            r"
            module A
              module B
              end
            end
            "
        });
        context.resolve();

        // Todos should be promoted
        assert_declaration_kind_eq!(context, "A", "Module");
        assert_declaration_kind_eq!(context, "A::B", "Module");
        assert_declaration_kind_eq!(context, "A::B::C", "Class");
        assert_members_eq!(context, "A", vec!["B"]);
        assert_members_eq!(context, "A::B", vec!["C"]);
        assert_members_eq!(context, "A::B::C", vec!["foo()"]);
    }

    #[test]
    fn with_self_method_and_ivar() {
        // def self.foo with @x inside a multi-level compact class — the SelfReceiver
        // on the method must find C's declaration to create the singleton class and ivar.
        let mut context = GraphTest::new();
        context.index_uri("file:///a.rb", {
            r"
            class A::B::C
              def self.foo
                @x = 1
              end
            end
            "
        });
        context.resolve();

        assert_declaration_kind_eq!(context, "A", "<TODO>");
        assert_declaration_kind_eq!(context, "A::B", "<TODO>");
        assert_declaration_kind_eq!(context, "A::B::C", "Class");
        assert_declaration_exists!(context, "A::B::C::<C>#foo()");
        assert_declaration_exists!(context, "A::B::C::<C>#@x");
    }

    #[test]
    fn nested_inside_module_with_separate_intermediate() {
        // Compact namespace nested inside a module, where the intermediate namespace
        // is defined separately. Bar::Baz should become a Todo since only Bar exists.
        let mut context = GraphTest::new();
        context.index_uri("file:///a.rb", {
            r"
            module Foo
              class Bar::Baz::Qux
              end
            end

            module Bar; end
            "
        });
        context.resolve();

        assert_declaration_kind_eq!(context, "Foo", "Module");
        assert_declaration_kind_eq!(context, "Bar", "Module");
        assert_declaration_kind_eq!(context, "Bar::Baz", "<TODO>");
        assert_declaration_kind_eq!(context, "Bar::Baz::Qux", "Class");
        assert_members_eq!(context, "Bar", vec!["Baz"]);
        assert_members_eq!(context, "Bar::Baz", vec!["Qux"]);
    }

    #[test]
    fn no_todo_when_parent_is_reachable_through_include() {
        // Baz::Qux inside Foo, where Baz comes from included Bar module.
        // Baz::Qux should resolve through inheritance to Bar::Baz::Qux, not create
        // a top-level Baz Todo.
        let mut context = GraphTest::new();
        context.index_uri("file:///file1.rb", {
            r"
            module Foo
              include Bar

              class Baz::Qux; end
            end
            "
        });
        context.index_uri("file:///file2.rb", {
            r"
            module Bar
              module Baz; end
            end
            "
        });
        context.resolve();

        assert_declaration_exists!(context, "Bar::Baz");
        assert_declaration_exists!(context, "Bar::Baz::Qux");
        assert_members_eq!(context, "Bar::Baz", vec!["Qux"]);
        assert_declaration_does_not_exist!(context, "Foo::Baz");
        // No spurious top-level Baz Todo should be created
        assert_declaration_does_not_exist!(context, "Baz");
        // Baz::Qux should NOT exist at top level
        assert_declaration_does_not_exist!(context, "Baz::Qux");
    }

    #[test]
    fn intermediate_todo_on_constant_alias() {
        let mut context = GraphTest::new();
        context.index_uri("file:///alias.rb", {
            r"
            module Bar; end
            module Foo; end
            Foo::Bar = Bar
            "
        });
        context.index_uri("file:///qux.rb", {
            r"
            class Foo::Bar::Baz::Qux
            end
            "
        });

        context.resolve();
        assert_no_diagnostics!(&context);

        assert_declaration_kind_eq!(context, "Foo", "Module");
        assert_declaration_kind_eq!(context, "Bar", "Module");
        assert_declaration_kind_eq!(context, "Foo::Bar", "ConstantAlias");
        assert_declaration_kind_eq!(context, "Bar::Baz", "<TODO>");
        assert_declaration_kind_eq!(context, "Bar::Baz::Qux", "Class");
    }

    #[test]
    fn rbs_method_definition() {
        let mut context = GraphTest::new();
        context.index_rbs_uri("file:///foo.rbs", {
            r"
            class Foo
              def foo: () -> void

              def self.bar: () -> void

              def self?.baz: () -> void
            end
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context);

        assert_members_eq!(context, "Foo", ["baz()", "foo()"]);
        assert_members_eq!(context, "Foo::<Foo>", ["bar()", "baz()"]);
    }
    #[test]
    fn resolves_constant_with_ancestors_partial() {
        // B has Ancestors::Partial because its prepend is defined in another file.
        // X must wait for B's ancestors to resolve, then resolve to A::X.
        let mut context = GraphTest::new();
        context.index_uri("file:///1.rb", {
            r"
            module A
              X = 1
            end
            class B
              X = 2
            end
            class C < B
              X
            end
            "
        });
        context.index_uri("file:///2.rb", {
            r"
            class B
              prepend A
            end
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context);

        assert_ancestors_eq!(context, "C", ["C", "A", "B", "Object", "Kernel", "BasicObject"]);
        assert_constant_reference_to!(context, "A::X", "file:///1.rb:8:3-8:4");
    }

    #[test]
    fn resolves_constant_with_ancestor_partial() {
        // C has an Ancestor::Partial entry because O::A is defined in another file.
        // X must wait for O::A to resolve, then resolve to O::A::X.
        let mut context = GraphTest::new();
        context.index_uri("file:///1.rb", {
            r"
            class B
              X = 2
            end
            class C
              include B
              include O::A
              X
            end
            "
        });
        context.index_uri("file:///2.rb", {
            r"
            module O
              module A
                X = 1
              end
            end
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context);

        assert_ancestors_eq!(context, "C", ["C", "O::A", "B", "Object", "Kernel", "BasicObject"]);
        assert_constant_reference_to!(context, "O::A::X", "file:///1.rb:7:3-7:4");
    }

    #[test]
    fn method_call_on_undefined_constant() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            Foo.bar
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context);
        assert_declaration_does_not_exist!(context, "Foo::<Foo>");
    }

    #[test]
    fn qualified_name_inside_nesting_resolves_to_top_level() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            module Foo
              class Bar::Baz
                def qux; end
              end
            end

            module Bar
            end
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context);
        assert_declaration_kind_eq!(context, "Bar", "Module");
        assert_members_eq!(context, "Bar", vec!["Baz"]);
        assert_declaration_exists!(context, "Bar::Baz");
        assert_members_eq!(context, "Bar::Baz", vec!["qux()"]);
        assert_declaration_does_not_exist!(context, "Foo::Bar");
    }
}

mod dynamic_namespace_tests {
    use super::*;

    #[test]
    fn resolving_meta_programming_class_reopened() {
        // It's often not possible to provide first-class support to meta-programming constructs, but we have to prevent
        // the implementation from crashing in cases like these.
        //
        // Here we use some meta-programming method call to define a class and then re-open it using the `class`
        // keyword. The first definition of Bar is considered a constant because we don't know `dynamic_class` returns a
        // new class. The second definition is a class.
        //
        // We need to ensure that the associated Declaration for Bar is transformed into a class if any of its
        // definitions represent one, otherwise we have no place to store the includes and ancestors
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            module Baz; end

            Bar = dynamic_class do
            end

            class Bar
              include Baz
            end
            "
        });

        context.resolve();
        assert_no_diagnostics!(&context);
        assert_ancestors_eq!(context, "Bar", ["Bar", "Baz", "Object", "Kernel", "BasicObject"]);
    }

    #[test]
    fn resolving_accessing_meta_programming_class() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            Foo = Protobuf.some_dynamic_class
            Foo::Bar = Protobuf.some_other_dynamic_class
            "
        });

        context.resolve();
        assert_no_diagnostics!(&context);
    }

    #[test]
    fn inheriting_from_dynamic_class() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            Foo = some_dynamic_class
            class Bar < Foo
            end
            "
        });

        context.resolve();
        assert_no_diagnostics!(&context);
        assert_ancestors_eq!(context, "Bar", ["Bar", "Object", "Kernel", "BasicObject"]);
    }

    #[test]
    fn including_dynamic_module() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            Foo = some_dynamic_module
            class Bar
              include Foo
            end
            "
        });

        context.resolve();
        assert_no_diagnostics!(&context);
        assert_ancestors_eq!(context, "Bar", ["Bar", "Object", "Kernel", "BasicObject"]);
    }

    #[test]
    fn prepending_dynamic_module() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            Foo = some_dynamic_module
            class Bar
              prepend Foo
            end
            "
        });

        context.resolve();
        assert_no_diagnostics!(&context);
        assert_ancestors_eq!(context, "Bar", ["Bar", "Object", "Kernel", "BasicObject"]);
    }

    #[test]
    fn extending_dynamic_module() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            Foo = some_dynamic_module
            class Bar
              extend Foo

              class << self
              end
            end
            "
        });

        context.resolve();
        assert_no_diagnostics!(&context);
        assert_ancestors_eq!(
            context,
            "Bar::<Bar>",
            [
                "Bar::<Bar>",
                "Object::<Object>",
                "BasicObject::<BasicObject>",
                "Class",
                "Module",
                "Object",
                "Kernel",
                "BasicObject"
            ]
        );
    }

    #[test]
    fn ancestor_operations_on_meta_programming_class() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            module Foo; end
            module Bar; end

            Qux = dynamic_class do
              include Foo
              prepend Bar
            end
            "
        });

        context.resolve();
        assert_no_diagnostics!(&context);
    }
}

mod promotability_tests {
    use super::*;

    #[test]
    fn non_promotable_constant_not_promoted_to_class_with_members() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            FOO = 42
            class FOO
              def bar; end
            end
            "
        });

        context.resolve();
        assert_no_diagnostics!(&context);
        assert_declaration_exists!(context, "FOO");
    }

    #[test]
    fn non_promotable_constant_not_promoted_to_module() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r#"
                FOO = "hello"
                module FOO
                end
                "#
        });

        context.resolve();
        assert_no_diagnostics!(&context);
        assert_declaration_kind_eq!(context, "FOO", "Constant");
    }

    #[test]
    fn promotable_constant_is_promoted_to_class() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            module Baz; end

            Bar = some_call

            class Bar
              include Baz
            end
            "
        });

        context.resolve();
        assert_no_diagnostics!(&context);
        assert_ancestors_eq!(context, "Bar", ["Bar", "Baz", "Object", "Kernel", "BasicObject"]);
    }

    #[test]
    fn mixed_promotable_and_non_promotable_blocks_promotion() {
        // If the same constant has both a promotable and non-promotable definition,
        // promotion should be blocked
        let mut context = GraphTest::new();
        context.index_uri("file:///a.rb", "Foo = some_call");
        context.index_uri("file:///b.rb", "Foo = 42");
        context.index_uri("file:///c.rb", "class Foo; end");

        context.resolve();
        assert_no_diagnostics!(&context);
        assert_declaration_kind_eq!(context, "Foo", "Constant");
    }

    #[test]
    fn promotable_constant_promoted_to_module() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            module Baz; end

            Bar = some_call

            module Bar
              include Baz
            end
            "
        });

        context.resolve();
        assert_no_diagnostics!(&context);
        assert_ancestors_eq!(context, "Bar", ["Bar", "Baz"]);
    }

    #[test]
    fn class_first_then_constant_stays_namespace() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            class Foo; end
            Foo = some_call
            "
        });

        context.resolve();
        assert_no_diagnostics!(&context);
        assert_declaration_kind_eq!(context, "Foo", "Class");
    }

    #[test]
    fn promotable_constant_path_write() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            module A; end
            A::B = some_factory_call
            class A::B; end
            "
        });

        context.resolve();
        assert_no_diagnostics!(&context);
        assert_declaration_exists!(context, "A::B");
    }

    #[test]
    fn method_call_on_promotable_constant() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            Qux = some_factory_call
            Qux.foo
            "
        });

        context.resolve();
        assert_no_diagnostics!(&context);
        assert_declaration_exists!(context, "Qux::<Qux>");
    }

    #[test]
    fn singleton_method_on_non_promotable_constant_does_not_crash() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            FOO = 42
            FOO.bar
            "
        });

        context.resolve();
        assert_no_diagnostics!(&context);
        assert_declaration_does_not_exist!(context, "FOO::<FOO>");
    }

    #[test]
    fn def_self_on_promotable_constant() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            Qux = some_factory_call
            def Qux.foo; end
            "
        });

        context.resolve();
        assert_no_diagnostics!(&context);
        assert_declaration_exists!(context, "Qux::<Qux>");
    }

    #[test]
    fn promoted_constant_has_correct_ancestors() {
        // When a promotable constant is auto-promoted via singleton class access, we conservatively
        // promote to a module (not a class) since we don't know what the call returns.
        // Modules don't inherit from Object.
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            Foo = some_factory_call
            Foo.bar
            "
        });

        context.resolve();
        assert_no_diagnostics!(&context);
        assert_ancestors_eq!(context, "Foo", ["Foo"]);
    }

    #[test]
    fn meta_programming_class_with_members() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            Foo = dynamic_class do
              def bar; end
            end
            "
        });

        context.resolve();
        assert_no_diagnostics!(&context);
        assert_declaration_exists!(context, "Foo");
        assert_declaration_does_not_exist!(context, "Foo#bar()");
    }

    #[test]
    fn self_method_inside_non_promotable_constant() {
        let mut context = GraphTest::new();
        context.index_uri("file:///a.rb", {
            r"
            CONST = 1
            module CONST
              def self.bar
              end
            end
            "
        });
        // Should not panic when a `def self.` method is inside a constant that can't be promoted to a namespace (e.g.,
        // `CONST = 1` is non-promotable).
        context.resolve();
        assert_declaration_exists!(context, "CONST");
    }

    #[test]
    fn defining_constant_in_promotable_constant() {
        let mut context = GraphTest::new();
        context.index_uri("file:///a.rb", {
            r"
            Foo = dynamic
            Foo::Bar = dynamic
            Foo::Bar::Baz = 123
            "
        });

        context.resolve();
        assert_declaration_kind_eq!(context, "Foo", "Module");
        assert_declaration_kind_eq!(context, "Foo::Bar", "Module");
        assert_declaration_kind_eq!(context, "Foo::Bar::Baz", "Constant");
    }

    #[test]
    fn singleton_class_block_for_promotable_constant() {
        let mut context = GraphTest::new();
        context.index_uri("file:///a.rb", {
            r"
            Foo = dynamic

            class << Foo
              def bar; end
            end
            "
        });

        context.resolve();
        assert_declaration_kind_eq!(context, "Foo", "Module");
        assert_declaration_exists!(context, "Foo::<Foo>#bar()");
    }

    #[test]
    fn singleton_class_block_for_non_promotable_constant() {
        let mut context = GraphTest::new();
        context.index_uri("file:///a.rb", {
            r"
            Foo = 1

            class << Foo
              def bar; end
            end
            "
        });

        context.resolve();
        assert_declaration_kind_eq!(context, "Foo", "Constant");
        assert_declaration_does_not_exist!(context, "Foo::<Foo>");
        assert_declaration_does_not_exist!(context, "Foo::<Foo>#bar()");
    }
}

mod rbs_tests {
    use super::*;

    #[test]
    fn rbs_module_and_class_declarations() {
        let mut context = GraphTest::new();
        context.index_rbs_uri("file:///test.rbs", {
            r"
            module Foo
            end

            class Bar
            end
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context);

        assert_declaration_exists!(context, "Foo");
        assert_declaration_exists!(context, "Bar");
    }

    #[test]
    fn rbs_nested_declarations() {
        let mut context = GraphTest::new();
        context.index_rbs_uri("file:///test.rbs", {
            r"
            module Foo
              module Bar
              end

              class Baz
                class Qux
                end
              end
            end
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context);

        assert_owner_eq!(context, "Foo::Bar", "Foo");
        assert_owner_eq!(context, "Foo::Baz", "Foo");
        assert_owner_eq!(context, "Foo::Baz::Qux", "Foo::Baz");
        assert_members_eq!(context, "Foo", ["Bar", "Baz"]);
        assert_members_eq!(context, "Foo::Baz", ["Qux"]);
    }

    #[test]
    fn rbs_qualified_module_name() {
        let mut context = GraphTest::new();
        context.index_rbs_uri("file:///parents.rbs", {
            r"
            module Foo
              module Bar
              end
            end
            "
        });
        context.index_rbs_uri("file:///test.rbs", {
            r"
            module Foo::Bar::Baz
            end
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context);

        assert_declaration_exists!(context, "Foo::Bar::Baz");
    }

    #[test]
    fn rbs_qualified_name_inside_nested_module() {
        let mut context = GraphTest::new();
        context.index_rbs_uri("file:///foo.rbs", {
            r"
            module Outer
              module Foo
              end
            end
            "
        });
        context.index_rbs_uri("file:///test.rbs", {
            r"
            module Outer
              module Foo::Bar
              end
            end
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context);

        assert_owner_eq!(context, "Outer::Foo::Bar", "Outer::Foo");
    }

    #[test]
    fn rbs_superclass_resolution() {
        let mut context = GraphTest::new();
        context.index_rbs_uri("file:///test.rbs", {
            r"
            class Foo
            end

            class Bar < Foo
            end

            module Baz
              class Base
              end

              class Child < Base
              end
            end
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context);

        assert_ancestors_eq!(context, "Bar", ["Bar", "Foo", "Object", "Kernel", "BasicObject"]);
        assert_ancestors_eq!(
            context,
            "Baz::Child",
            ["Baz::Child", "Baz::Base", "Object", "Kernel", "BasicObject"]
        );
    }

    #[test]
    fn rbs_constant_declarations() {
        let mut context = GraphTest::new();
        context.index_rbs_uri("file:///test.rbs", {
            r"
            FOO: String

            class Bar
              BAZ: Integer
            end

            Bar::QUX: ::String
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context);

        assert_declaration_exists!(context, "FOO");
        assert_declaration_kind_eq!(context, "FOO", "Constant");
        assert_owner_eq!(context, "FOO", "Object");

        assert_declaration_exists!(context, "Bar::BAZ");
        assert_declaration_kind_eq!(context, "Bar::BAZ", "Constant");
        assert_owner_eq!(context, "Bar::BAZ", "Bar");

        assert_declaration_exists!(context, "Bar::QUX");
        assert_declaration_kind_eq!(context, "Bar::QUX", "Constant");
        assert_owner_eq!(context, "Bar::QUX", "Bar");
    }

    #[test]
    fn rbs_global_declaration() {
        let mut context = GraphTest::new();
        context.index_rbs_uri("file:///test.rbs", "$foo: String");
        context.resolve();

        assert_no_diagnostics!(&context);

        assert_members_eq!(
            context,
            "Object",
            ["$foo", "BasicObject", "Class", "Kernel", "Module", "Object"]
        );
    }

    #[test]
    fn rbs_mixin_resolution() {
        let mut context = GraphTest::new();
        context.index_rbs_uri("file:///test.rbs", {
            r"
            module Bar
            end

            module Baz
            end

            class Foo
              include Bar
              include Baz
            end
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context);

        assert_ancestors_eq!(context, "Foo", ["Foo", "Baz", "Bar", "Object", "Kernel", "BasicObject"]);
    }

    #[test]
    fn rbs_method_alias_resolution() {
        let mut context = GraphTest::new();
        context.index_uri("file:///foo.rb", {
            r"
            class Foo
              def bar; end
              def self.class_method; end
            end

            module Baz
              def original; end
            end
            "
        });
        context.index_rbs_uri("file:///test.rbs", {
            r"
            class Foo
              alias qux bar
              alias self.class_alias self.class_method
            end

            module Baz
              alias copy original
            end
            "
        });
        context.resolve();

        assert_no_diagnostics!(&context);

        assert_members_eq!(context, "Foo", ["bar()", "qux()"]);
        assert_members_eq!(context, "Foo::<Foo>", ["class_alias()", "class_method()"]);
        assert_members_eq!(context, "Baz", ["copy()", "original()"]);
    }
}

#[test]
fn singleton_class_created_in_remaining_definitions_has_linearized_ancestors() {
    let mut context = GraphTest::new();
    context.index_uri(
        "file:///foo.rb",
        r"
            class Foo
              @var = 1
            end
            ",
    );
    context.resolve();

    assert_no_diagnostics!(&context);
    assert_ancestors_eq!(
        context,
        "Foo::<Foo>",
        [
            "Foo::<Foo>",
            "Object::<Object>",
            "BasicObject::<BasicObject>",
            "Class",
            "Module",
            "Object",
            "Kernel",
            "BasicObject"
        ]
    );
}

mod visibility_resolution_tests {
    use super::*;
    use crate::model::visibility::Visibility;

    macro_rules! assert_visibility_eq {
        ($context:expr, $declaration_name:expr, $expected_visibility:expr) => {
            let decl_id = crate::model::ids::DeclarationId::from($declaration_name);
            let actual = $context
                .graph()
                .visibility(&decl_id)
                .unwrap_or_else(|| panic!("No visibility found for `{}`", $declaration_name));
            assert_eq!(
                actual, $expected_visibility,
                "Expected `{}` to have visibility {}, got {}",
                $declaration_name, $expected_visibility, actual
            );
        };
    }

    #[test]
    fn retroactive_visibility_on_direct_method() {
        let mut context = GraphTest::new();
        context.index_uri(
            "file:///foo.rb",
            r"
            class Foo
              def bar; end
              private :bar

              def baz; end
              protected :baz

              private def qux; end
              public :qux
            end
            ",
        );
        context.resolve();

        assert_no_diagnostics!(&context);
        assert_visibility_eq!(context, "Foo#bar()", Visibility::Private);
        assert_visibility_eq!(context, "Foo#baz()", Visibility::Protected);
        assert_visibility_eq!(context, "Foo#qux()", Visibility::Public);
    }

    #[test]
    fn retroactive_visibility_on_attr_methods() {
        let mut context = GraphTest::new();
        context.index_uri(
            "file:///foo.rb",
            r"
            class Foo
              attr_reader :reader_method
              private :reader_method

              attr_writer :writer_method
              protected :writer_method

              attr_accessor :accessor_method
              private :accessor_method
            end
            ",
        );
        context.resolve();

        assert_no_diagnostics!(&context);
        assert_visibility_eq!(context, "Foo#reader_method()", Visibility::Private);
        assert_visibility_eq!(context, "Foo#writer_method()", Visibility::Protected);
        assert_visibility_eq!(context, "Foo#accessor_method()", Visibility::Private);
    }

    #[test]
    fn retroactive_visibility_on_inherited_method() {
        let mut context = GraphTest::new();
        context.index_uri(
            "file:///foo.rb",
            r"
            class Parent
              def foo; end
            end

            class Child < Parent
              private :foo
            end
            ",
        );
        context.resolve();

        assert_no_diagnostics!(&context);
        assert_declaration_exists!(context, "Child#foo()");
        assert_members_eq!(context, "Child", ["foo()"]);
        assert_owner_eq!(context, "Child#foo()", "Child");
        assert_visibility_eq!(context, "Child#foo()", Visibility::Private);
        assert_visibility_eq!(context, "Parent#foo()", Visibility::Public);
    }

    #[test]
    fn retroactive_visibility_on_grandparent_method() {
        let mut context = GraphTest::new();
        context.index_uri(
            "file:///foo.rb",
            r"
            class GrandParent
              def greet; end
            end

            class Parent < GrandParent; end

            class Child < Parent
              private :greet
            end
            ",
        );
        context.resolve();

        assert_no_diagnostics!(&context);
        assert_owner_eq!(context, "Child#greet()", "Child");
        assert_visibility_eq!(context, "Child#greet()", Visibility::Private);
    }

    #[test]
    fn retroactive_visibility_on_included_module_method() {
        let mut context = GraphTest::new();
        context.index_uri(
            "file:///foo.rb",
            r"
            module Greetable
              def greet; end
            end

            class Foo
              include Greetable
              private :greet
            end
            ",
        );
        context.resolve();

        assert_no_diagnostics!(&context);
        assert_owner_eq!(context, "Foo#greet()", "Foo");
        assert_visibility_eq!(context, "Foo#greet()", Visibility::Private);
    }

    #[test]
    fn retroactive_visibility_on_undefined_method_emits_diagnostic() {
        let mut context = GraphTest::new();
        context.index_uri(
            "file:///foo.rb",
            r"
            class Foo
              private :nonexistent
            end
            ",
        );
        context.resolve();

        assert_diagnostics_eq!(
            context,
            &[
                "undefined-method-visibility-target: undefined method `nonexistent()` for visibility change in `Foo` (2:12-2:23)"
            ]
        );
    }

    #[test]
    fn retroactive_visibility_across_reopened_class() {
        let mut context = GraphTest::new();
        context.index_uri(
            "file:///a.rb",
            r"
            class Foo
              def bar; end
            end
            ",
        );
        context.index_uri(
            "file:///b.rb",
            r"
            class Foo
              private :bar
            end
            ",
        );
        context.resolve();

        assert_no_diagnostics!(&context);
        assert_visibility_eq!(context, "Foo#bar()", Visibility::Private);
    }

    #[test]
    fn retroactive_visibility_resolves_when_ancestor_discovered_incrementally() {
        let mut context = GraphTest::new();
        context.index_uri(
            "file:///child.rb",
            r"
            class Child
              include M
              private :foo
            end
            ",
        );
        context.resolve();

        assert_no_diagnostics!(&context);
        assert_declaration_does_not_exist!(context, "Child#foo()");

        context.index_uri(
            "file:///module.rb",
            r"
            module M
              def foo; end
            end
            ",
        );
        context.resolve();

        assert_no_diagnostics!(&context);
        assert_declaration_exists!(context, "Child#foo()");
        assert_owner_eq!(context, "Child#foo()", "Child");
        assert_visibility_eq!(context, "Child#foo()", Visibility::Private);
    }
}
