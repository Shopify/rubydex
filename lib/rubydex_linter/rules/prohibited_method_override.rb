# typed: strict
# frozen_string_literal: true

module Rubydex
  module Linter
    module Rules
      # Prohibits overriding configured methods at the instance or singleton level.
      class ProhibitedMethodOverride < CustomRule
        DEFAULTS = {
          "Module#private()" => :singleton,
          "Module#public()" => :singleton,
          "Module#protected()" => :singleton,
          "Module#include()" => :singleton,
          "Kernel#extend()" => :singleton,
          "Module#prepend()" => :singleton,
        }.freeze #: Hash[String, Symbol]

        class << self
          # @override
          #: -> singleton(Severity::Base)
          def default_severity
            Severity::Error
          end
        end

        # @override
        #: -> void
        def lint
          overrides = prohibited_overrides
          return if overrides.empty?

          overrides_by_name = overrides.group_by { |name, _| name.split("#", 2).last }
          query = overrides_by_name.keys.map { |m| "##{m}" }
          results = graph.search(*query)

          results.each do |m|
            owner = m.owner

            configs = overrides_by_name[m.unqualified_name]
            next unless configs

            configs.each do |full_method_name, level|
              protected_owner = full_method_name.split("#", 2).first #: as !nil
              next if protected_owner == owner.name

              singleton = owner.is_a?(Rubydex::SingletonClass)

              # If the owner is a class or singleton class, we don't have to check descendants as we'll already flag the
              # override on the class itself and that is the problem to be fixed
              unless owner.is_a?(Rubydex::Module)
                next unless overrides_owner?(owner.ancestors, owner.name, protected_owner)

                if level == :any || (singleton && level == :singleton) || (!singleton && level == :instance)
                  first_def, *other_defs = m.definitions.to_a

                  add_diagnostic(
                    "Prohibited override of `#{full_method_name}`",
                    diagnostic_location(
                      first_def, #: as !nil
                    ),
                    related_information: other_defs.map { |d| RelatedInformation.new("also defined here", diagnostic_location(d)) },
                  )
                end

                next
              end

              # On modules, the story is a bit different. An override is only prohibited if the module is mixed in a way
              # that would override the method in a prohibited way. For example, if a module defines `private` and gets
              # extended, we want to flag it as that's overriding `Module#private`, but not if the module is being included
              # since that's not overriding anything.
              #
              # This part of the check is more expensive as it needs to traverse the module's descendants
              collect_mixin_violations(level, owner, full_method_name, m)
            end
          end
        end

        private

        #: -> Hash[String, Symbol]
        def prohibited_overrides
          overrides = DEFAULTS.dup

          options.each do |name, level|
            # Verify if the configuration is actually an `Owner#method()` string.
            unless /[A-Z][a-z0-9A-Z:]+#[A-Za-z0-9_]+\(\)/.match?(name)
              raise ArgumentError, "Invalid `#{rule_name}` option #{name.inspect}: expected an `Owner#method()` key"
            end

            case level
            when "instance", "singleton", "any"
              overrides[name] = level.to_sym
            when false
              overrides.delete(name)
            else
              raise ArgumentError, "Invalid `#{rule_name}.#{name}`: expected \"instance\", \"singleton\", \"any\", or false"
            end
          end

          overrides
        end

        # Checks if the owner of the method we found appears before the protected one we're trying to prevent overrides
        # for. Also ensures that the method owner actually does inherit from the protected owner to prevent false
        # positives on unrelated classes using the same method name.
        #
        #: (Enumerable[Rubydex::Namespace], String, String) -> bool
        def overrides_owner?(ancestors, method_owner, protected_owner)
          found_method_owner = false

          ancestors.each do |ancestor|
            ancestor_name = ancestor.name

            # Both owners must appear in the chain, with the overriding method first.
            return found_method_owner if ancestor_name == protected_owner

            found_method_owner = true if ancestor_name == method_owner
          end

          false
        end

        #: (Symbol, Rubydex::Namespace, String, Rubydex::Method) -> void
        def collect_mixin_violations(level, owner, full_method_name, method_declaration)
          protected_owner = full_method_name.split("#", 2).first #: as !nil
          descendants = case level
          when :singleton
            owner.descendants.grep(Rubydex::SingletonClass)
          when :instance
            owner.descendants.select { |d| !d.is_a?(Rubydex::SingletonClass) }
          else
            owner.descendants
          end

          # This code path means that an override got applied due to extending/including/prepending a module. We want to
          # attach the violation to the ancestor operation and not the entire class/module. Note that for singleton
          # classes, the offending operation could be an include/prepend in the singleton itself or an extend in the
          # attached class
          offending_operations = [] #: Array[[Rubydex::Namespace, Rubydex::ResolvedConstantReference]]

          descendants.each do |descendant|
            next if descendant.name == owner.name
            next unless overrides_owner?(descendant.ancestors, owner.name, protected_owner)

            offending_operations.concat(collect_mixins(descendant, owner.name))
          end

          offending_operations.each do |descendant, ref|
            add_diagnostic(
              "Prohibited override of `#{full_method_name}`",
              ref.location,
              related_information: [
                RelatedInformation.new(
                  "`#{descendant.name}` mixes `#{full_method_name}` from `#{owner.name}`",
                  diagnostic_location(
                    method_declaration.definitions.first, #: as !nil
                  ),
                ),
              ],
            )
          end
        end

        #: (Rubydex::Namespace, String, ?bool) -> Array[[Rubydex::Namespace, Rubydex::ResolvedConstantReference]]
        def collect_mixins(declaration, owner_name, gather_extend = false)
          offending_operations = [] #: Array[[Rubydex::Namespace, Rubydex::ResolvedConstantReference]]

          declaration.definitions.each do |definition|
            # Narrow the definition type
            unless definition.is_a?(Rubydex::ModuleDefinition) ||
                definition.is_a?(Rubydex::ClassDefinition) ||
                definition.is_a?(Rubydex::SingletonClassDefinition)
              next
            end

            # Find the mixin operation that caused the override
            definition.mixins.each do |mixin|
              # If we're recursing to check the attached class of a singleton, we only care about extends. If we're
              # looking at the current namespace directly, we only care about include and prepend
              next if (gather_extend && !mixin.is_a?(Rubydex::Extend)) || (!gather_extend && mixin.is_a?(Rubydex::Extend))

              ref = mixin.constant_reference
              next unless ref.is_a?(Rubydex::ResolvedConstantReference)
              next unless ref.declaration.is_a?(Rubydex::Namespace) && ref.declaration.has_ancestor?(owner_name)

              offending_operations << [declaration, ref]
            end
          end

          # If the declaration is a singleton class, recurse and look at the attached class to see if any extends caused
          # the override
          if declaration.is_a?(Rubydex::SingletonClass)
            offending_operations.concat(
              collect_mixins(
                declaration.owner, #: as Rubydex::Namespace
                owner_name,
                true,
              ),
            )
          end

          offending_operations
        end
      end
    end
  end
end
