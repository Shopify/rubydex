# frozen_string_literal: true
# typed: strict

module Rubydex; end

class Rubydex::Comment
  sig { params(string: String, location: Rubydex::Location).void }
  def initialize(string:, location:); end

  sig { returns(Rubydex::Location) }
  def location; end

  sig { returns(String) }
  def string; end
end

class Rubydex::ConstantReference < Rubydex::Reference
  abstract!

  sig { returns(Rubydex::Location) }
  def location; end

  sig { returns(Rubydex::Document) }
  def document; end

  class << self
    private

    def new(*args); end
  end
end

class Rubydex::UnresolvedConstantReference < Rubydex::ConstantReference
  sig { returns(String) }
  def name; end
end

class Rubydex::ResolvedConstantReference < Rubydex::ConstantReference
  sig { returns(Rubydex::Declaration) }
  def declaration; end
end

class Rubydex::Declaration
  abstract!

  sig { returns(T::Enumerable[Rubydex::Definition]) }
  def definitions; end

  sig { returns(String) }
  def name; end

  sig { returns(Rubydex::Declaration) }
  def owner; end

  sig { returns(T::Enumerable[Rubydex::Reference]) }
  def references; end

  sig { returns(String) }
  def unqualified_name; end

  class << self
    private

    def new(*args); end
  end
end

class Rubydex::GlobalVariable < Rubydex::Declaration
  sig { returns(T::Array[T.untyped]) }
  def references; end
end

class Rubydex::InstanceVariable < Rubydex::Declaration
  sig { returns(T::Array[T.untyped]) }
  def references; end
end

class Rubydex::Constant < Rubydex::Declaration
  sig { returns(T::Enumerable[Rubydex::ConstantReference]) }
  def references; end
end

class Rubydex::ConstantAlias < Rubydex::Declaration
  sig { returns(T::Enumerable[Rubydex::ConstantReference]) }
  def references; end

  sig { returns(T.nilable(Rubydex::Declaration)) }
  def target; end
end

class Rubydex::ClassVariable < Rubydex::Declaration
  sig { returns(T::Array[T.untyped]) }
  def references; end
end

class Rubydex::Method < Rubydex::Declaration
  sig { returns(T::Enumerable[Rubydex::MethodReference]) }
  def references; end
end

class Rubydex::Namespace < Rubydex::Declaration
  abstract!

  sig { returns(T::Enumerable[Rubydex::ConstantReference]) }
  def references; end

  sig { returns(T::Enumerable[Rubydex::Namespace]) }
  def ancestors; end

  sig { params(ancestor_names: String).returns(T::Boolean) }
  def has_ancestor?(*ancestor_names); end

  sig { returns(T::Enumerable[Rubydex::Namespace]) }
  def descendants; end

  sig { returns(T::Enumerable[Rubydex::Declaration]) }
  def members; end

  sig { params(name: String).returns(T.nilable(Rubydex::Declaration)) }
  def member(name); end

  sig { params(name: String, only_inherited: T::Boolean).returns(T.nilable(Rubydex::Declaration)) }
  def find_member(name, only_inherited: false); end

  sig { returns(T.nilable(Rubydex::SingletonClass)) }
  def singleton_class; end
end

class Rubydex::Class < Rubydex::Namespace; end
class Rubydex::Module < Rubydex::Namespace; end

class Rubydex::SingletonClass < Rubydex::Namespace
  sig { returns(Rubydex::Declaration) }
  def attached_class; end
end

class Rubydex::Definition
  abstract!

  sig { returns(T::Array[Rubydex::Comment]) }
  def comments; end

  sig { returns(T::Boolean) }
  def deprecated?; end

  sig { returns(Rubydex::Location) }
  def location; end

  sig { returns(String) }
  def name; end

  sig { returns(T.nilable(Rubydex::Location)) }
  def name_location; end

  sig { returns(T.nilable(Rubydex::Declaration)) }
  def declaration; end

  sig { returns(T.nilable(Rubydex::Definition)) }
  def lexical_owner; end

  sig { returns(T::Array[Rubydex::Definition]) }
  def lexical_nesting; end

  sig { returns(Rubydex::Document) }
  def document; end

  class << self
    private

    def new(*args); end
  end
end

class Rubydex::AttrAccessorDefinition < Rubydex::Definition; end
class Rubydex::AttrReaderDefinition < Rubydex::Definition; end
class Rubydex::AttrWriterDefinition < Rubydex::Definition; end
class Rubydex::ClassVariableDefinition < Rubydex::Definition; end

class Rubydex::ConstantAliasDefinition < Rubydex::Definition
  sig { returns(String) }
  def raw_name; end
end

class Rubydex::ConstantDefinition < Rubydex::Definition
  sig { returns(String) }
  def raw_name; end
end

class Rubydex::GlobalVariableAliasDefinition < Rubydex::Definition; end
class Rubydex::GlobalVariableDefinition < Rubydex::Definition; end
class Rubydex::InstanceVariableDefinition < Rubydex::Definition; end

class Rubydex::MethodAliasDefinition < Rubydex::Definition
  sig { returns(T::Array[Rubydex::Signature]) }
  def signatures; end

  sig { returns(T.nilable(Rubydex::Method)) }
  def target; end
end

class Rubydex::MethodDefinition < Rubydex::Definition
  sig { returns(T::Array[Rubydex::Signature]) }
  def signatures; end
end

class Rubydex::Signature
  sig { params(parameters: T::Array[Rubydex::Signature::Parameter]).void }
  def initialize(parameters); end

  sig { returns(T::Array[Rubydex::Signature::Parameter]) }
  def parameters; end

  sig do
    returns([
      T::Array[Rubydex::Signature::PositionalParameter],
      T::Array[Rubydex::Signature::OptionalPositionalParameter],
      T.nilable(Rubydex::Signature::RestPositionalParameter),
      T::Array[Rubydex::Signature::PostParameter],
      T::Array[Rubydex::Signature::KeywordParameter],
      T::Array[Rubydex::Signature::OptionalKeywordParameter],
      T.nilable(Rubydex::Signature::RestKeywordParameter),
      T.nilable(Rubydex::Signature::ForwardParameter),
      T.nilable(Rubydex::Signature::BlockParameter),
    ])
  end
  def deconstruct; end

  sig { params(keys: T.nilable(T::Array[Symbol])).returns(T::Hash[Symbol, T.untyped]) }
  def deconstruct_keys(keys); end

  sig { returns(T::Array[Rubydex::Signature::PositionalParameter]) }
  def positional_parameters; end

  sig { returns(T::Array[Rubydex::Signature::OptionalPositionalParameter]) }
  def optional_positional_parameters; end

  sig { returns(T.nilable(Rubydex::Signature::RestPositionalParameter)) }
  def rest_positional_parameter; end

  sig { returns(T::Array[Rubydex::Signature::PostParameter]) }
  def post_parameters; end

  sig { returns(T::Array[Rubydex::Signature::KeywordParameter]) }
  def keyword_parameters; end

  sig { returns(T::Array[Rubydex::Signature::OptionalKeywordParameter]) }
  def optional_keyword_parameters; end

  sig { returns(T.nilable(Rubydex::Signature::RestKeywordParameter)) }
  def rest_keyword_parameter; end

  sig { returns(T.nilable(Rubydex::Signature::ForwardParameter)) }
  def forward_parameter; end

  sig { returns(T.nilable(Rubydex::Signature::BlockParameter)) }
  def block_parameter; end
end

class Rubydex::Signature::Parameter
  sig { returns(Symbol) }
  def name; end

  sig { returns(Rubydex::Location) }
  def location; end

  sig { params(name: Symbol, location: Rubydex::Location).void }
  def initialize(name, location); end
end

class Rubydex::Signature::PositionalParameter < Rubydex::Signature::Parameter; end
class Rubydex::Signature::OptionalPositionalParameter < Rubydex::Signature::Parameter; end
class Rubydex::Signature::RestPositionalParameter < Rubydex::Signature::Parameter; end
class Rubydex::Signature::PostParameter < Rubydex::Signature::Parameter; end
class Rubydex::Signature::KeywordParameter < Rubydex::Signature::Parameter; end
class Rubydex::Signature::OptionalKeywordParameter < Rubydex::Signature::Parameter; end
class Rubydex::Signature::RestKeywordParameter < Rubydex::Signature::Parameter; end
class Rubydex::Signature::ForwardParameter < Rubydex::Signature::Parameter; end
class Rubydex::Signature::BlockParameter < Rubydex::Signature::Parameter; end

class Rubydex::ModuleDefinition < Rubydex::Definition
  sig { returns(T::Array[Rubydex::Mixin]) }
  def mixins; end

  sig { returns(String) }
  def raw_name; end
end

class Rubydex::SingletonClassDefinition < Rubydex::Definition
  sig { returns(T::Array[Rubydex::Mixin]) }
  def mixins; end
end

class Rubydex::ClassDefinition < Rubydex::Definition
  sig { returns(T.nilable(Rubydex::ConstantReference)) }
  def superclass; end

  sig { returns(T::Array[Rubydex::Mixin]) }
  def mixins; end

  sig { returns(String) }
  def raw_name; end
end

class Rubydex::Mixin
  abstract!

  sig { returns(Rubydex::ConstantReference) }
  attr_reader :constant_reference

  sig { params(constant_reference: Rubydex::ConstantReference).void }
  def initialize(constant_reference); end
end

class Rubydex::Include < Rubydex::Mixin; end
class Rubydex::Prepend < Rubydex::Mixin; end
class Rubydex::Extend < Rubydex::Mixin; end

module Rubydex::Severity
  # Every severity, ordered from the most to the least severe.
  ALL = T.let(T.unsafe(nil), T::Array[T.class_of(Rubydex::Severity::Base)])

  sig { params(value: Symbol).returns(T.class_of(Rubydex::Severity::Base)) }
  def self.from_value(value); end
end

class Rubydex::Severity::Base
  abstract!

  sig { returns(Symbol) }
  def self.value; end
end

class Rubydex::Severity::Error < Rubydex::Severity::Base; end
class Rubydex::Severity::Warning < Rubydex::Severity::Base; end
class Rubydex::Severity::Information < Rubydex::Severity::Base; end
class Rubydex::Severity::Hint < Rubydex::Severity::Base; end

class Rubydex::Rule
  abstract!

  class << self
    abstract!

    sig { returns(String) }
    def rule_name; end

    sig { abstract.returns(T.class_of(Rubydex::Severity::Base)) }
    def default_severity; end

    sig { params(config: Rubydex::LinterConfig).returns(T.class_of(Rubydex::Severity::Base)) }
    def severity(config); end
  end
end

module Rubydex::Rules
  ALL = T.let(T.unsafe(nil), T::Array[T.class_of(Rubydex::Rule)])
end

class Rubydex::Rules::ParseError < Rubydex::Rule
  class << self
    sig { override.returns(T.class_of(Rubydex::Severity::Base)) }
    def default_severity; end
  end
end

class Rubydex::Rules::ParseWarning < Rubydex::Rule
  class << self
    sig { override.returns(T.class_of(Rubydex::Severity::Base)) }
    def default_severity; end
  end
end

class Rubydex::Rules::DynamicConstantReference < Rubydex::Rule
  class << self
    sig { override.returns(T.class_of(Rubydex::Severity::Base)) }
    def default_severity; end
  end
end

class Rubydex::Rules::DynamicSingletonDefinition < Rubydex::Rule
  class << self
    sig { override.returns(T.class_of(Rubydex::Severity::Base)) }
    def default_severity; end
  end
end

class Rubydex::Rules::DynamicAncestor < Rubydex::Rule
  class << self
    sig { override.returns(T.class_of(Rubydex::Severity::Base)) }
    def default_severity; end
  end
end

class Rubydex::Rules::TopLevelMixinSelf < Rubydex::Rule
  class << self
    sig { override.returns(T.class_of(Rubydex::Severity::Base)) }
    def default_severity; end
  end
end

class Rubydex::Rules::InvalidConstantVisibility < Rubydex::Rule
  class << self
    sig { override.returns(T.class_of(Rubydex::Severity::Base)) }
    def default_severity; end
  end
end

class Rubydex::Rules::InvalidMethodVisibility < Rubydex::Rule
  class << self
    sig { override.returns(T.class_of(Rubydex::Severity::Base)) }
    def default_severity; end
  end
end

class Rubydex::Rules::UndefinedMethodVisibilityTarget < Rubydex::Rule
  class << self
    sig { override.returns(T.class_of(Rubydex::Severity::Base)) }
    def default_severity; end
  end
end

class Rubydex::Rules::UndefinedConstantVisibilityTarget < Rubydex::Rule
  class << self
    sig { override.returns(T.class_of(Rubydex::Severity::Base)) }
    def default_severity; end
  end
end

class Rubydex::RelatedInformation
  sig { params(message: String, location: Rubydex::Location).void }
  def initialize(message, location); end

  sig { returns(Rubydex::Location) }
  def location; end

  sig { returns(String) }
  def message; end
end

class Rubydex::Diagnostic
  sig do
    params(
      rule: T.class_of(Rubydex::Rule),
      message: String,
      location: Rubydex::Location,
      related_information: T::Array[Rubydex::RelatedInformation],
    ).void
  end
  def initialize(rule:, message:, location:, related_information: []); end

  sig { returns(Rubydex::Location) }
  def location; end

  sig { returns(String) }
  def message; end

  sig { returns(T.class_of(Rubydex::Rule)) }
  def rule; end

  sig { returns(T::Array[Rubydex::RelatedInformation]) }
  def related_information; end
end

module Rubydex::Linter; end
module Rubydex::Linter::Helpers; end
module Rubydex::Linter::Rules; end

module Rubydex::Linter::Helpers::PathHelpers
  extend T::Helpers

  requires_ancestor { Rubydex::Linter::CustomRule }

  EXCLUDE_FNMATCH_FLAGS = T.let(T.unsafe(nil), Integer)
  TEST_PATHS = T.let(T.unsafe(nil), T::Array[String])

  sig do
    params(
      path: String,
      patterns: T::Array[String],
      workspace: String,
      flags: Integer,
    ).returns(T::Boolean)
  end
  def self.path_matches_patterns?(path, patterns, workspace:, flags: 0); end

  sig { params(location: Rubydex::Location, workspace: String).returns(String) }
  def self.display_path(location, workspace:); end

  sig do
    params(
      definitions: T::Enumerable[Rubydex::Definition],
      excluded_patterns: T::Array[String],
    ).returns(T::Array[Rubydex::Definition])
  end
  def reject_definitions_in_paths(definitions, excluded_patterns); end

  sig do
    params(
      definitions: T::Enumerable[Rubydex::Definition],
      patterns: T::Array[String],
    ).returns(T::Array[Rubydex::Definition])
  end
  def select_definitions_in_paths(definitions, patterns); end

  private

  sig { params(path: String, patterns: T::Array[String]).returns(T::Boolean) }
  def path_matches_patterns?(path, patterns); end

  sig { params(path: String).returns(T::Boolean) }
  def test_path?(path); end

  sig { params(definition: Rubydex::Definition).returns(T.nilable(String)) }
  def path_for_definition(definition); end
end

module Rubydex::Linter::Helpers::SourceAccessHelpers
  sig { params(uri: String).returns(Rubydex::Location) }
  def file_location(uri); end

  sig { params(uri: String).returns(String) }
  def path_for_uri(uri); end

  private

  sig { params(location: Rubydex::Location).returns(T.nilable(String)) }
  def source_for_location(location); end
end

# Base class for semantic lint rules, which collect their diagnostics by walking a resolved graph.
class Rubydex::Linter::CustomRule < Rubydex::Rule
  abstract!

  sig { params(graph: Rubydex::Graph, config: Rubydex::LinterConfig).void }
  def initialize(graph, config:); end

  sig { returns(Rubydex::Graph) }
  def graph; end

  sig { returns(Rubydex::LinterConfig) }
  def config; end

  sig { returns(T::Array[Rubydex::Diagnostic]) }
  def diagnostics; end

  sig { params(definition: Rubydex::Definition).returns(Rubydex::Location) }
  def diagnostic_location(definition); end

  sig { params(base_name: String).returns(T::Enumerable[Rubydex::Class]) }
  def child_classes(base_name); end

  sig { params(name: String).returns(Rubydex::Namespace) }
  def required_namespace(name); end

  sig do
    params(
      namespace: Rubydex::Namespace,
      method_name: String,
    ).returns(Rubydex::Method)
  end
  def required_method(namespace, method_name); end

  sig { returns(String) }
  def rule_name; end

  sig { abstract.void }
  def lint; end

  protected

  sig do
    params(
      message: String,
      location: Rubydex::Location,
      related_information: T::Array[Rubydex::RelatedInformation],
    ).void
  end
  def add_diagnostic(message, location, related_information: []); end
end

class Rubydex::Linter::MissingGraphDependencyError < StandardError
  sig { params(rule_name: String, dependency: String).void }
  def initialize(rule_name, dependency); end
end

class Rubydex::Linter::RuleLoadError < StandardError; end

class Rubydex::Linter::RuleLoader
  RULE_GLOB = T.let(T.unsafe(nil), String)

  sig { params(workspace_path: String).void }
  def self.load(workspace_path); end
end

class Rubydex::Linter::Rules::RuleStructure < Rubydex::Linter::CustomRule
  include Rubydex::Linter::Helpers::SourceAccessHelpers

  BASE_RULE_NAME = T.let(T.unsafe(nil), String)
  RULE_NAMESPACE = T.let(T.unsafe(nil), String)
  RULE_FILE_PATTERNS = T.let(T.unsafe(nil), T::Array[String])
  TEST_FILE_PATTERNS = T.let(T.unsafe(nil), T::Array[String])

  class << self
    sig { override.returns(T.class_of(Rubydex::Severity::Base)) }
    def default_severity; end
  end

  sig { override.void }
  def lint; end
end

class Rubydex::Linter::RuleTestCase < Minitest::Test
  DEFAULT_FILE = T.let(T.unsafe(nil), String)
  ANNOTATION_PATTERN = T.let(T.unsafe(nil), Regexp)

  sig { returns(String) }
  def workspace_path; end

  sig { params(name: String).void }
  def initialize(name); end

  sig { void }
  def teardown; end

  sig { returns(T.class_of(Rubydex::Linter::CustomRule)) }
  def rule_class; end

  sig { params(sources: T::Hash[String, String]).void }
  def add_shared_source(sources); end

  sig { returns(T::Array[String]) }
  def ignored_diagnostic_files; end

  sig { returns(Rubydex::LinterConfig) }
  def rule_config; end

  sig do
    params(
      args: T.any(String, T::Hash[T.any(String, Symbol), String]),
      rule_builder: T.nilable(T.proc.params(graph: Rubydex::Graph).returns(Rubydex::Linter::CustomRule)),
    ).returns(T::Array[Rubydex::Diagnostic])
  end
  def assert_diagnostics(*args, &rule_builder); end

  sig do
    params(
      args: T.any(String, T::Hash[T.any(String, Symbol), String]),
      rule_builder: T.nilable(T.proc.params(graph: Rubydex::Graph).returns(Rubydex::Linter::CustomRule)),
    ).returns(T::Array[Rubydex::Diagnostic])
  end
  def assert_no_diagnostics(*args, &rule_builder); end

  sig do
    params(
      dependency: String,
      args: T.any(String, T::Hash[T.any(String, Symbol), String]),
      after_excluding: T::Array[String],
      rule_builder: T.nilable(T.proc.params(graph: Rubydex::Graph).returns(Rubydex::Linter::CustomRule)),
    ).void
  end
  def assert_handles_missing_required_dependency(dependency, *args, after_excluding: [], &rule_builder); end
end

class Rubydex::Linter::Runner
  sig do
    params(
      graph: Rubydex::Graph,
      custom_rules: T::Array[T.class_of(Rubydex::Linter::CustomRule)],
      config: Rubydex::LinterConfig,
    ).void
  end
  def initialize(graph, custom_rules:, config:); end

  sig { returns(Rubydex::Graph) }
  def graph; end

  sig { returns(T::Array[T.class_of(Rubydex::Linter::CustomRule)]) }
  def custom_rules; end

  sig { returns(T::Array[Rubydex::Diagnostic]) }
  def run; end
end

class Rubydex::Keyword
  sig { params(name: String, documentation: String).void }
  def initialize(name, documentation); end

  sig { returns(String) }
  def name; end

  sig { returns(String) }
  def documentation; end
end

class Rubydex::KeywordParameter
  sig { params(name: String).void }
  def initialize(name); end

  sig { returns(String) }
  def name; end
end

class Rubydex::Document
  sig { returns(T::Enumerable[Rubydex::Definition]) }
  def definitions; end

  sig { returns(T::Enumerable[Rubydex::MethodReference]) }
  def method_references; end

  sig { returns(String) }
  def uri; end

  class << self
    private

    def new(*args); end
  end
end

class Rubydex::Error < StandardError; end
class Rubydex::AliasCycleError < Rubydex::Error; end
class Rubydex::ConfigError < Rubydex::Error; end
class Rubydex::QueryError < Rubydex::Error; end
class Rubydex::QuerySyntaxError < Rubydex::QueryError; end
class Rubydex::QueryExecutionError < Rubydex::QueryError; end
class Rubydex::StaleQueryResultError < Rubydex::QueryError; end

# The configuration of a workspace, parsed from its `rubydex.toml`. It carries both the settings that are global to
# every built-in tool, such as the workspace being analyzed, and the typed settings of each tool's own section (e.g.
# `[graph]`, `[linter]`).
class Rubydex::Config
  class << self
    # Loads the configuration file for `workspace_path/rubydex.toml` if it exists or uses the defaults.
    sig { params(workspace_path: String).returns(Rubydex::Config) }
    def load(workspace_path); end
  end

  # The linter's settings, read from the `[linter]` section.
  sig { returns(Rubydex::LinterConfig) }
  def linter; end

  # The configured workspace path, which is usually PWD, except for editors that spawn language servers outside of pwd.
  sig { returns(String) }
  def workspace_path; end
end

# The linter's settings, read from the `[linter]` section of the configuration file.
class Rubydex::LinterConfig
  # The configured rules, keyed by rule name. Only rules the configuration file mentions appear here, so a rule that
  # was never configured is absent rather than present with its defaults.
  sig { returns(T::Hash[String, Rubydex::RuleConfig]) }
  attr_reader :rules

  sig { params(rules: T::Hash[String, Rubydex::RuleConfig]).void }
  def initialize(rules); end

  sig { params(rule_class: T.class_of(Rubydex::Rule)).returns(T::Boolean) }
  def rule_enabled?(rule_class); end

  sig { params(rule_class: T.class_of(Rubydex::Rule)).returns(T::Array[String]) }
  def excludes_for(rule_class); end

  sig { params(rule_class: T.class_of(Rubydex::Rule)).returns(T.nilable(T.class_of(Rubydex::Severity::Base))) }
  def severity_for(rule_class); end
end

# The settings of a single linter rule, read from a `[linter.rules.RuleName]` table.
class Rubydex::RuleConfig
  sig { returns(String) }
  attr_reader :name

  sig { returns(T::Array[String]) }
  attr_reader :exclude_patterns

  sig { returns(T.nilable(T.class_of(Rubydex::Severity::Base))) }
  attr_reader :severity

  sig do
    params(
      name: String,
      enabled: T::Boolean,
      exclude_patterns: T::Array[String],
      severity: T.nilable(T.class_of(Rubydex::Severity::Base)),
    ).void
  end
  def initialize(name, enabled, exclude_patterns = [], severity = nil); end

  sig { returns(T::Boolean) }
  def enabled?; end
end

class Rubydex::Failure
  sig { params(message: String).void }
  def initialize(message); end

  sig { returns(String) }
  def message; end
end

class Rubydex::IntegrityFailure < Rubydex::Failure; end

class Rubydex::Query
  class << self
    sig { params(query: String).returns(Rubydex::Query) }
    def parse(query); end

    sig { params(format: T.any(String, Symbol)).returns(String) }
    def schema(format = :table); end
  end

  sig { params(graph: Rubydex::Graph).returns(Rubydex::Query::Result) }
  def run(graph); end
end

# The columns and rows produced by one run of a query. Enumerable over its rows.
class Rubydex::Query::Result
  include Enumerable
  extend T::Generic

  Elem = type_member { { fixed: T::Hash[String, T.untyped] } }

  class << self
    private

    # A result only comes from `Query#run`. The C extension undefines `new`, so a call raises.
    sig { returns(T.noreturn) }
    def new; end
  end

  sig { returns(T::Array[String]) }
  def columns; end

  sig { returns(T::Array[T::Hash[String, T.untyped]]) }
  def rows; end

  sig { override.params(block: T.proc.params(row: T::Hash[String, T.untyped]).void).returns(T.self_type) }
  sig { override.returns(T::Enumerator[T::Hash[String, T.untyped]]) }
  def each(&block); end

  sig { returns(Integer) }
  def size; end

  sig { returns(Integer) }
  def length; end

  sig { returns(T::Boolean) }
  def empty?; end

  sig { params(format: T.any(String, Symbol)).returns(String) }
  def render(format = :table); end
end

class Rubydex::Graph
  class << self
    # Creates a new graph with the loaded configuration. For use cases where the graph must be shared between
    # different tools, do not use this. Create and own a `Config` object instead.
    sig { params(workspace_path: String).returns(T.attached_class) }
    def configure_for_workspace(workspace_path); end
  end

  sig { params(fully_qualified_name: String).returns(T.nilable(Rubydex::Declaration)) }
  def [](fully_qualified_name); end

  sig { returns(T::Enumerable[Rubydex::ConstantReference]) }
  def constant_references; end

  sig { returns(T::Enumerable[Rubydex::Declaration]) }
  def declarations; end

  sig { params(uri: String).returns(T.nilable(Rubydex::Document)) }
  def delete_document(uri); end

  sig { params(uri: String).returns(T.nilable(Rubydex::Document)) }
  def document(uri); end

  sig { returns(T::Array[Rubydex::Diagnostic]) }
  def diagnostics; end

  sig { returns(T::Enumerable[Rubydex::Document]) }
  def documents; end

  sig { params(file_paths: T::Array[String]).returns(T::Array[String]) }
  def index_all(file_paths); end

  sig { params(uri: String, source: String, language_id: String).void }
  def index_source(uri, source, language_id); end

  # Index all files and dependencies of the workspace that exists in `workspace_path`
  sig { returns(T::Array[String]) }
  def index_workspace; end

  # Returns the keyword object for the name, or `nil` if it is not a Ruby keyword
  sig { params(name: String).returns(T.nilable(Rubydex::Keyword)) }
  def keyword(name); end

  # Loads the configuration in the graph.
  sig { params(config: Rubydex::Config).void }
  def load_config(config); end

  sig { returns(T::Enumerable[Rubydex::MethodReference]) }
  def method_references; end

  sig { params(load_paths: T::Array[String]).returns(T::Array[String]) }
  def require_paths(load_paths); end

  sig { returns(T.self_type) }
  def resolve; end

  sig { params(name: String, nesting: T::Array[String]).returns(T.nilable(Rubydex::Declaration)) }
  def resolve_constant(name, nesting); end

  sig { params(require_path: String, load_paths: T::Array[String]).returns(T.nilable(Rubydex::Document)) }
  def resolve_require_path(require_path, load_paths); end

  sig { params(queries: String).returns(T::Enumerable[Rubydex::Declaration]) }
  def search(*queries); end

  sig { params(queries: String).returns(T::Enumerable[Rubydex::Declaration]) }
  def fuzzy_search(*queries); end

  sig { returns(T::Enumerable[Rubydex::Declaration]) }
  def dead_code_candidates; end

  sig { params(encoding: String).void }
  def encoding=(encoding); end

  # The root directory of the workspace being analyzed, which comes from the loaded `Rubydex::Config`
  sig { returns(String) }
  def workspace_path; end

  sig { returns(T::Array[String]) }
  def workspace_paths; end

  sig { returns(T::Array[Rubydex::Failure]) }
  def check_integrity; end

  # Returns completion candidates for an expression context. This includes all keywords, constants, methods, instance
  # variables, class variables and global variables reachable from the current lexical scope and self type.
  #
  # The nesting array represents the lexical scope stack. The required `self_receiver` keyword argument overrides the
  # self type independently of the lexical scope (e.g., `"Foo::<Foo>"` for `def Foo.bar`). This distinction is important
  # because constants and class variables are always attached to the lexical scope. Meanwhile, methods and instance
  # variables are attached to the type of `self` and those don't always match. Pass `nil` when the self type is unknown
  sig do
    params(
      nesting: T::Array[String],
      self_receiver: T.nilable(String),
    ).returns(T::Array[T.any(Rubydex::Declaration, Rubydex::Keyword)])
  end
  def complete_expression(nesting, self_receiver:); end

  # Returns completion candidates after a namespace access operator (e.g., `Foo::`). This includes all constants and
  # singleton methods for the namespace and its ancestors.
  #
  # The required `self_receiver` kwarg is the caller's runtime self type. It's used to filter visibility-restricted
  # singleton methods (e.g., `private_class_method`). Pass `nil` for top-level/script scope.
  sig do
    params(
      name: String,
      self_receiver: T.nilable(String),
    ).returns(T::Array[Rubydex::Declaration])
  end
  def complete_namespace_access(name, self_receiver:); end

  # Returns completion candidates after a method call operator (e.g., `foo.`). This includes all methods that exist on
  # the type of the receiver and its ancestors.
  #
  # The required `self_receiver` kwarg is the caller's runtime self type. It's used for visibility checks for `private`
  # and `protected` methods. Pass `nil` for top-level/script scope.
  sig do
    params(
      name: String,
      self_receiver: T.nilable(String),
    ).returns(T::Array[Rubydex::Method])
  end
  def complete_method_call(name, self_receiver:); end

  # Returns completion candidates inside a method call's argument list (e.g., `foo.bar(|)`). This includes everything
  # that expression completion provides plus keyword argument names of the method being called.
  #
  # See `complete_expression` for the semantics of `nesting` and `self_receiver` (required, may be `nil`).
  sig do
    params(
      name: String,
      nesting: T::Array[String],
      self_receiver: T.nilable(String),
    ).returns(T::Array[T.any(Rubydex::Declaration, Rubydex::Keyword, Rubydex::KeywordParameter)])
  end
  def complete_method_argument(name, nesting, self_receiver:); end

  private

  sig { params(paths: T::Array[String]).void }
  def add_core_rbs_definition_paths(paths); end

  sig { params(paths: T::Array[String]).void }
  def add_workspace_dependency_paths(paths); end

  sig { params(patterns: T::Array[String]).void }
  def exclude_patterns(patterns); end

  sig { returns(T::Array[String]) }
  def excluded_patterns; end
end

class Rubydex::DisplayLocation < Rubydex::Location
  class << self
    sig { params(prism_location: Prism::Location, uri: String).returns(T.noreturn) }
    def from_prism(prism_location, uri:); end
  end

  sig { returns([String, Integer, Integer, Integer, Integer]) }
  def comparable_values; end

  sig { returns(Rubydex::DisplayLocation) }
  def to_display; end

  sig { returns(String) }
  def to_s; end
end

class Rubydex::Location
  include ::Comparable

  class << self
    sig { params(prism_location: Prism::Location, uri: String).returns(Rubydex::Location) }
    def from_prism(prism_location, uri:); end
  end

  sig do
    params(
      uri: String,
      start_line: Integer,
      end_line: Integer,
      start_column: Integer,
      end_column: Integer,
    ).void
  end
  def initialize(uri:, start_line:, end_line:, start_column:, end_column:); end

  sig { params(other: T.untyped).returns(T.nilable(Integer)) }
  def <=>(other); end

  sig { returns([String, Integer, Integer, Integer, Integer]) }
  def comparable_values; end

  sig { returns(Integer) }
  def end_column; end

  sig { returns(Integer) }
  def end_line; end

  sig { returns(String) }
  def to_file_path; end

  sig { returns(Integer) }
  def start_column; end

  sig { returns(Integer) }
  def start_line; end

  sig { returns(Rubydex::DisplayLocation) }
  def to_display; end

  sig { returns(String) }
  def to_s; end

  sig { returns(String) }
  def uri; end

  class NotFileUriError < StandardError; end
end

class Rubydex::MethodReference < Rubydex::Reference
  sig { returns(Rubydex::Location) }
  def location; end

  sig { returns(String) }
  def name; end

  sig { returns(T.nilable(Rubydex::Declaration)) }
  def receiver; end

  sig { returns(Rubydex::Document) }
  def document; end
end

class Rubydex::Reference
  abstract!

  class << self
    private

    def new(*args); end
  end
end

Rubydex::VERSION = T.let(T.unsafe(nil), String)
