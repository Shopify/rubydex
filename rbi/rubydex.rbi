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
  sig { returns(Rubydex::Location) }
  def location; end

  sig { returns(String) }
  def name; end
end

class Rubydex::Declaration
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

class Rubydex::GlobalVariable < Rubydex::Declaration; end
class Rubydex::InstanceVariable < Rubydex::Declaration; end
class Rubydex::Constant < Rubydex::Declaration; end
class Rubydex::ConstantAlias < Rubydex::Declaration; end
class Rubydex::ClassVariable < Rubydex::Declaration; end
class Rubydex::Method < Rubydex::Declaration; end

class Rubydex::Namespace < Rubydex::Declaration
  sig { returns(T::Enumerable[Rubydex::Namespace]) }
  def ancestors; end

  sig { returns(T::Enumerable[Rubydex::Namespace]) }
  def descendants; end

  sig { params(name: String).returns(T.nilable(Rubydex::Declaration)) }
  def member(name); end

  sig { params(name: String, only_inherited: T::Boolean).returns(T.nilable(Rubydex::Declaration)) }
  def find_member(name, only_inherited: false); end

  sig { returns(T.nilable(Rubydex::SingletonClass)) }
  def singleton_class; end
end

class Rubydex::Class < Rubydex::Namespace; end
class Rubydex::Module < Rubydex::Namespace; end
class Rubydex::SingletonClass < Rubydex::Namespace; end

class Rubydex::Definition
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

  class << self
    private

    def new(*args); end
  end
end

class Rubydex::AttrAccessorDefinition < Rubydex::Definition; end
class Rubydex::AttrReaderDefinition < Rubydex::Definition; end
class Rubydex::AttrWriterDefinition < Rubydex::Definition; end
class Rubydex::ClassDefinition < Rubydex::Definition; end
class Rubydex::ClassVariableDefinition < Rubydex::Definition; end
class Rubydex::ConstantAliasDefinition < Rubydex::Definition; end
class Rubydex::ConstantDefinition < Rubydex::Definition; end
class Rubydex::GlobalVariableAliasDefinition < Rubydex::Definition; end
class Rubydex::GlobalVariableDefinition < Rubydex::Definition; end
class Rubydex::InstanceVariableDefinition < Rubydex::Definition; end
class Rubydex::MethodAliasDefinition < Rubydex::Definition; end
class Rubydex::MethodDefinition < Rubydex::Definition; end
class Rubydex::ModuleDefinition < Rubydex::Definition; end
class Rubydex::SingletonClassDefinition < Rubydex::Definition; end

class Rubydex::Diagnostic
  sig { params(rule: Symbol, message: String, location: Rubydex::Location).void }
  def initialize(rule:, message:, location:); end

  sig { returns(Rubydex::Location) }
  def location; end

  sig { returns(String) }
  def message; end

  sig { returns(Symbol) }
  def rule; end
end

class Rubydex::Document
  sig { returns(T::Enumerable[Rubydex::Definition]) }
  def definitions; end

  sig { returns(String) }
  def uri; end

  class << self
    private

    def new(*args); end
  end
end

class Rubydex::Error < StandardError; end

class Rubydex::Failure
  sig { params(message: String).void }
  def initialize(message); end

  sig { returns(String) }
  def message; end
end

class Rubydex::IntegrityFailure < Rubydex::Failure; end

class Rubydex::Graph
  IGNORED_DIRECTORIES = T.let(T.unsafe(nil), T::Array[String])

  sig { params(workspace_path: T.nilable(String)).void }
  def initialize(workspace_path: nil); end

  sig { params(fully_qualified_name: String).returns(T.nilable(Rubydex::Declaration)) }
  def [](fully_qualified_name); end

  sig { returns(T::Enumerable[Rubydex::ConstantReference]) }
  def constant_references; end

  sig { returns(T::Enumerable[Rubydex::Declaration]) }
  def declarations; end

  sig { params(uri: String).returns(T.nilable(Rubydex::Document)) }
  def delete_document(uri); end

  sig { returns(T::Array[Rubydex::Diagnostic]) }
  def diagnostics; end

  sig { returns(T::Enumerable[Rubydex::Document]) }
  def documents; end

  sig { params(file_paths: T::Array[String]).returns(T::Array[String]) }
  def index_all(file_paths); end

  sig { params(uri: String, source: String, language_id: String).void }
  def index_source(uri, source, language_id); end

  # Index all files and dependencies of the workspace that exists in `@workspace_path`
  sig { returns(T::Array[String]) }
  def index_workspace; end

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

  sig { params(query: String).returns(T::Enumerable[Rubydex::Declaration]) }
  def search(query); end

  sig { params(encoding: String).void }
  def encoding=(encoding); end

  sig { returns(String) }
  def workspace_path; end

  sig { params(workspace_path: String).returns(String) }
  def workspace_path=(workspace_path); end

  sig { returns(T::Array[String]) }
  def workspace_paths; end

  sig { returns(T::Array[Rubydex::Failure]) }
  def check_integrity; end

  private

  sig { params(paths: T::Array[String]).void }
  def add_core_rbs_definition_paths(paths); end

  sig { params(paths: T::Array[String]).void }
  def add_workspace_dependency_paths(paths); end
end

class Rubydex::DisplayLocation < Rubydex::Location
  sig { returns([String, Integer, Integer, Integer, Integer]) }
  def comparable_values; end

  sig { returns(Rubydex::DisplayLocation) }
  def to_display; end

  sig { returns(String) }
  def to_s; end
end

class Rubydex::Location
  include ::Comparable

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
end

class Rubydex::Reference
  class << self
    private

    def new(*args); end
  end
end

Rubydex::VERSION = T.let(T.unsafe(nil), String)
