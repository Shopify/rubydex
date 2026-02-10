# frozen_string_literal: true
# typed: strict

module Rubydex
  class Comment
    sig { params(string: String, location: Location).void }
    def initialize(string:, location:); end

    sig { returns(Location) }
    def location; end

    sig { returns(String) }
    def string; end
  end

  class ConstantReference < Reference
    sig { returns(Location) }
    def location; end

    sig { returns(String) }
    def name; end
  end

  class Declaration
    sig { returns(T::Enumerable[Definition]) }
    def definitions; end

    sig { returns(String) }
    def name; end

    sig { returns(Declaration) }
    def owner; end

    sig { returns(String) }
    def unqualified_name; end

    class << self
      private

      def new(*args); end
    end
  end

  class GlobalVariable < Declaration; end
  class InstanceVariable < Declaration; end
  class Constant < Declaration; end
  class ConstantAlias < Declaration; end
  class ClassVariable < Declaration; end
  class Method < Declaration; end

  class Namespace < Declaration
    sig { returns(T::Enumerable[Namespace]) }
    def ancestors; end

    sig { returns(T::Enumerable[Namespace]) }
    def descendants; end

    sig { params(name: String).returns(T.nilable(Declaration)) }
    def member(name); end

    sig { returns(T.nilable(SingletonClass)) }
    def singleton_class; end
  end

  class Class < Namespace; end
  class Module < Namespace; end
  class SingletonClass < Namespace; end

  class Definition
    sig { returns(T::Array[Comment]) }
    def comments; end

    sig { returns(T::Boolean) }
    def deprecated?; end

    sig { returns(Location) }
    def location; end

    sig { returns(String) }
    def name; end

    sig { returns(T.nilable(Location)) }
    def name_location; end

    class << self
      private

      def new(*args); end
    end
  end

  class AttrAccessorDefinition < Definition; end
  class AttrReaderDefinition < Definition; end
  class AttrWriterDefinition < Definition; end
  class ClassDefinition < Definition; end
  class ClassVariableDefinition < Definition; end
  class ConstantAliasDefinition < Definition; end
  class ConstantDefinition < Definition; end
  class GlobalVariableAliasDefinition < Definition; end
  class GlobalVariableDefinition < Definition; end
  class InstanceVariableDefinition < Definition; end
  class MethodAliasDefinition < Definition; end
  class MethodDefinition < Definition; end
  class ModuleDefinition < Definition; end
  class SingletonClassDefinition < Definition; end

  class Diagnostic
    sig { params(rule: Symbol, message: String, location: Location).void }
    def initialize(rule:, message:, location:); end

    sig { returns(Location) }
    def location; end

    sig { returns(String) }
    def message; end

    sig { returns(Symbol) }
    def rule; end
  end

  class Document
    sig { returns(T::Enumerable[Definition]) }
    def definitions; end

    sig { returns(String) }
    def uri; end

    class << self
      private

      def new(*args); end
    end
  end

  class Error < StandardError; end

  # The global graph representing all declarations and their relationships for the workspace
  #
  # Note: this class is partially defined in C to integrate with the Rust backend
  class Graph
    IGNORED_DIRECTORIES = T.let(T.unsafe(nil), T::Array[String])

    sig { params(workspace_path: T.nilable(String)).void }
    def initialize(workspace_path: nil); end

    sig { params(fully_qualified_name: String).returns(T.nilable(Declaration)) }
    def [](fully_qualified_name); end

    sig { returns(T::Enumerable[ConstantReference]) }
    def constant_references; end

    sig { returns(T::Enumerable[Declaration]) }
    def declarations; end

    sig { params(uri: String).returns(T.nilable(Document)) }
    def delete_document(uri); end

    sig { returns(T::Array[Diagnostic]) }
    def diagnostics; end

    sig { returns(T::Enumerable[Document]) }
    def documents; end

    sig { params(file_paths: T::Array[String]).returns(T::Array[String]) }
    def index_all(file_paths); end

    sig { params(uri: String, source: String, language_id: String).void }
    def index_source(uri, source, language_id); end

    # Index all files and dependencies of the workspace that exists in `@workspace_path`
    sig { returns(T::Array[String]) }
    def index_workspace; end

    sig { returns(T::Enumerable[MethodReference]) }
    def method_references; end

    sig { params(load_paths: T::Array[String]).returns(T::Array[String]) }
    def require_paths(load_paths); end

    sig { returns(T.self_type) }
    def resolve; end

    sig { params(name: String, nesting: T::Array[String]).returns(T.nilable(Declaration)) }
    def resolve_constant(name, nesting); end

    sig { params(require_path: String, load_paths: T::Array[String]).returns(T.nilable(Document)) }
    def resolve_require_path(require_path, load_paths); end

    sig { params(query: String).returns(T::Enumerable[Declaration]) }
    def search(query); end

    sig { params(encoding: String).void }
    def encoding=(encoding); end

    sig { returns(T::Array[String]) }
    def workspace_paths; end

    private

    # Gathers the paths we have to index for all workspace dependencies
    sig { params(paths: T::Array[String]).void }
    def add_workspace_dependency_paths(paths); end
  end

  class Location
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

    sig { returns(Integer) }
    def end_column; end

    sig { returns(Integer) }
    def end_line; end

    sig { returns(String) }
    def path; end

    sig { returns(Integer) }
    def start_column; end

    sig { returns(Integer) }
    def start_line; end

    sig { returns(String) }
    def to_s; end

    sig { returns(String) }
    def uri; end
  end

  class MethodReference < Reference
    sig { returns(Location) }
    def location; end

    sig { returns(String) }
    def name; end
  end

  class Reference
    class << self
      private

      def new(*args); end
    end
  end

  VERSION = T.let(T.unsafe(nil), String)
end
