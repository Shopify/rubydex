//! Operations represent the ordered actions extracted from Ruby/RBS source code.
//!
//! Unlike definitions (which are unordered and declarative), operations are ordered and imperative.
//! They model what Ruby actually does at execution time: define a class, define a method, change
//! visibility, include a module, etc.
//!
//! Each operation is self-contained: it carries enough context (`name_id`, etc.) to know what it
//! defines, while scope context is provided by surrounding Enter/Exit scope operations.
//!
//! The builder produces a `Vec<Operation>` for each file. These operations are then applied in order
//! by the applier to produce definitions and declarations in the graph.
//!
//! # Example
//!
//! ```ruby
//! class Foo
//!   def bar; end
//!   private :bar
//! end
//! ```
//!
//! Produces the operations:
//! 1. `EnterClass` (name: Foo)
//! 2.   `EnterMethod` (name: bar)
//! 3.   `ExitScope` # exit method bar
//! 4.   `SetMethodVisibility` (name: bar, visibility: private)
//! 5. `ExitScope` # exit class Foo

pub mod applier;
pub mod printer;
pub mod ruby_builder;

use crate::model::{
    comment::Comment,
    definitions::{DefinitionFlags, Signatures},
    ids::{NameId, StringId, UriId},
    visibility::Visibility,
};
use crate::offset::Offset;

/// An ordered instruction extracted from Ruby/RBS source code.
///
/// Operations are produced by the builder in the order they appear in the source file.
/// Scope context is established by Enter/Exit operations rather than carried on each variant.
#[derive(Debug)]
pub enum Operation {
    /// Enter a class scope (`class Foo` or `Class.new`).
    EnterClass(EnterClass),
    /// Enter a module scope (`module Foo` or `Module.new`).
    EnterModule(EnterModule),
    /// Enter a singleton class scope (`class << self` or `class << Foo`).
    EnterSingletonClass(EnterSingletonClass),
    /// Enter a method scope (`def foo` or `def self.foo`).
    EnterMethod(EnterMethod),
    /// Exit the current scope (class, module, singleton class, or method).
    ExitScope,
    /// Alias a method (`alias new_name old_name` or `alias_method :new, :old`).
    AliasMethod(AliasMethod),
    /// Change visibility of a specific method (`private :foo`).
    SetMethodVisibility(SetMethodVisibility),
    /// Change the default visibility for subsequent method definitions (`private` with no args).
    SetDefaultVisibility(SetDefaultVisibility),
    /// Define a constant (`FOO = 1`).
    DefineConstant(DefineConstant),
    /// Define a constant alias (`ALIAS = OtherConstant`).
    AliasConstant(AliasConstant),
    /// Change visibility of constants (`private_constant :FOO` or `public_constant :BAR`).
    SetConstantVisibility(SetConstantVisibility),
    /// Include, prepend, or extend a module.
    Mixin(Mixin),
    /// Define an attribute (`attr_accessor :foo`, `attr_reader :bar`, `attr_writer :baz`).
    DefineAttribute(DefineAttribute),
    /// Define a global variable (`$foo = 1`).
    DefineGlobalVariable(DefineGlobalVariable),
    /// Define an instance variable (`@foo = 1`).
    DefineInstanceVariable(DefineInstanceVariable),
    /// Define a class variable (`@@foo = 1`).
    DefineClassVariable(DefineClassVariable),
    /// Alias a global variable (`alias $new $old`).
    AliasGlobalVariable(AliasGlobalVariable),
    /// Record a reference to a constant (for tracking usages).
    ReferenceConstant(ReferenceConstant),
    /// Record a reference to a method (for tracking usages).
    ReferenceMethod(ReferenceMethod),
    /// Record a reference to an instance variable (`@foo` read or operator-write).
    ReferenceInstanceVariable(ReferenceInstanceVariable),
}

/// A resolved target as it appears in source code.
///
/// Used for method receivers (`def self.foo`), mixin targets (`include Foo`),
/// constant visibility (`Foo.private_constant`), and method references (`Foo.bar`).
#[derive(Debug, Clone, Copy)]
pub enum Target {
    /// Explicit `self` (e.g. `def self.foo`, `extend self`).
    ExplicitSelf,
    /// A constant name (e.g. `def Foo.foo`, `include Foo`).
    Constant(NameId),
    /// An expression we don't resolve (e.g. `def expr.foo`).
    Other,
}

/// The kind of attribute definition.
#[derive(Debug, Clone, Copy)]
pub enum AttrKind {
    Accessor,
    Reader,
    Writer,
}

/// The kind of mixin operation.
#[derive(Debug, Clone, Copy)]
pub enum MixinKind {
    Include,
    Prepend,
    Extend,
}

#[derive(Debug)]
pub struct EnterClass {
    pub name_id: NameId,
    pub uri_id: UriId,
    pub offset: Offset,
    pub name_offset: Offset,
    pub comments: Box<[Comment]>,
    pub flags: DefinitionFlags,
    pub superclass_name: Option<NameId>,
    pub is_lexical_scope: bool,
}

#[derive(Debug)]
pub struct EnterModule {
    pub name_id: NameId,
    pub uri_id: UriId,
    pub offset: Offset,
    pub name_offset: Offset,
    pub comments: Box<[Comment]>,
    pub flags: DefinitionFlags,
    pub is_lexical_scope: bool,
}

#[derive(Debug)]
pub struct EnterSingletonClass {
    pub name_id: NameId,
    pub uri_id: UriId,
    pub offset: Offset,
    pub name_offset: Offset,
    pub comments: Box<[Comment]>,
    pub flags: DefinitionFlags,
}

#[derive(Debug)]
pub struct EnterMethod {
    pub str_id: StringId,
    pub uri_id: UriId,
    pub offset: Offset,
    pub name_offset: Offset,
    pub comments: Box<[Comment]>,
    pub flags: DefinitionFlags,
    pub signatures: Signatures,
    pub receiver: Option<Target>,
}

#[derive(Debug)]
pub struct AliasMethod {
    pub new_name_str_id: StringId,
    pub old_name_str_id: StringId,
    pub uri_id: UriId,
    pub offset: Offset,
    pub comments: Box<[Comment]>,
    pub flags: DefinitionFlags,
    pub receiver: Option<Target>,
}

#[derive(Debug)]
pub struct SetMethodVisibility {
    pub str_id: StringId,
    pub visibility: Visibility,
    pub uri_id: UriId,
    pub offset: Offset,
    pub flags: DefinitionFlags,
}

#[derive(Debug)]
pub struct SetDefaultVisibility {
    pub visibility: Visibility,
    pub uri_id: UriId,
    pub offset: Offset,
}

#[derive(Debug)]
pub struct DefineConstant {
    pub name_id: NameId,
    pub uri_id: UriId,
    pub offset: Offset,
    pub comments: Box<[Comment]>,
    pub flags: DefinitionFlags,
}

#[derive(Debug)]
pub struct AliasConstant {
    pub name_id: NameId,
    pub target_name_id: NameId,
    pub uri_id: UriId,
    pub offset: Offset,
    pub comments: Box<[Comment]>,
    pub flags: DefinitionFlags,
}

#[derive(Debug)]
pub struct SetConstantVisibility {
    pub receiver: Option<Target>,
    pub target: StringId,
    pub visibility: Visibility,
    pub uri_id: UriId,
    pub offset: Offset,
    pub comments: Box<[Comment]>,
    pub flags: DefinitionFlags,
}

#[derive(Debug)]
pub struct Mixin {
    pub kind: MixinKind,
    pub target: Target,
}

#[derive(Debug)]
pub struct DefineAttribute {
    pub kind: AttrKind,
    pub str_id: StringId,
    pub uri_id: UriId,
    pub offset: Offset,
    pub comments: Box<[Comment]>,
    pub flags: DefinitionFlags,
}

#[derive(Debug)]
pub struct DefineGlobalVariable {
    pub str_id: StringId,
    pub uri_id: UriId,
    pub offset: Offset,
    pub comments: Box<[Comment]>,
    pub flags: DefinitionFlags,
}

#[derive(Debug)]
pub struct DefineInstanceVariable {
    pub str_id: StringId,
    pub uri_id: UriId,
    pub offset: Offset,
    pub comments: Box<[Comment]>,
    pub flags: DefinitionFlags,
}

#[derive(Debug)]
pub struct DefineClassVariable {
    pub str_id: StringId,
    pub uri_id: UriId,
    pub offset: Offset,
    pub comments: Box<[Comment]>,
    pub flags: DefinitionFlags,
}

#[derive(Debug)]
pub struct AliasGlobalVariable {
    pub new_name_str_id: StringId,
    pub old_name_str_id: StringId,
    pub uri_id: UriId,
    pub offset: Offset,
    pub comments: Box<[Comment]>,
    pub flags: DefinitionFlags,
}

#[derive(Debug)]
pub struct ReferenceConstant {
    pub name_id: NameId,
    pub uri_id: UriId,
    pub offset: Offset,
}

#[derive(Debug)]
pub struct ReferenceMethod {
    pub str_id: StringId,
    pub uri_id: UriId,
    pub offset: Offset,
    pub receiver: Option<Target>,
}

#[derive(Debug)]
pub struct ReferenceInstanceVariable {
    pub str_id: StringId,
    pub uri_id: UriId,
    pub offset: Offset,
}
