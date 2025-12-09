//! The definitions of the Ruby constructs in the source code.
//!
//! All the definitions constitute the `Declaration` of a name.
//!
//! Consider the following example:
//!
//! ```ruby
//! module Foo
//!   class Bar; end
//! end
//!
//! class Foo::Bar; end
//! ```
//!
//! There are 3 definitions:
//!
//! 1. The module definition for `Foo`
//! 2. The class definition for `Foo::Bar` inside `Foo`
//! 3. The class definition for `Foo::Bar` again
//!
//! And there are 2 declarations:
//!
//! 1. The declaration for the name `Foo`
//! 2. The declaration for the name `Foo::Bar`

use std::ops::Deref;

use crate::{
    model::{
        comment::Comment,
        ids::{DefinitionId, NameId, StringId, UriId},
        visibility::Visibility,
    },
    offset::Offset,
};

#[derive(Debug)]
pub enum Definition {
    Class(Box<ClassDefinition>),
    SingletonClass(Box<SingletonClassDefinition>),
    Module(Box<ModuleDefinition>),
    Constant(Box<ConstantDefinition>),
    Method(Box<MethodDefinition>),
    AttrAccessor(Box<AttrAccessorDefinition>),
    AttrReader(Box<AttrReaderDefinition>),
    AttrWriter(Box<AttrWriterDefinition>),
    GlobalVariable(Box<GlobalVariableDefinition>),
    InstanceVariable(Box<InstanceVariableDefinition>),
    ClassVariable(Box<ClassVariableDefinition>),
}

macro_rules! all_definitions {
    ($value:expr, $var:ident => $expr:expr) => {
        match $value {
            Definition::Class($var) => $expr,
            Definition::SingletonClass($var) => $expr,
            Definition::Module($var) => $expr,
            Definition::Constant($var) => $expr,
            Definition::GlobalVariable($var) => $expr,
            Definition::InstanceVariable($var) => $expr,
            Definition::ClassVariable($var) => $expr,
            Definition::AttrAccessor($var) => $expr,
            Definition::AttrReader($var) => $expr,
            Definition::AttrWriter($var) => $expr,
            Definition::Method($var) => $expr,
        }
    };
}

impl Definition {
    #[must_use]
    pub fn id(&self) -> DefinitionId {
        all_definitions!(self, it => it.id())
    }

    #[must_use]
    pub fn uri_id(&self) -> &UriId {
        all_definitions!(self, it => &it.uri_id())
    }

    #[must_use]
    pub fn offset(&self) -> &Offset {
        all_definitions!(self, it => &it.offset())
    }

    #[must_use]
    pub fn comments(&self) -> &Vec<Comment> {
        all_definitions!(self, it => &it.comments)
    }

    #[must_use]
    pub fn lexical_nesting_id(&self) -> &Option<DefinitionId> {
        all_definitions!(self, it => &it.lexical_nesting_id())
    }

    #[must_use]
    pub fn kind(&self) -> &'static str {
        match self {
            Definition::Class(_) => "Class",
            Definition::SingletonClass(_) => "SingletonClass",
            Definition::Module(_) => "Module",
            Definition::Constant(_) => "Constant",
            Definition::Method(_) => "Method",
            Definition::AttrAccessor(_) => "AttrAccessor",
            Definition::AttrReader(_) => "AttrReader",
            Definition::AttrWriter(_) => "AttrWriter",
            Definition::GlobalVariable(_) => "GlobalVariable",
            Definition::InstanceVariable(_) => "InstanceVariable",
            Definition::ClassVariable(_) => "ClassVariable",
        }
    }
}

/// Represents a mixin: include, prepend, or extend.
/// During resolution, `Extend` mixins are attached to the singleton class.
#[derive(Debug)]
pub enum Mixin {
    Include(NameId),
    Prepend(NameId),
    Extend(NameId),
}

// Deref implementation to conveniently extract the reference ID with a dereference
impl Deref for Mixin {
    type Target = NameId;

    fn deref(&self) -> &Self::Target {
        match self {
            Mixin::Include(id) | Mixin::Prepend(id) | Mixin::Extend(id) => id,
        }
    }
}

/// A class definition
///
/// # Example
/// ```ruby
/// class Foo
/// end
/// ```
#[derive(Debug)]
pub struct ClassDefinition {
    name_id: NameId,
    uri_id: UriId,
    offset: Offset,
    comments: Vec<Comment>,
    lexical_nesting_id: Option<DefinitionId>,
    members: Vec<DefinitionId>,
    superclass_ref: Option<NameId>,
    mixins: Vec<Mixin>,
}

/// A singleton class definition created from `class << X` syntax.
/// This is created only when `class << X` syntax is encountered, NOT for `def self.foo`.
/// Methods with receivers (like `def self.foo`) have their `receiver` field set instead.
///
/// # Examples
/// ```ruby
/// class Foo
///   class << self     # attached_target = NameId("Foo")
///     def bar; end
///   end
/// end
///
/// class << Foo        # attached_target = NameId("Foo")
///   def baz; end
/// end
/// ```
#[derive(Debug)]
pub struct SingletonClassDefinition {
    /// The name of this singleton class (e.g., `<Foo>` for `class << self` inside `class Foo`)
    name_id: NameId,
    uri_id: UriId,
    offset: Offset,
    comments: Vec<Comment>,
    /// The definition where `class << X` was found (lexical owner)
    lexical_nesting_id: Option<DefinitionId>,
    /// Members defined directly in this singleton class
    members: Vec<DefinitionId>,
    /// Mixins declared in this singleton class
    mixins: Vec<Mixin>,
}

impl SingletonClassDefinition {
    #[must_use]
    pub const fn new(
        name_id: NameId,
        uri_id: UriId,
        offset: Offset,
        comments: Vec<Comment>,
        lexical_nesting_id: Option<DefinitionId>,
    ) -> Self {
        Self {
            name_id,
            uri_id,
            offset,
            comments,
            lexical_nesting_id,
            members: Vec::new(),
            mixins: Vec::new(),
        }
    }

    #[must_use]
    pub fn id(&self) -> DefinitionId {
        DefinitionId::from(&format!("{}{}{}", *self.uri_id, self.offset.start(), *self.name_id))
    }

    #[must_use]
    pub fn name_id(&self) -> &NameId {
        &self.name_id
    }

    #[must_use]
    pub fn uri_id(&self) -> &UriId {
        &self.uri_id
    }

    #[must_use]
    pub fn offset(&self) -> &Offset {
        &self.offset
    }

    #[must_use]
    pub fn comments(&self) -> &[Comment] {
        &self.comments
    }

    #[must_use]
    pub fn lexical_nesting_id(&self) -> &Option<DefinitionId> {
        &self.lexical_nesting_id
    }

    #[must_use]
    pub fn members(&self) -> &[DefinitionId] {
        &self.members
    }

    #[must_use]
    pub fn mixins(&self) -> &[Mixin] {
        &self.mixins
    }

    pub fn add_member(&mut self, member_id: DefinitionId) {
        self.members.push(member_id);
    }

    pub fn add_mixin(&mut self, mixin: Mixin) {
        self.mixins.push(mixin);
    }
}

impl ClassDefinition {
    #[must_use]
    pub const fn new(
        name_id: NameId,
        uri_id: UriId,
        offset: Offset,
        comments: Vec<Comment>,
        lexical_nesting_id: Option<DefinitionId>,
        superclass_ref: Option<NameId>,
    ) -> Self {
        Self {
            name_id,
            uri_id,
            offset,
            comments,
            lexical_nesting_id,
            superclass_ref,
            members: Vec::new(),
            mixins: Vec::new(),
        }
    }

    #[must_use]
    pub fn id(&self) -> DefinitionId {
        DefinitionId::from(&format!("{}{}{}", *self.uri_id, self.offset.start(), *self.name_id))
    }

    #[must_use]
    pub fn name_id(&self) -> &NameId {
        &self.name_id
    }

    #[must_use]
    pub fn uri_id(&self) -> &UriId {
        &self.uri_id
    }

    #[must_use]
    pub fn offset(&self) -> &Offset {
        &self.offset
    }

    #[must_use]
    pub fn comments(&self) -> &[Comment] {
        &self.comments
    }

    #[must_use]
    pub fn lexical_nesting_id(&self) -> &Option<DefinitionId> {
        &self.lexical_nesting_id
    }

    #[must_use]
    pub fn superclass_ref(&self) -> &Option<NameId> {
        &self.superclass_ref
    }

    #[must_use]
    pub fn members(&self) -> &[DefinitionId] {
        &self.members
    }

    pub fn add_member(&mut self, member_id: DefinitionId) {
        self.members.push(member_id);
    }

    #[must_use]
    pub fn mixins(&self) -> &[Mixin] {
        &self.mixins
    }

    pub fn add_mixin(&mut self, mixin: Mixin) {
        self.mixins.push(mixin);
    }
}

/// A module definition
///
/// # Example
/// ```ruby
/// module Foo
/// end
/// ```
#[derive(Debug)]
pub struct ModuleDefinition {
    name_id: NameId,
    uri_id: UriId,
    offset: Offset,
    comments: Vec<Comment>,
    lexical_nesting_id: Option<DefinitionId>,
    members: Vec<DefinitionId>,
    mixins: Vec<Mixin>,
}

impl ModuleDefinition {
    #[must_use]
    pub const fn new(
        name_id: NameId,
        uri_id: UriId,
        offset: Offset,
        comments: Vec<Comment>,
        lexical_nesting_id: Option<DefinitionId>,
    ) -> Self {
        Self {
            name_id,
            uri_id,
            offset,
            comments,
            lexical_nesting_id,
            members: Vec::new(),
            mixins: Vec::new(),
        }
    }

    #[must_use]
    pub fn id(&self) -> DefinitionId {
        DefinitionId::from(&format!("{}{}{}", *self.uri_id, self.offset.start(), *self.name_id))
    }

    #[must_use]
    pub fn name_id(&self) -> &NameId {
        &self.name_id
    }

    #[must_use]
    pub fn uri_id(&self) -> &UriId {
        &self.uri_id
    }

    #[must_use]
    pub fn offset(&self) -> &Offset {
        &self.offset
    }

    #[must_use]
    pub fn comments(&self) -> &[Comment] {
        &self.comments
    }

    #[must_use]
    pub fn lexical_nesting_id(&self) -> &Option<DefinitionId> {
        &self.lexical_nesting_id
    }

    #[must_use]
    pub fn members(&self) -> &[DefinitionId] {
        &self.members
    }

    pub fn add_member(&mut self, member_id: DefinitionId) {
        self.members.push(member_id);
    }

    #[must_use]
    pub fn mixins(&self) -> &[Mixin] {
        &self.mixins
    }

    pub fn add_mixin(&mut self, mixin: Mixin) {
        self.mixins.push(mixin);
    }
}

/// A constant definition
///
/// # Example
/// ```ruby
/// FOO = 1
/// ```
#[derive(Debug)]
pub struct ConstantDefinition {
    name_id: NameId,
    uri_id: UriId,
    offset: Offset,
    comments: Vec<Comment>,
    lexical_nesting_id: Option<DefinitionId>,
}

impl ConstantDefinition {
    #[must_use]
    pub const fn new(
        name_id: NameId,
        uri_id: UriId,
        offset: Offset,
        comments: Vec<Comment>,
        lexical_nesting_id: Option<DefinitionId>,
    ) -> Self {
        Self {
            name_id,
            uri_id,
            offset,
            comments,
            lexical_nesting_id,
        }
    }

    #[must_use]
    pub fn id(&self) -> DefinitionId {
        DefinitionId::from(&format!("{}{}{}", *self.uri_id, self.offset.start(), *self.name_id))
    }

    #[must_use]
    pub fn name_id(&self) -> &NameId {
        &self.name_id
    }

    #[must_use]
    pub fn uri_id(&self) -> &UriId {
        &self.uri_id
    }

    #[must_use]
    pub fn offset(&self) -> &Offset {
        &self.offset
    }

    #[must_use]
    pub fn comments(&self) -> &[Comment] {
        &self.comments
    }

    #[must_use]
    pub fn lexical_nesting_id(&self) -> &Option<DefinitionId> {
        &self.lexical_nesting_id
    }
}

/// A method definition
///
/// # Example
/// ```ruby
/// def foo(bar, baz)
/// end
/// ```
#[derive(Debug)]
pub struct MethodDefinition {
    str_id: StringId,
    uri_id: UriId,
    offset: Offset,
    comments: Vec<Comment>,
    lexical_nesting_id: Option<DefinitionId>,
    parameters: Vec<Parameter>,
    visibility: Visibility,
    receiver: Option<NameId>,
}

impl MethodDefinition {
    #[allow(clippy::too_many_arguments)]
    #[must_use]
    pub const fn new(
        str_id: StringId,
        uri_id: UriId,
        offset: Offset,
        comments: Vec<Comment>,
        lexical_nesting_id: Option<DefinitionId>,
        parameters: Vec<Parameter>,
        visibility: Visibility,
        receiver: Option<NameId>,
    ) -> Self {
        Self {
            str_id,
            uri_id,
            offset,
            comments,
            lexical_nesting_id,
            parameters,
            visibility,
            receiver,
        }
    }

    #[must_use]
    pub fn id(&self) -> DefinitionId {
        DefinitionId::from(&format!("{}{}{}", *self.uri_id, self.offset.start(), *self.str_id))
    }

    #[must_use]
    pub fn str_id(&self) -> &StringId {
        &self.str_id
    }

    #[must_use]
    pub fn uri_id(&self) -> &UriId {
        &self.uri_id
    }

    #[must_use]
    pub fn offset(&self) -> &Offset {
        &self.offset
    }

    #[must_use]
    pub fn comments(&self) -> &[Comment] {
        &self.comments
    }

    #[must_use]
    pub fn lexical_nesting_id(&self) -> &Option<DefinitionId> {
        &self.lexical_nesting_id
    }

    #[must_use]
    pub fn parameters(&self) -> &Vec<Parameter> {
        &self.parameters
    }

    #[must_use]
    pub fn receiver(&self) -> &Option<NameId> {
        &self.receiver
    }

    #[must_use]
    pub fn visibility(&self) -> &Visibility {
        &self.visibility
    }
}

#[derive(Debug)]
pub enum Parameter {
    RequiredPositional(ParameterStruct),
    OptionalPositional(ParameterStruct),
    RestPositional(ParameterStruct),
    Post(ParameterStruct),
    RequiredKeyword(ParameterStruct),
    OptionalKeyword(ParameterStruct),
    RestKeyword(ParameterStruct),
    Forward(ParameterStruct),
    Block(ParameterStruct),
}

#[derive(Debug)]
pub struct ParameterStruct {
    offset: Offset,
    str: StringId,
}

impl ParameterStruct {
    #[must_use]
    pub const fn new(offset: Offset, str: StringId) -> Self {
        Self { offset, str }
    }

    #[must_use]
    pub fn offset(&self) -> &Offset {
        &self.offset
    }

    #[must_use]
    pub fn str(&self) -> &StringId {
        &self.str
    }
}

/// An attr accessor definition
///
/// # Example
/// ```ruby
/// attr_accessor :foo
/// ```
#[derive(Debug)]
pub struct AttrAccessorDefinition {
    str_id: StringId,
    uri_id: UriId,
    offset: Offset,
    comments: Vec<Comment>,
    lexical_nesting_id: Option<DefinitionId>,
    visibility: Visibility,
}

impl AttrAccessorDefinition {
    #[must_use]
    pub const fn new(
        str_id: StringId,
        uri_id: UriId,
        offset: Offset,
        comments: Vec<Comment>,
        lexical_nesting_id: Option<DefinitionId>,
        visibility: Visibility,
    ) -> Self {
        Self {
            str_id,
            uri_id,
            offset,
            comments,
            lexical_nesting_id,
            visibility,
        }
    }

    #[must_use]
    pub fn id(&self) -> DefinitionId {
        DefinitionId::from(&format!("{}{}{}", *self.uri_id, self.offset.start(), *self.str_id))
    }

    #[must_use]
    pub fn str_id(&self) -> &StringId {
        &self.str_id
    }

    #[must_use]
    pub fn uri_id(&self) -> &UriId {
        &self.uri_id
    }

    #[must_use]
    pub fn offset(&self) -> &Offset {
        &self.offset
    }

    #[must_use]
    pub fn comments(&self) -> &[Comment] {
        &self.comments
    }

    #[must_use]
    pub fn lexical_nesting_id(&self) -> &Option<DefinitionId> {
        &self.lexical_nesting_id
    }

    #[must_use]
    pub fn visibility(&self) -> &Visibility {
        &self.visibility
    }
}

/// An attr reader definition
///
/// # Example
/// ```ruby
/// attr_reader :foo
/// ```
#[derive(Debug)]
pub struct AttrReaderDefinition {
    str_id: StringId,
    uri_id: UriId,
    offset: Offset,
    comments: Vec<Comment>,
    lexical_nesting_id: Option<DefinitionId>,
    visibility: Visibility,
}

impl AttrReaderDefinition {
    #[must_use]
    pub const fn new(
        str_id: StringId,
        uri_id: UriId,
        offset: Offset,
        comments: Vec<Comment>,
        lexical_nesting_id: Option<DefinitionId>,
        visibility: Visibility,
    ) -> Self {
        Self {
            str_id,
            uri_id,
            offset,
            comments,
            lexical_nesting_id,
            visibility,
        }
    }

    #[must_use]
    pub fn id(&self) -> DefinitionId {
        DefinitionId::from(&format!("{}{}{}", *self.uri_id, self.offset.start(), *self.str_id))
    }

    #[must_use]
    pub fn str_id(&self) -> &StringId {
        &self.str_id
    }

    #[must_use]
    pub fn uri_id(&self) -> &UriId {
        &self.uri_id
    }

    #[must_use]
    pub fn offset(&self) -> &Offset {
        &self.offset
    }

    #[must_use]
    pub fn comments(&self) -> &[Comment] {
        &self.comments
    }

    #[must_use]
    pub fn lexical_nesting_id(&self) -> &Option<DefinitionId> {
        &self.lexical_nesting_id
    }

    #[must_use]
    pub fn visibility(&self) -> &Visibility {
        &self.visibility
    }
}

/// An attr writer definition
///
/// # Example
/// ```ruby
/// attr_writer :foo
/// ```
#[derive(Debug)]
pub struct AttrWriterDefinition {
    str_id: StringId,
    uri_id: UriId,
    offset: Offset,
    comments: Vec<Comment>,
    lexical_nesting_id: Option<DefinitionId>,
    visibility: Visibility,
}

impl AttrWriterDefinition {
    #[must_use]
    pub const fn new(
        str_id: StringId,
        uri_id: UriId,
        offset: Offset,
        comments: Vec<Comment>,
        lexical_nesting_id: Option<DefinitionId>,
        visibility: Visibility,
    ) -> Self {
        Self {
            str_id,
            uri_id,
            offset,
            comments,
            lexical_nesting_id,
            visibility,
        }
    }

    #[must_use]
    pub fn id(&self) -> DefinitionId {
        DefinitionId::from(&format!("{}{}{}", *self.uri_id, self.offset.start(), *self.str_id))
    }

    #[must_use]
    pub fn str_id(&self) -> &StringId {
        &self.str_id
    }

    #[must_use]
    pub fn comments(&self) -> &[Comment] {
        &self.comments
    }

    #[must_use]
    pub fn uri_id(&self) -> &UriId {
        &self.uri_id
    }

    #[must_use]
    pub fn offset(&self) -> &Offset {
        &self.offset
    }

    #[must_use]
    pub fn lexical_nesting_id(&self) -> &Option<DefinitionId> {
        &self.lexical_nesting_id
    }

    #[must_use]
    pub fn visibility(&self) -> &Visibility {
        &self.visibility
    }
}

/// A global variable definition
///
/// # Example
/// ```ruby
/// $foo = 1
/// ```
#[derive(Debug)]
pub struct GlobalVariableDefinition {
    str_id: StringId,
    uri_id: UriId,
    offset: Offset,
    comments: Vec<Comment>,
    lexical_nesting_id: Option<DefinitionId>,
}

impl GlobalVariableDefinition {
    #[must_use]
    pub const fn new(
        str_id: StringId,
        uri_id: UriId,
        offset: Offset,
        comments: Vec<Comment>,
        lexical_nesting_id: Option<DefinitionId>,
    ) -> Self {
        Self {
            str_id,
            uri_id,
            offset,
            comments,
            lexical_nesting_id,
        }
    }

    #[must_use]
    pub fn id(&self) -> DefinitionId {
        DefinitionId::from(&format!("{}{}{}", *self.uri_id, self.offset.start(), *self.str_id))
    }

    #[must_use]
    pub fn str_id(&self) -> &StringId {
        &self.str_id
    }

    #[must_use]
    pub fn uri_id(&self) -> &UriId {
        &self.uri_id
    }

    #[must_use]
    pub fn offset(&self) -> &Offset {
        &self.offset
    }

    #[must_use]
    pub fn comments(&self) -> &[Comment] {
        &self.comments
    }

    #[must_use]
    pub fn lexical_nesting_id(&self) -> &Option<DefinitionId> {
        &self.lexical_nesting_id
    }
}

/// An instance variable definition
///
/// # Example
/// ```ruby
/// @foo = 1
/// ```
#[derive(Debug)]
pub struct InstanceVariableDefinition {
    str_id: StringId,
    uri_id: UriId,
    offset: Offset,
    comments: Vec<Comment>,
    lexical_nesting_id: Option<DefinitionId>,
}

impl InstanceVariableDefinition {
    #[must_use]
    pub const fn new(
        str_id: StringId,
        uri_id: UriId,
        offset: Offset,
        comments: Vec<Comment>,
        lexical_nesting_id: Option<DefinitionId>,
    ) -> Self {
        Self {
            str_id,
            uri_id,
            offset,
            comments,
            lexical_nesting_id,
        }
    }

    #[must_use]
    pub fn id(&self) -> DefinitionId {
        DefinitionId::from(&format!("{}{}{}", *self.uri_id, self.offset.start(), *self.str_id))
    }

    #[must_use]
    pub fn str_id(&self) -> &StringId {
        &self.str_id
    }

    #[must_use]
    pub fn uri_id(&self) -> &UriId {
        &self.uri_id
    }

    #[must_use]
    pub fn offset(&self) -> &Offset {
        &self.offset
    }

    #[must_use]
    pub fn comments(&self) -> &[Comment] {
        &self.comments
    }

    #[must_use]
    pub fn lexical_nesting_id(&self) -> &Option<DefinitionId> {
        &self.lexical_nesting_id
    }
}

/// A class variable definition
///
/// # Example
/// ```ruby
/// @@foo = 1
/// ```
#[derive(Debug)]
pub struct ClassVariableDefinition {
    str_id: StringId,
    uri_id: UriId,
    offset: Offset,
    comments: Vec<Comment>,
    lexical_nesting_id: Option<DefinitionId>,
}

impl ClassVariableDefinition {
    #[must_use]
    pub const fn new(
        str_id: StringId,
        uri_id: UriId,
        offset: Offset,
        comments: Vec<Comment>,
        lexical_nesting_id: Option<DefinitionId>,
    ) -> Self {
        Self {
            str_id,
            uri_id,
            offset,
            comments,
            lexical_nesting_id,
        }
    }

    #[must_use]
    pub fn id(&self) -> DefinitionId {
        DefinitionId::from(&format!("{}{}{}", *self.uri_id, self.offset.start(), *self.str_id))
    }

    #[must_use]
    pub fn str_id(&self) -> &StringId {
        &self.str_id
    }

    #[must_use]
    pub fn uri_id(&self) -> &UriId {
        &self.uri_id
    }

    #[must_use]
    pub fn offset(&self) -> &Offset {
        &self.offset
    }

    #[must_use]
    pub fn comments(&self) -> &[Comment] {
        &self.comments
    }

    #[must_use]
    pub fn lexical_nesting_id(&self) -> &Option<DefinitionId> {
        &self.lexical_nesting_id
    }
}
