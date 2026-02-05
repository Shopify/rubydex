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

use bitflags::bitflags;

use crate::{
    assert_mem_size,
    model::{
        comment::Comment,
        ids::{DefinitionId, NameId, ReferenceId, StringId, UriId},
        visibility::Visibility,
    },
    offset::Offset,
};

bitflags! {
    #[derive(Debug, Clone)]
    pub struct DefinitionFlags: u8 {
        const DEPRECATED = 0b0001;
    }
}

impl DefinitionFlags {
    #[must_use]
    pub fn is_deprecated(&self) -> bool {
        self.contains(Self::DEPRECATED)
    }
}

#[derive(Debug)]
pub enum Definition {
    Class(Box<ClassDefinition>),
    SingletonClass(Box<SingletonClassDefinition>),
    Module(Box<ModuleDefinition>),
    Constant(Box<ConstantDefinition>),
    ConstantAlias(Box<ConstantAliasDefinition>),
    Method(Box<MethodDefinition>),
    AttrAccessor(Box<AttrAccessorDefinition>),
    AttrReader(Box<AttrReaderDefinition>),
    AttrWriter(Box<AttrWriterDefinition>),
    GlobalVariable(Box<GlobalVariableDefinition>),
    InstanceVariable(Box<InstanceVariableDefinition>),
    ClassVariable(Box<ClassVariableDefinition>),
    MethodAlias(Box<MethodAliasDefinition>),
    GlobalVariableAlias(Box<GlobalVariableAliasDefinition>),
}
assert_mem_size!(Definition, 16);

macro_rules! all_definitions {
    ($value:expr, $var:ident => $expr:expr) => {
        match $value {
            Definition::Class($var) => $expr,
            Definition::SingletonClass($var) => $expr,
            Definition::Module($var) => $expr,
            Definition::Constant($var) => $expr,
            Definition::ConstantAlias($var) => $expr,
            Definition::GlobalVariable($var) => $expr,
            Definition::InstanceVariable($var) => $expr,
            Definition::ClassVariable($var) => $expr,
            Definition::AttrAccessor($var) => $expr,
            Definition::AttrReader($var) => $expr,
            Definition::AttrWriter($var) => $expr,
            Definition::Method($var) => $expr,
            Definition::MethodAlias($var) => $expr,
            Definition::GlobalVariableAlias($var) => $expr,
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
    pub fn comments(&self) -> &[Comment] {
        all_definitions!(self, it => it.comments())
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
            Definition::ConstantAlias(_) => "ConstantAlias",
            Definition::Method(_) => "Method",
            Definition::AttrAccessor(_) => "AttrAccessor",
            Definition::AttrReader(_) => "AttrReader",
            Definition::AttrWriter(_) => "AttrWriter",
            Definition::GlobalVariable(_) => "GlobalVariable",
            Definition::InstanceVariable(_) => "InstanceVariable",
            Definition::ClassVariable(_) => "ClassVariable",
            Definition::MethodAlias(_) => "AliasMethod",
            Definition::GlobalVariableAlias(_) => "GlobalVariableAlias",
        }
    }

    #[must_use]
    pub fn name_id(&self) -> Option<&NameId> {
        match self {
            Definition::Class(d) => Some(d.name_id()),
            Definition::SingletonClass(d) => Some(d.name_id()),
            Definition::Module(d) => Some(d.name_id()),
            Definition::Constant(d) => Some(d.name_id()),
            Definition::ConstantAlias(d) => Some(d.name_id()),
            Definition::GlobalVariable(_)
            | Definition::InstanceVariable(_)
            | Definition::ClassVariable(_)
            | Definition::AttrAccessor(_)
            | Definition::AttrReader(_)
            | Definition::AttrWriter(_)
            | Definition::Method(_)
            | Definition::MethodAlias(_)
            | Definition::GlobalVariableAlias(_) => None,
        }
    }

    #[must_use]
    pub fn name_offset(&self) -> Option<&Offset> {
        match self {
            Definition::Class(d) => Some(d.name_offset()),
            Definition::Module(d) => Some(d.name_offset()),
            Definition::SingletonClass(d) => Some(d.name_offset()),
            _ => None,
        }
    }

    #[must_use]
    pub fn is_deprecated(&self) -> bool {
        all_definitions!(self, it => it.flags().is_deprecated())
    }
}

/// Represents a mixin: include, prepend, or extend.
/// During resolution, `Extend` mixins are attached to the singleton class.
#[derive(Debug, Clone)]
pub enum Mixin {
    Include(IncludeDefinition),
    Prepend(PrependDefinition),
    Extend(ExtendDefinition),
}

impl Mixin {
    #[must_use]
    pub fn constant_reference_id(&self) -> &ReferenceId {
        match self {
            Mixin::Include(def) => def.constant_reference_id(),
            Mixin::Prepend(def) => def.constant_reference_id(),
            Mixin::Extend(def) => def.constant_reference_id(),
        }
    }
}

macro_rules! mixin_definition {
    ($variant:ident, $name:ident) => {
        #[derive(Debug, Clone)]
        pub struct $name {
            constant_reference_id: ReferenceId,
        }

        impl $name {
            #[must_use]
            pub const fn new(constant_reference_id: ReferenceId) -> Self {
                Self {
                    constant_reference_id,
                }
            }

            #[must_use]
            pub fn constant_reference_id(&self) -> &ReferenceId {
                &self.constant_reference_id
            }
        }
    };
}

mixin_definition!(Include, IncludeDefinition);
mixin_definition!(Prepend, PrependDefinition);
mixin_definition!(Extend, ExtendDefinition);

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
    name_offset: Offset,
    flags: DefinitionFlags,
    comments: Vec<Comment>,
    lexical_nesting_id: Option<DefinitionId>,
    members: Vec<DefinitionId>,
    superclass_ref: Option<ReferenceId>,
    mixins: Vec<Mixin>,
}
assert_mem_size!(ClassDefinition, 144);

impl ClassDefinition {
    #[must_use]
    #[allow(clippy::too_many_arguments)]
    pub const fn new(
        name_id: NameId,
        uri_id: UriId,
        offset: Offset,
        name_offset: Offset,
        comments: Vec<Comment>,
        flags: DefinitionFlags,
        lexical_nesting_id: Option<DefinitionId>,
        superclass_ref: Option<ReferenceId>,
    ) -> Self {
        Self {
            name_id,
            uri_id,
            offset,
            name_offset,
            flags,
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
    pub fn name_offset(&self) -> &Offset {
        &self.name_offset
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
    pub fn superclass_ref(&self) -> Option<&ReferenceId> {
        self.superclass_ref.as_ref()
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

    #[must_use]
    pub fn flags(&self) -> &DefinitionFlags {
        &self.flags
    }
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
    name_offset: Offset,
    flags: DefinitionFlags,
    comments: Vec<Comment>,
    /// The definition where `class << X` was found (lexical owner)
    lexical_nesting_id: Option<DefinitionId>,
    /// Members defined directly in this singleton class
    members: Vec<DefinitionId>,
    /// Mixins declared in this singleton class
    mixins: Vec<Mixin>,
}
assert_mem_size!(SingletonClassDefinition, 128);

impl SingletonClassDefinition {
    #[must_use]
    pub const fn new(
        name_id: NameId,
        uri_id: UriId,
        offset: Offset,
        name_offset: Offset,
        comments: Vec<Comment>,
        flags: DefinitionFlags,
        lexical_nesting_id: Option<DefinitionId>,
    ) -> Self {
        Self {
            name_id,
            uri_id,
            offset,
            name_offset,
            flags,
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
    pub fn name_offset(&self) -> &Offset {
        &self.name_offset
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

    #[must_use]
    pub fn flags(&self) -> &DefinitionFlags {
        &self.flags
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
    name_offset: Offset,
    flags: DefinitionFlags,
    comments: Vec<Comment>,
    lexical_nesting_id: Option<DefinitionId>,
    members: Vec<DefinitionId>,
    mixins: Vec<Mixin>,
}
assert_mem_size!(ModuleDefinition, 128);

impl ModuleDefinition {
    #[must_use]
    pub const fn new(
        name_id: NameId,
        uri_id: UriId,
        offset: Offset,
        name_offset: Offset,
        comments: Vec<Comment>,
        flags: DefinitionFlags,
        lexical_nesting_id: Option<DefinitionId>,
    ) -> Self {
        Self {
            name_id,
            uri_id,
            offset,
            name_offset,
            flags,
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
    pub fn name_offset(&self) -> &Offset {
        &self.name_offset
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

    #[must_use]
    pub fn flags(&self) -> &DefinitionFlags {
        &self.flags
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
    flags: DefinitionFlags,
    comments: Vec<Comment>,
    lexical_nesting_id: Option<DefinitionId>,
}
assert_mem_size!(ConstantDefinition, 72);

impl ConstantDefinition {
    #[must_use]
    pub const fn new(
        name_id: NameId,
        uri_id: UriId,
        offset: Offset,
        comments: Vec<Comment>,
        flags: DefinitionFlags,
        lexical_nesting_id: Option<DefinitionId>,
    ) -> Self {
        Self {
            name_id,
            uri_id,
            offset,
            flags,
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

    #[must_use]
    pub fn flags(&self) -> &DefinitionFlags {
        &self.flags
    }
}

/// A constant alias definition
///
/// # Example
/// ```ruby
/// module Foo; end
/// ALIAS = Foo
/// ```
#[derive(Debug)]
pub struct ConstantAliasDefinition {
    alias_constant: ConstantDefinition,
    target_name_id: NameId,
}
assert_mem_size!(ConstantAliasDefinition, 80);

impl ConstantAliasDefinition {
    #[must_use]
    pub const fn new(target_name_id: NameId, alias_constant: ConstantDefinition) -> Self {
        Self {
            alias_constant,
            target_name_id,
        }
    }

    #[must_use]
    pub fn id(&self) -> DefinitionId {
        DefinitionId::from(&format!(
            "{}{}{}{}",
            *self.alias_constant.uri_id(),
            self.alias_constant.offset().start(),
            *self.alias_constant.name_id(),
            *self.target_name_id,
        ))
    }

    #[must_use]
    pub fn name_id(&self) -> &NameId {
        self.alias_constant.name_id()
    }

    #[must_use]
    pub fn target_name_id(&self) -> &NameId {
        &self.target_name_id
    }

    #[must_use]
    pub fn uri_id(&self) -> &UriId {
        self.alias_constant.uri_id()
    }

    #[must_use]
    pub fn offset(&self) -> &Offset {
        self.alias_constant.offset()
    }

    #[must_use]
    pub fn comments(&self) -> &[Comment] {
        self.alias_constant.comments()
    }

    #[must_use]
    pub fn lexical_nesting_id(&self) -> &Option<DefinitionId> {
        self.alias_constant.lexical_nesting_id()
    }

    #[must_use]
    pub fn flags(&self) -> &DefinitionFlags {
        self.alias_constant.flags()
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
    flags: DefinitionFlags,
    comments: Vec<Comment>,
    lexical_nesting_id: Option<DefinitionId>,
    parameters: Vec<Parameter>,
    visibility: Visibility,
    receiver: Option<NameId>,
}
assert_mem_size!(MethodDefinition, 112);

impl MethodDefinition {
    #[allow(clippy::too_many_arguments)]
    #[must_use]
    pub const fn new(
        str_id: StringId,
        uri_id: UriId,
        offset: Offset,
        comments: Vec<Comment>,
        flags: DefinitionFlags,
        lexical_nesting_id: Option<DefinitionId>,
        parameters: Vec<Parameter>,
        visibility: Visibility,
        receiver: Option<NameId>,
    ) -> Self {
        Self {
            str_id,
            uri_id,
            offset,
            flags,
            comments,
            lexical_nesting_id,
            parameters,
            visibility,
            receiver,
        }
    }

    #[must_use]
    pub fn id(&self) -> DefinitionId {
        let mut formatted_id = format!("{}{}{}", *self.uri_id, self.offset.start(), *self.str_id);

        if let Some(receiver) = self.receiver {
            formatted_id.push_str(&receiver.to_string());
        }

        DefinitionId::from(&formatted_id)
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

    #[must_use]
    pub fn flags(&self) -> &DefinitionFlags {
        &self.flags
    }
}

#[derive(Debug, Clone)]
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
assert_mem_size!(Parameter, 24);

#[derive(Debug, Clone)]
pub struct ParameterStruct {
    offset: Offset,
    str: StringId,
}
assert_mem_size!(ParameterStruct, 16);

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
    flags: DefinitionFlags,
    comments: Vec<Comment>,
    lexical_nesting_id: Option<DefinitionId>,
    visibility: Visibility,
}
assert_mem_size!(AttrAccessorDefinition, 72);

impl AttrAccessorDefinition {
    #[must_use]
    pub const fn new(
        str_id: StringId,
        uri_id: UriId,
        offset: Offset,
        comments: Vec<Comment>,
        flags: DefinitionFlags,
        lexical_nesting_id: Option<DefinitionId>,
        visibility: Visibility,
    ) -> Self {
        Self {
            str_id,
            uri_id,
            offset,
            flags,
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

    #[must_use]
    pub fn flags(&self) -> &DefinitionFlags {
        &self.flags
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
    flags: DefinitionFlags,
    comments: Vec<Comment>,
    lexical_nesting_id: Option<DefinitionId>,
    visibility: Visibility,
}
assert_mem_size!(AttrReaderDefinition, 72);

impl AttrReaderDefinition {
    #[must_use]
    pub const fn new(
        str_id: StringId,
        uri_id: UriId,
        offset: Offset,
        comments: Vec<Comment>,
        flags: DefinitionFlags,
        lexical_nesting_id: Option<DefinitionId>,
        visibility: Visibility,
    ) -> Self {
        Self {
            str_id,
            uri_id,
            offset,
            flags,
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

    #[must_use]
    pub fn flags(&self) -> &DefinitionFlags {
        &self.flags
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
    flags: DefinitionFlags,
    comments: Vec<Comment>,
    lexical_nesting_id: Option<DefinitionId>,
    visibility: Visibility,
}
assert_mem_size!(AttrWriterDefinition, 72);

impl AttrWriterDefinition {
    #[must_use]
    pub const fn new(
        str_id: StringId,
        uri_id: UriId,
        offset: Offset,
        comments: Vec<Comment>,
        flags: DefinitionFlags,
        lexical_nesting_id: Option<DefinitionId>,
        visibility: Visibility,
    ) -> Self {
        Self {
            str_id,
            uri_id,
            offset,
            flags,
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

    #[must_use]
    pub fn flags(&self) -> &DefinitionFlags {
        &self.flags
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
    flags: DefinitionFlags,
    comments: Vec<Comment>,
    lexical_nesting_id: Option<DefinitionId>,
}
assert_mem_size!(GlobalVariableDefinition, 72);

impl GlobalVariableDefinition {
    #[must_use]
    pub const fn new(
        str_id: StringId,
        uri_id: UriId,
        offset: Offset,
        comments: Vec<Comment>,
        flags: DefinitionFlags,
        lexical_nesting_id: Option<DefinitionId>,
    ) -> Self {
        Self {
            str_id,
            uri_id,
            offset,
            flags,
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

    #[must_use]
    pub fn flags(&self) -> &DefinitionFlags {
        &self.flags
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
    flags: DefinitionFlags,
    comments: Vec<Comment>,
    lexical_nesting_id: Option<DefinitionId>,
}
assert_mem_size!(InstanceVariableDefinition, 72);

impl InstanceVariableDefinition {
    #[must_use]
    pub const fn new(
        str_id: StringId,
        uri_id: UriId,
        offset: Offset,
        comments: Vec<Comment>,
        flags: DefinitionFlags,
        lexical_nesting_id: Option<DefinitionId>,
    ) -> Self {
        Self {
            str_id,
            uri_id,
            offset,
            flags,
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

    #[must_use]
    pub fn flags(&self) -> &DefinitionFlags {
        &self.flags
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
    flags: DefinitionFlags,
    comments: Vec<Comment>,
    lexical_nesting_id: Option<DefinitionId>,
}
assert_mem_size!(ClassVariableDefinition, 72);

impl ClassVariableDefinition {
    #[must_use]
    pub const fn new(
        str_id: StringId,
        uri_id: UriId,
        offset: Offset,
        comments: Vec<Comment>,
        flags: DefinitionFlags,
        lexical_nesting_id: Option<DefinitionId>,
    ) -> Self {
        Self {
            str_id,
            uri_id,
            offset,
            flags,
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

    #[must_use]
    pub fn flags(&self) -> &DefinitionFlags {
        &self.flags
    }
}

#[derive(Debug)]
pub struct MethodAliasDefinition {
    new_name_str_id: StringId,
    old_name_str_id: StringId,
    uri_id: UriId,
    offset: Offset,
    flags: DefinitionFlags,
    comments: Vec<Comment>,
    lexical_nesting_id: Option<DefinitionId>,
}
assert_mem_size!(MethodAliasDefinition, 80);

impl MethodAliasDefinition {
    #[must_use]
    pub const fn new(
        new_name_str_id: StringId,
        old_name_str_id: StringId,
        uri_id: UriId,
        offset: Offset,
        comments: Vec<Comment>,
        flags: DefinitionFlags,
        lexical_nesting_id: Option<DefinitionId>,
    ) -> Self {
        Self {
            new_name_str_id,
            old_name_str_id,
            uri_id,
            offset,
            flags,
            comments,
            lexical_nesting_id,
        }
    }

    #[must_use]
    pub fn id(&self) -> DefinitionId {
        DefinitionId::from(&format!(
            "{}{}{}{}",
            *self.uri_id,
            self.offset.start(),
            *self.new_name_str_id,
            *self.old_name_str_id,
        ))
    }

    #[must_use]
    pub fn new_name_str_id(&self) -> &StringId {
        &self.new_name_str_id
    }

    #[must_use]
    pub fn old_name_str_id(&self) -> &StringId {
        &self.old_name_str_id
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
    pub fn flags(&self) -> &DefinitionFlags {
        &self.flags
    }
}

#[derive(Debug)]
pub struct GlobalVariableAliasDefinition {
    new_name_str_id: StringId,
    old_name_str_id: StringId,
    uri_id: UriId,
    offset: Offset,
    flags: DefinitionFlags,
    comments: Vec<Comment>,
    lexical_nesting_id: Option<DefinitionId>,
}
assert_mem_size!(GlobalVariableAliasDefinition, 80);

impl GlobalVariableAliasDefinition {
    #[must_use]
    pub const fn new(
        new_name_str_id: StringId,
        old_name_str_id: StringId,
        uri_id: UriId,
        offset: Offset,
        comments: Vec<Comment>,
        flags: DefinitionFlags,
        lexical_nesting_id: Option<DefinitionId>,
    ) -> Self {
        Self {
            new_name_str_id,
            old_name_str_id,
            uri_id,
            offset,
            flags,
            comments,
            lexical_nesting_id,
        }
    }

    #[must_use]
    pub fn id(&self) -> DefinitionId {
        DefinitionId::from(&format!(
            "{}{}{}{}",
            *self.uri_id,
            self.offset.start(),
            *self.new_name_str_id,
            *self.old_name_str_id,
        ))
    }

    #[must_use]
    pub fn new_name_str_id(&self) -> &StringId {
        &self.new_name_str_id
    }

    #[must_use]
    pub fn old_name_str_id(&self) -> &StringId {
        &self.old_name_str_id
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
    pub fn flags(&self) -> &DefinitionFlags {
        &self.flags
    }
}
