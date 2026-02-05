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
        dsl::DslArgumentList,
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

#[repr(u8)]
#[derive(PartialEq, Debug)]
pub enum DefinitionKind {
    Class = 0,
    SingletonClass = 1,
    Module = 2,
    Constant = 3,
    ConstantAlias = 4,
    Method = 5,
    AttrAccessor = 6,
    AttrReader = 7,
    AttrWriter = 8,
    GlobalVariable = 9,
    InstanceVariable = 10,
    ClassVariable = 11,
    MethodAlias = 12,
    GlobalVariableAlias = 13,
}

impl From<u8> for DefinitionKind {
    fn from(value: u8) -> Self {
        match value {
            0 => DefinitionKind::Class,
            1 => DefinitionKind::SingletonClass,
            2 => DefinitionKind::Module,
            3 => DefinitionKind::Constant,
            4 => DefinitionKind::ConstantAlias,
            5 => DefinitionKind::Method,
            6 => DefinitionKind::AttrAccessor,
            7 => DefinitionKind::AttrReader,
            8 => DefinitionKind::AttrWriter,
            9 => DefinitionKind::GlobalVariable,
            10 => DefinitionKind::InstanceVariable,
            11 => DefinitionKind::ClassVariable,
            12 => DefinitionKind::MethodAlias,
            13 => DefinitionKind::GlobalVariableAlias,
            _ => panic!("Invalid DefinitionKind value: {value}"),
        }
    }
}

impl From<DefinitionKind> for u8 {
    fn from(kind: DefinitionKind) -> Self {
        kind as u8
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
    Dsl(Box<DslDefinition>),
    DynamicClass(Box<DynamicClassDefinition>),
    DynamicModule(Box<DynamicModuleDefinition>),
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
            Definition::Dsl($var) => $expr,
            Definition::DynamicClass($var) => $expr,
            Definition::DynamicModule($var) => $expr,
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
        all_definitions!(self, it => it.uri_id())
    }

    #[must_use]
    pub fn offset(&self) -> &Offset {
        all_definitions!(self, it => it.offset())
    }

    #[must_use]
    pub fn comments(&self) -> &[Comment] {
        all_definitions!(self, it => it.comments())
    }

    #[must_use]
    pub fn lexical_nesting_id(&self) -> &Option<DefinitionId> {
        all_definitions!(self, it => it.lexical_nesting_id())
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
            Definition::Dsl(_) => "Dsl",
            Definition::DynamicClass(_) => "DynamicClass",
            Definition::DynamicModule(_) => "DynamicModule",
        }
    }

    #[must_use]
    pub fn name_id(&self) -> Option<&NameId> {
        match self {
            Definition::Class(d) => Some(d.name_id()),
            Definition::SingletonClass(d) => Some(d.name_id()),
            Definition::Module(d) => Some(d.name_id()),
            Definition::Constant(d) => Some(d.name_id()),
            Definition::DynamicClass(d) => d.name_id(),
            Definition::DynamicModule(d) => d.name_id(),
            _ => None,
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
assert_mem_size!(ClassDefinition, 120);

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
        let mut id = DefinitionId::from(&format!("{}{}{}", *self.uri_id, self.offset.start(), *self.name_id));
        id.tag_kind(DefinitionKind::Class);
        id
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
assert_mem_size!(SingletonClassDefinition, 112);

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
        let mut id = DefinitionId::from(&format!("{}{}{}", *self.uri_id, self.offset.start(), *self.name_id));
        id.tag_kind(DefinitionKind::SingletonClass);
        id
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
assert_mem_size!(ModuleDefinition, 112);

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
        let mut id = DefinitionId::from(&format!("{}{}{}", *self.uri_id, self.offset.start(), *self.name_id));
        id.tag_kind(DefinitionKind::Module);
        id
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
assert_mem_size!(ConstantDefinition, 56);

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
        let mut id = DefinitionId::from(&format!("{}{}{}", *self.uri_id, self.offset.start(), *self.name_id));
        id.tag_kind(DefinitionKind::Constant);
        id
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
assert_mem_size!(ConstantAliasDefinition, 64);

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
        let mut id = DefinitionId::from(&format!(
            "{}{}{}{}",
            *self.alias_constant.uri_id(),
            self.alias_constant.offset().start(),
            *self.alias_constant.name_id(),
            *self.target_name_id,
        ));
        id.tag_kind(DefinitionKind::ConstantAlias);
        id
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

/// Represents the receiver of a singleton method definition.
///
/// - `SelfReceiver`: `def self.foo` - the enclosing class/module/DSL definition
/// - `ConstantReceiver`: `def Foo.bar` - an explicit constant reference
#[derive(Debug, Clone, Copy)]
pub enum Receiver {
    /// `def self.foo` - receiver is the enclosing definition (class, module, or DSL)
    SelfReceiver(DefinitionId),
    /// `def Foo.bar` - receiver is an explicit constant that needs resolution
    ConstantReceiver(NameId),
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
    receiver: Option<Receiver>,
}
assert_mem_size!(MethodDefinition, 88);

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
        receiver: Option<Receiver>,
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

        if let Some(receiver) = &self.receiver {
            match receiver {
                Receiver::SelfReceiver(def_id) => formatted_id.push_str(&def_id.to_string()),
                Receiver::ConstantReceiver(name_id) => formatted_id.push_str(&name_id.to_string()),
            }
        }

        let mut id = DefinitionId::from(&formatted_id);
        id.tag_kind(DefinitionKind::Method);
        id
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
    pub fn receiver(&self) -> Option<Receiver> {
        self.receiver
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
assert_mem_size!(Parameter, 16);

#[derive(Debug, Clone)]
pub struct ParameterStruct {
    offset: Offset,
    str: StringId,
}
assert_mem_size!(ParameterStruct, 12);

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
assert_mem_size!(AttrAccessorDefinition, 56);

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
        let mut id = DefinitionId::from(&format!("{}{}{}", *self.uri_id, self.offset.start(), *self.str_id));
        id.tag_kind(DefinitionKind::AttrAccessor);
        id
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
assert_mem_size!(AttrReaderDefinition, 56);

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
        let mut id = DefinitionId::from(&format!("{}{}{}", *self.uri_id, self.offset.start(), *self.str_id));
        id.tag_kind(DefinitionKind::AttrReader);
        id
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
assert_mem_size!(AttrWriterDefinition, 56);

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
        let mut id = DefinitionId::from(&format!("{}{}{}", *self.uri_id, self.offset.start(), *self.str_id));
        id.tag_kind(DefinitionKind::AttrWriter);
        id
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
assert_mem_size!(GlobalVariableDefinition, 56);

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
        let mut id = DefinitionId::from(&format!("{}{}{}", *self.uri_id, self.offset.start(), *self.str_id));
        id.tag_kind(DefinitionKind::GlobalVariable);
        id
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
assert_mem_size!(InstanceVariableDefinition, 56);

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
        let mut id = DefinitionId::from(&format!("{}{}{}", *self.uri_id, self.offset.start(), *self.str_id));
        id.tag_kind(DefinitionKind::InstanceVariable);
        id
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
assert_mem_size!(ClassVariableDefinition, 56);

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
        let mut id = DefinitionId::from(&format!("{}{}{}", *self.uri_id, self.offset.start(), *self.str_id));
        id.tag_kind(DefinitionKind::ClassVariable);
        id
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
assert_mem_size!(MethodAliasDefinition, 56);

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
        let mut id = DefinitionId::from(&format!(
            "{}{}{}{}",
            *self.uri_id,
            self.offset.start(),
            *self.new_name_str_id,
            *self.old_name_str_id,
        ));
        id.tag_kind(DefinitionKind::MethodAlias);
        id
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
assert_mem_size!(GlobalVariableAliasDefinition, 56);

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
        let mut id = DefinitionId::from(&format!(
            "{}{}{}{}",
            *self.uri_id,
            self.offset.start(),
            *self.new_name_str_id,
            *self.old_name_str_id,
        ));
        id.tag_kind(DefinitionKind::GlobalVariableAlias);
        id
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

/// A DSL definition captures a DSL method call (e.g., `Class.new`, `Module.new`).
///
/// This is created during indexing when a DSL call is detected. The receiver is
/// captured as a `NameId` which will be resolved during the resolution phase.
///
/// # Example
/// ```ruby
/// Foo = Class.new do
///   def bar; end
/// end
/// ```
///
/// Creates a `DslDefinition` with:
/// - `receiver_name`: `NameId(Class)` (to be resolved to `::Class`)
/// - `method_name`: `StringId("new")`
/// - `members`: `[MethodDefinition(bar)]`
/// - `assigned_to`: points to `ConstantDefinition` for `Foo`
#[derive(Debug)]
pub struct DslDefinition {
    /// The receiver of the DSL call (e.g., `Class` in `Class.new`).
    /// `None` for DSLs called without a receiver (e.g., `attr_accessor`).
    receiver_name: Option<NameId>,
    /// The method name of the DSL call (e.g., `"new"`)
    method_name: StringId,
    /// The arguments passed to the DSL method
    arguments: DslArgumentList,
    uri_id: UriId,
    offset: Offset,
    /// The lexical parent of this DSL definition (enclosing Class/Module).
    lexical_nesting_id: Option<DefinitionId>,
    /// Definitions that are owned by this DSL block (e.g., methods defined inside)
    members: Vec<DefinitionId>,
    /// Mixins (include, prepend, extend) within this DSL block
    mixins: Vec<Mixin>,
    /// Reference to the `ConstantDefinition` if this DSL is assigned to a constant.
    /// E.g., for `Foo = Class.new`, this points to the `ConstantDefinition` for `Foo`.
    assigned_to: Option<DefinitionId>,
}

impl DslDefinition {
    #[must_use]
    pub const fn new(
        receiver_name: Option<NameId>,
        method_name: StringId,
        arguments: DslArgumentList,
        uri_id: UriId,
        offset: Offset,
        lexical_nesting_id: Option<DefinitionId>,
        assigned_to: Option<DefinitionId>,
    ) -> Self {
        Self {
            receiver_name,
            method_name,
            arguments,
            uri_id,
            offset,
            lexical_nesting_id,
            members: Vec::new(),
            mixins: Vec::new(),
            assigned_to,
        }
    }

    #[must_use]
    pub fn id(&self) -> DefinitionId {
        let receiver_str = self.receiver_name.map_or(String::from("None"), |id| id.to_string());
        DefinitionId::from(&format!(
            "{}{}{}{}",
            *self.uri_id,
            self.offset.start(),
            receiver_str,
            *self.method_name
        ))
    }

    #[must_use]
    pub fn receiver_name(&self) -> Option<NameId> {
        self.receiver_name
    }

    #[must_use]
    pub fn method_name(&self) -> &StringId {
        &self.method_name
    }

    #[must_use]
    pub fn arguments(&self) -> &DslArgumentList {
        &self.arguments
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
        // DSL definitions don't have their own comments; those are on the ConstantDefinition
        &[]
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

    /// Returns the `DefinitionId` of the `ConstantDefinition` if this DSL is assigned to a constant.
    #[must_use]
    pub fn assigned_to(&self) -> Option<DefinitionId> {
        self.assigned_to
    }

    /// Sets the constant this DSL is assigned to.
    pub fn set_assigned_to(&mut self, id: DefinitionId) {
        self.assigned_to = Some(id);
    }

    #[must_use]
    pub fn flags(&self) -> &DefinitionFlags {
        // DSL definitions don't have flags
        static EMPTY_FLAGS: DefinitionFlags = DefinitionFlags::empty();
        &EMPTY_FLAGS
    }
}

/// A dynamically created class definition from `Class.new`.
///
/// This is created during DSL processing (Phase 3) when a `Class.new` call
/// is recognized and processed by the handler.
///
/// # Example
/// ```ruby
/// Foo = Class.new do
///   def bar; end
/// end
/// ```
///
/// After DSL processing, creates a `DynamicClassDefinition` that references
/// the underlying `DslDefinition` by ID and adds resolution-specific data.
#[derive(Debug)]
pub struct DynamicClassDefinition {
    /// Reference to the underlying DSL definition by ID
    dsl_definition_id: DefinitionId,
    /// The resolved name of the class (reparented if nested), or `None` for anonymous classes
    name_id: Option<NameId>,
    /// Reference to the superclass if specified
    superclass_ref: Option<ReferenceId>,
    /// Comments from the constant assignment (if any)
    comments: Vec<Comment>,
    /// Flags from the constant assignment (if any)
    flags: DefinitionFlags,
    // Copied from DSL for convenience (avoids needing graph access for common operations)
    uri_id: UriId,
    offset: Offset,
    lexical_nesting_id: Option<DefinitionId>,
    mixins: Vec<Mixin>,
}

impl DynamicClassDefinition {
    #[must_use]
    #[allow(clippy::too_many_arguments)]
    pub fn new(
        dsl_definition_id: DefinitionId,
        name_id: Option<NameId>,
        superclass_ref: Option<ReferenceId>,
        comments: Vec<Comment>,
        flags: DefinitionFlags,
        uri_id: UriId,
        offset: Offset,
        lexical_nesting_id: Option<DefinitionId>,
        mixins: Vec<Mixin>,
    ) -> Self {
        Self {
            dsl_definition_id,
            name_id,
            superclass_ref,
            comments,
            flags,
            uri_id,
            offset,
            lexical_nesting_id,
            mixins,
        }
    }

    #[must_use]
    pub fn id(&self) -> DefinitionId {
        DefinitionId::from(&format!("{}DynamicClass", *self.dsl_definition_id))
    }

    /// Computes the `DynamicClass` ID that would be created from a given DSL definition ID.
    /// This allows direct lookup without iteration.
    #[must_use]
    pub fn id_for_dsl(dsl_definition_id: DefinitionId) -> DefinitionId {
        DefinitionId::from(&format!("{}DynamicClass", *dsl_definition_id))
    }

    #[must_use]
    pub fn dsl_definition_id(&self) -> DefinitionId {
        self.dsl_definition_id
    }

    #[must_use]
    pub fn name_id(&self) -> Option<&NameId> {
        self.name_id.as_ref()
    }

    #[must_use]
    pub fn superclass_ref(&self) -> Option<&ReferenceId> {
        self.superclass_ref.as_ref()
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
    pub fn mixins(&self) -> &[Mixin] {
        &self.mixins
    }

    #[must_use]
    pub fn flags(&self) -> &DefinitionFlags {
        &self.flags
    }
}

/// A dynamically created module definition from `Module.new`.
///
/// This is created during DSL processing (Phase 3) when a `Module.new` call
/// is recognized and processed by the handler. It references the underlying
/// `DslDefinition` by ID and adds resolution-specific data.
#[derive(Debug)]
pub struct DynamicModuleDefinition {
    /// Reference to the underlying DSL definition by ID
    dsl_definition_id: DefinitionId,
    /// The resolved name of the module (reparented if nested), or `None` for anonymous modules
    name_id: Option<NameId>,
    /// Comments from the constant assignment (if any)
    comments: Vec<Comment>,
    /// Flags from the constant assignment (if any)
    flags: DefinitionFlags,
    // Copied from DSL for convenience (avoids needing graph access for common operations)
    uri_id: UriId,
    offset: Offset,
    lexical_nesting_id: Option<DefinitionId>,
    mixins: Vec<Mixin>,
}

impl DynamicModuleDefinition {
    #[must_use]
    #[allow(clippy::too_many_arguments)]
    pub fn new(
        dsl_definition_id: DefinitionId,
        name_id: Option<NameId>,
        comments: Vec<Comment>,
        flags: DefinitionFlags,
        uri_id: UriId,
        offset: Offset,
        lexical_nesting_id: Option<DefinitionId>,
        mixins: Vec<Mixin>,
    ) -> Self {
        Self {
            dsl_definition_id,
            name_id,
            comments,
            flags,
            uri_id,
            offset,
            lexical_nesting_id,
            mixins,
        }
    }

    #[must_use]
    pub fn id(&self) -> DefinitionId {
        DefinitionId::from(&format!("{}DynamicModule", *self.dsl_definition_id))
    }

    /// Computes the `DynamicModule` ID that would be created from a given DSL definition ID.
    /// This allows direct lookup without iteration.
    #[must_use]
    pub fn id_for_dsl(dsl_definition_id: DefinitionId) -> DefinitionId {
        DefinitionId::from(&format!("{}DynamicModule", *dsl_definition_id))
    }

    #[must_use]
    pub fn dsl_definition_id(&self) -> DefinitionId {
        self.dsl_definition_id
    }

    #[must_use]
    pub fn name_id(&self) -> Option<&NameId> {
        self.name_id.as_ref()
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
    pub fn mixins(&self) -> &[Mixin] {
        &self.mixins
    }

    #[must_use]
    pub fn flags(&self) -> &DefinitionFlags {
        &self.flags
    }
}
