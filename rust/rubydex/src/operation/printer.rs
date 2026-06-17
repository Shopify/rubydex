use std::fmt::Write;

use crate::model::{
    identity_maps::IdentityHashMap,
    ids::{NameId, StringId},
    name::NameRef,
    string_ref::StringRef,
    visibility::Visibility,
};
use crate::operation::{
    AliasConstant, AliasGlobalVariable, AliasMethod, AttrKind, DefineAttribute, DefineClassVariable, DefineConstant,
    DefineGlobalVariable, DefineInstanceVariable, EnterClass, EnterMethod, EnterModule, EnterSingletonClass, Mixin,
    MixinKind, Operation, ReferenceConstant, ReferenceInstanceVariable, ReferenceMethod, SetConstantVisibility,
    SetDefaultVisibility, SetMethodVisibility, Target,
};

struct OperationPrinter<'a> {
    strings: &'a IdentityHashMap<StringId, StringRef>,
    names: &'a IdentityHashMap<NameId, NameRef>,
    out: String,
    depth: usize,
    include_references: bool,
}

impl OperationPrinter<'_> {
    fn name_str(&self, name_id: NameId) -> String {
        let mut parts = Vec::new();
        let mut current = Some(name_id);
        while let Some(id) = current {
            let name = self.names.get(&id).expect("NameId should exist");
            let s = self.strings.get(name.str()).expect("StringId should exist");
            parts.push(s.as_str().to_string());
            current = name.parent_scope().as_ref().copied();
        }
        parts.reverse();
        parts.join("::")
    }

    fn string_value(&self, str_id: StringId) -> String {
        self.strings
            .get(&str_id)
            .expect("StringId should exist")
            .as_str()
            .to_string()
    }

    fn receiver_prefix(&self, receiver: Option<&Target>) -> String {
        match receiver {
            Some(Target::ExplicitSelf) => "self.".to_string(),
            Some(Target::Constant(name_id)) => format!("{}.", self.name_str(*name_id)),
            Some(Target::Other) => "<expr>.".to_string(),
            None => String::new(),
        }
    }

    fn vis(visibility: Visibility) -> &'static str {
        match visibility {
            Visibility::Public => "public",
            Visibility::Protected => "protected",
            Visibility::Private => "private",
            Visibility::ModuleFunction => "module_function",
        }
    }

    fn indent(&self) -> String {
        "  ".repeat(self.depth)
    }

    fn print_operation(&mut self, op: &Operation) {
        match op {
            Operation::EnterClass(op) => self.print_enter_class(op),
            Operation::EnterModule(op) => self.print_enter_module(op),
            Operation::EnterSingletonClass(op) => self.print_enter_singleton_class(op),
            Operation::EnterMethod(op) => self.print_enter_method(op),
            Operation::ExitScope => {
                self.depth = self.depth.saturating_sub(1);
                let indent = self.indent();
                writeln!(self.out, "{indent}ExitScope").unwrap();
            }
            Operation::AliasMethod(op) => self.print_alias_method(op),
            Operation::SetMethodVisibility(op) => self.print_set_method_visibility(op),
            Operation::SetDefaultVisibility(op) => self.print_set_default_visibility(op),
            Operation::DefineConstant(op) => self.print_define_constant(op),
            Operation::AliasConstant(op) => self.print_alias_constant(op),
            Operation::SetConstantVisibility(op) => self.print_set_constant_visibility(op),
            Operation::Mixin(op) => self.print_mixin(op),
            Operation::DefineAttribute(op) => self.print_define_attribute(op),
            Operation::DefineGlobalVariable(op) => self.print_define_global_variable(op),
            Operation::DefineInstanceVariable(op) => self.print_define_instance_variable(op),
            Operation::DefineClassVariable(op) => self.print_define_class_variable(op),
            Operation::AliasGlobalVariable(op) => self.print_alias_global_variable(op),
            Operation::ReferenceConstant(op) => self.print_reference_constant(op),
            Operation::ReferenceMethod(op) => self.print_reference_method(op),
            Operation::ReferenceInstanceVariable(op) => self.print_reference_instance_variable(op),
        }
    }

    fn print_enter_class(&mut self, op: &EnterClass) {
        let indent = self.indent();
        let name = self.name_str(op.name_id);
        write!(self.out, "{indent}EnterClass({name}").unwrap();
        if let Some(sc_name_id) = op.superclass_name {
            let sc = self.name_str(sc_name_id);
            write!(self.out, ", superclass: {sc}").unwrap();
        }
        writeln!(self.out, ")").unwrap();
        self.depth += 1;
    }

    fn print_enter_module(&mut self, op: &EnterModule) {
        let indent = self.indent();
        let name = self.name_str(op.name_id);
        writeln!(self.out, "{indent}EnterModule({name})").unwrap();
        self.depth += 1;
    }

    fn print_enter_singleton_class(&mut self, op: &EnterSingletonClass) {
        let indent = self.indent();
        let name = self.name_str(op.name_id);
        writeln!(self.out, "{indent}EnterSingletonClass({name})").unwrap();
        self.depth += 1;
    }

    fn print_enter_method(&mut self, op: &EnterMethod) {
        let indent = self.indent();
        let prefix = self.receiver_prefix(op.receiver.as_ref());
        let name = self.string_value(op.str_id);
        writeln!(self.out, "{indent}EnterMethod({prefix}{name})").unwrap();
        self.depth += 1;
    }

    fn print_alias_method(&mut self, op: &AliasMethod) {
        let indent = self.indent();
        let new_name = self.string_value(op.new_name_str_id);
        let old_name = self.string_value(op.old_name_str_id);
        writeln!(self.out, "{indent}AliasMethod({new_name} -> {old_name})").unwrap();
    }

    fn print_set_method_visibility(&mut self, op: &SetMethodVisibility) {
        let indent = self.indent();
        let name = self.string_value(op.str_id);
        let v = Self::vis(op.visibility);
        writeln!(self.out, "{indent}SetMethodVisibility({name}, vis: {v})").unwrap();
    }

    fn print_set_default_visibility(&mut self, op: &SetDefaultVisibility) {
        let indent = self.indent();
        let v = Self::vis(op.visibility);
        writeln!(self.out, "{indent}SetDefaultVisibility({v})").unwrap();
    }

    fn print_define_constant(&mut self, op: &DefineConstant) {
        let indent = self.indent();
        let name = self.name_str(op.name_id);
        writeln!(self.out, "{indent}DefineConstant({name})").unwrap();
    }

    fn print_alias_constant(&mut self, op: &AliasConstant) {
        let indent = self.indent();
        let name = self.name_str(op.name_id);
        let target = self.name_str(op.target_name_id);
        writeln!(self.out, "{indent}AliasConstant({name} -> {target})").unwrap();
    }

    fn print_set_constant_visibility(&mut self, op: &SetConstantVisibility) {
        let indent = self.indent();
        let name = self.string_value(op.target);
        let v = Self::vis(op.visibility);
        writeln!(self.out, "{indent}SetConstantVisibility({name}, vis: {v})").unwrap();
    }

    fn print_mixin(&mut self, op: &Mixin) {
        let indent = self.indent();
        let kind_str = match op.kind {
            MixinKind::Include => "include",
            MixinKind::Prepend => "prepend",
            MixinKind::Extend => "extend",
        };
        let target_name = match op.target {
            Target::Constant(name_id) => self.name_str(name_id),
            Target::ExplicitSelf => "self".to_string(),
            Target::Other => "<expr>".to_string(),
        };
        writeln!(self.out, "{indent}Mixin({kind_str}, {target_name})").unwrap();
    }

    fn print_define_attribute(&mut self, op: &DefineAttribute) {
        let indent = self.indent();
        let kind_str = match op.kind {
            AttrKind::Accessor => "accessor",
            AttrKind::Reader => "reader",
            AttrKind::Writer => "writer",
        };
        let name = self.string_value(op.str_id);
        writeln!(self.out, "{indent}DefineAttribute({kind_str} {name})").unwrap();
    }

    fn print_define_global_variable(&mut self, op: &DefineGlobalVariable) {
        let indent = self.indent();
        let name = self.string_value(op.str_id);
        writeln!(self.out, "{indent}DefineGlobalVariable({name})").unwrap();
    }

    fn print_define_instance_variable(&mut self, op: &DefineInstanceVariable) {
        let indent = self.indent();
        let name = self.string_value(op.str_id);
        writeln!(self.out, "{indent}DefineInstanceVariable({name})").unwrap();
    }

    fn print_define_class_variable(&mut self, op: &DefineClassVariable) {
        let indent = self.indent();
        let name = self.string_value(op.str_id);
        writeln!(self.out, "{indent}DefineClassVariable({name})").unwrap();
    }

    fn print_alias_global_variable(&mut self, op: &AliasGlobalVariable) {
        let indent = self.indent();
        let new_name = self.string_value(op.new_name_str_id);
        let old_name = self.string_value(op.old_name_str_id);
        writeln!(self.out, "{indent}AliasGlobalVariable({new_name} -> {old_name})").unwrap();
    }

    fn print_reference_constant(&mut self, op: &ReferenceConstant) {
        if self.include_references {
            let indent = self.indent();
            let name = self.name_str(op.name_id);
            writeln!(self.out, "{indent}ReferenceConstant({name})").unwrap();
        }
    }

    fn print_reference_method(&mut self, op: &ReferenceMethod) {
        if self.include_references {
            let indent = self.indent();
            let name = self.string_value(op.str_id);
            writeln!(self.out, "{indent}ReferenceMethod({name})").unwrap();
        }
    }

    fn print_reference_instance_variable(&mut self, op: &ReferenceInstanceVariable) {
        if self.include_references {
            let indent = self.indent();
            let name = self.string_value(op.str_id);
            writeln!(self.out, "{indent}ReferenceInstanceVariable({name})").unwrap();
        }
    }
}

#[must_use]
#[allow(clippy::implicit_hasher)]
pub fn print_operations(
    operations: &[Operation],
    strings: &IdentityHashMap<StringId, StringRef>,
    names: &IdentityHashMap<NameId, NameRef>,
    include_references: bool,
) -> String {
    let mut printer = OperationPrinter {
        strings,
        names,
        out: String::new(),
        depth: 0,
        include_references,
    };

    for op in operations {
        printer.print_operation(op);
    }

    printer.out.trim_end().to_string()
}
