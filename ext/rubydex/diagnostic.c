#include "diagnostic.h"
#include "rustbindings.h"

/*
 * RDoc parser workaround for https://github.com/ruby/rdoc/issues/1744:
 * mRubydex = rb_define_module("Rubydex")
 */

static VALUE mRubydex;
VALUE cDiagnostic;
static VALUE cRuleDefinition;
static ID id_default_severity;

VALUE rdxi_build_diagnostic_severity_value(VALUE mRubydex, DiagnosticSeverity severity) {
    VALUE mSeverity = rb_const_get(mRubydex, rb_intern("Severity"));

    switch (severity) {
    case DiagnosticSeverity_Error:
        return rb_const_get(mSeverity, rb_intern("Error"));
    case DiagnosticSeverity_Warning:
        return rb_const_get(mSeverity, rb_intern("Warning"));
    case DiagnosticSeverity_Information:
        return rb_const_get(mSeverity, rb_intern("Information"));
    case DiagnosticSeverity_Hint:
        return rb_const_get(mSeverity, rb_intern("Hint"));
    default:
        rb_raise(rb_eRuntimeError, "Unknown DiagnosticSeverity: %d", severity);
    }

    return Qnil;
}

/*
 * call-seq:
 *   default_severity -> Rubydex::Severity::Base
 *
 * Returns the Rubydex::Severity subclass the diagnostics of this rule get unless the configuration overrides it.
 */
static VALUE rdxr_generated_rule_default_severity(VALUE self) {
    VALUE severity = rb_attr_get(self, id_default_severity);

    // Class-level instance variables are not inherited, so a class inheriting from a generated rule reaches this without
    // a severity of its own.
    if (NIL_P(severity)) {
        rb_raise(rb_eRuntimeError, "Rule definition %s has no default severity", rb_class2name(self));
    }

    return severity;
}

// Generates a rule definition for every rule the graph can report. Severities are resolved here rather than when they are
// read, which is why Rubydex::Severity is loaded before this extension (see `lib/rubydex.rb`).
static void define_generated_rules(VALUE mRules) {
    CRuleDefinitionArray definition_array = rdx_rule_definitions();
    VALUE definitions = rb_ary_new_capa((long)definition_array.len);

    for (size_t i = 0; i < definition_array.len; i++) {
        CRuleDefinition definition = definition_array.items[i];
        VALUE name = rb_utf8_str_new(definition.name, (long)definition.name_length);
        VALUE rule = rb_define_class_under(mRules, StringValueCStr(name), cRuleDefinition);
        VALUE severity = rdxi_build_diagnostic_severity_value(mRubydex, definition.default_severity);

        rb_ivar_set(rule, id_default_severity, severity);
        rb_define_singleton_method(rule, "default_severity", rdxr_generated_rule_default_severity, 0);
        rb_ary_push(definitions, rule);
    }

    rdx_rule_definitions_free(definition_array);
    rb_define_const(mRules, "ALL", rb_obj_freeze(definitions));
}

void rdxi_initialize_diagnostic(VALUE moduleRubydex) {
    mRubydex = moduleRubydex;
    id_default_severity = rb_intern("@default_severity");

    cDiagnostic = rb_define_class_under(mRubydex, "Diagnostic", rb_cObject);
    cRuleDefinition = rb_define_class_under(mRubydex, "RuleDefinition", rb_cObject);
    define_generated_rules(rb_define_module_under(mRubydex, "Rules"));
}
