#include "diagnostic.h"
#include "rustbindings.h"

/*
 * RDoc parser workaround for https://github.com/ruby/rdoc/issues/1744:
 * mRubydex = rb_define_module("Rubydex")
 */

static VALUE mRules;
VALUE cDiagnostic;

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

VALUE rdxi_rule_class_from_name(const char *name, size_t length) {
    return rb_const_get_at(mRules, rb_intern2(name, (long)length));
}

void rdxi_initialize_diagnostic(VALUE moduleRubydex) {
    cDiagnostic = rb_define_class_under(moduleRubydex, "Diagnostic", rb_cObject);
    mRules = rb_define_module_under(moduleRubydex, "Rules");
}
