#include "diagnostic.h"
#include "rustbindings.h"

/*
 * RDoc parser workaround for https://github.com/ruby/rdoc/issues/1744:
 * mRubydex = rb_define_module("Rubydex")
 */

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

void rdxi_initialize_diagnostic(VALUE mRubydex) {
    cDiagnostic = rb_define_class_under(mRubydex, "Diagnostic", rb_cObject);
}
