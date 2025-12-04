#include "diagnostic.h"
#include "rustbindings.h"

VALUE cDiagnostic;

VALUE severity_symbol(DiagnosticSeverity severity) {
    switch (severity) {
    case DiagnosticSeverity_Error:
        return ID2SYM(rb_intern("error"));
    case DiagnosticSeverity_Warning:
        return ID2SYM(rb_intern("warning"));
    default:
        rb_raise(rb_eRuntimeError, "Unknown DiagnosticSeverity: %d", severity);
    }

    return Qnil;
}

void initialize_diagnostic(VALUE mSaturn) { cDiagnostic = rb_define_class_under(mSaturn, "Diagnostic", rb_cObject); }
