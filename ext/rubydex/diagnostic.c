#include "diagnostic.h"
#include "rustbindings.h"

VALUE cDiagnostic;

void initialize_diagnostic(VALUE mRubydex) { cDiagnostic = rb_define_class_under(mRubydex, "Diagnostic", rb_cObject); }
