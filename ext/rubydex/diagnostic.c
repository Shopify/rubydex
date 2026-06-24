#include "diagnostic.h"
#include "rustbindings.h"

/*
 * RDoc parser workaround for https://github.com/ruby/rdoc/issues/1744:
 * mRubydex = rb_define_module("Rubydex")
 */

VALUE cDiagnostic;

void rdxi_initialize_diagnostic(VALUE mRubydex) { cDiagnostic = rb_define_class_under(mRubydex, "Diagnostic", rb_cObject); }
