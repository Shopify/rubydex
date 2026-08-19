#ifndef RUBYDEX_DIAGNOSTIC_H
#define RUBYDEX_DIAGNOSTIC_H

#include "ruby.h"
#include "rustbindings.h"

extern VALUE cDiagnostic;

void rdxi_initialize_diagnostic(VALUE mRubydex);
VALUE rdxi_build_diagnostic_severity_value(VALUE mRubydex, DiagnosticSeverity severity);
VALUE rdxi_rule_class_from_name(const char *name, size_t length);

#endif // RUBYDEX_DIAGNOSTIC_H
