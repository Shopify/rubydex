#ifndef RUBYDEX_DIAGNOSTIC_H
#define RUBYDEX_DIAGNOSTIC_H

#include "ruby.h"
#include "rustbindings.h"

extern VALUE cDiagnostic;

void rdxi_initialize_diagnostic(VALUE mRubydex);
VALUE rdxi_build_diagnostic_severity_value(VALUE mRubydex, DiagnosticSeverity severity);

#endif // RUBYDEX_DIAGNOSTIC_H
