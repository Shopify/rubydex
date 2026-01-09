#ifndef RUBYDEX_DIAGNOSTIC_H
#define RUBYDEX_DIAGNOSTIC_H

#include "ruby.h"
#include "rustbindings.h"

extern VALUE cDiagnostic;

void initialize_diagnostic(VALUE mRubydex);

VALUE severity_symbol(DiagnosticSeverity severity);

#endif // RUBYDEX_DIAGNOSTIC_H
