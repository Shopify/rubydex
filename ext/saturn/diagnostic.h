#ifndef SATURN_DIAGNOSTIC_H
#define SATURN_DIAGNOSTIC_H

#include "ruby.h"
#include "rustbindings.h"

extern VALUE cDiagnostic;

void initialize_diagnostic(VALUE mSaturn);

VALUE severity_symbol(DiagnosticSeverity severity);

#endif // SATURN_DIAGNOSTIC_H
