#ifndef RUBYDEX_DIAGNOSTIC_H
#define RUBYDEX_DIAGNOSTIC_H

#include "ruby.h"
#include "rustbindings.h"

extern VALUE cDiagnostic;

void rdxi_initialize_diagnostic(VALUE mRubydex);

#endif // RUBYDEX_DIAGNOSTIC_H
