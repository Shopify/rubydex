#ifndef RUBYDEX_REFERENCE_H
#define RUBYDEX_REFERENCE_H

#include "ruby.h"
#include "rustbindings.h"

extern VALUE cReference;
extern VALUE cConstantReference;
extern VALUE cUnresolvedConstantReference;
extern VALUE cResolvedConstantReference;
extern VALUE cMethodReference;

void rdxi_initialize_reference(VALUE mRubydex);

#endif // RUBYDEX_REFERENCE_H
