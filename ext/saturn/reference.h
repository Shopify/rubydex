#ifndef SATURN_REFERENCE_H
#define SATURN_REFERENCE_H

#include "ruby.h"
#include "rustbindings.h"

extern VALUE cReference;
extern VALUE cConstantReference;
extern VALUE cMethodReference;

void initialize_reference(VALUE mSaturn);

// Returns the Ruby class for a given UnresolvedReferenceKind without calling back into Rust
VALUE reference_class_for_kind(ReferenceKind kind);

#endif // SATURN_REFERENCE_H
