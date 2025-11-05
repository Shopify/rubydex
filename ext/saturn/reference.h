#ifndef SATURN_REFERENCE_H
#define SATURN_REFERENCE_H

#include "ruby.h"
#include "rustbindings.h"

extern VALUE cUnresolvedReference;
extern VALUE cUnresolvedConstantReference;
extern VALUE cUnresolvedMethodReference;

void initialize_reference(VALUE mSaturn);

// Returns the Ruby class for a given UnresolvedReferenceKind without calling back into Rust
VALUE reference_class_for_kind(UnresolvedReferenceKind kind);

#endif // SATURN_REFERENCE_H
