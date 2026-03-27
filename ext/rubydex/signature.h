#ifndef RUBYDEX_SIGNATURE_H
#define RUBYDEX_SIGNATURE_H

#include "ruby.h"
#include "rustbindings.h"

extern VALUE cSignature;

// Convert a SignatureArray into a Ruby array of Rubydex::Signature objects.
// If default_method_def is not Qnil, it is used as method_definition for all signatures.
// Otherwise, a new MethodDefinition handle is built from each SignatureEntry's definition_id.
// The SignatureArray is freed after conversion.
VALUE rdxi_signatures_to_ruby(SignatureArray *arr, VALUE graph_obj, VALUE default_method_def);

void rdxi_initialize_signature(VALUE mRubydex);

#endif // RUBYDEX_SIGNATURE_H
