#ifndef RUBYDEX_SIGNATURE_H
#define RUBYDEX_SIGNATURE_H

#include "ruby.h"
#include "rustbindings.h"

extern VALUE cSignature;
extern VALUE cParameter;
extern VALUE cPositionalParameter;
extern VALUE cOptionalPositionalParameter;
extern VALUE cRestPositionalParameter;
extern VALUE cPostParameter;
extern VALUE cKeywordParameter;
extern VALUE cOptionalKeywordParameter;
extern VALUE cRestKeywordParameter;
extern VALUE cForwardParameter;
extern VALUE cBlockParameter;

void rdxi_initialize_signature(VALUE mRubydex);

VALUE rdxi_signatures_to_ruby(SignatureArray *arr);

#endif // RUBYDEX_SIGNATURE_H
