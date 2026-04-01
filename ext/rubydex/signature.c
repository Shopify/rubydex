#include "signature.h"
#include "location.h"

VALUE cSignature;
VALUE cParameter;
VALUE cPositionalParameter;
VALUE cOptionalPositionalParameter;
VALUE cRestPositionalParameter;
VALUE cPostParameter;
VALUE cKeywordParameter;
VALUE cOptionalKeywordParameter;
VALUE cRestKeywordParameter;
VALUE cForwardParameter;
VALUE cBlockParameter;

static VALUE parameter_class_for_kind(ParameterKind kind) {
    switch (kind) {
    case ParameterKind_RequiredPositional: return cPositionalParameter;
    case ParameterKind_OptionalPositional: return cOptionalPositionalParameter;
    case ParameterKind_RestPositional:     return cRestPositionalParameter;
    case ParameterKind_Post:              return cPostParameter;
    case ParameterKind_RequiredKeyword:    return cKeywordParameter;
    case ParameterKind_OptionalKeyword:    return cOptionalKeywordParameter;
    case ParameterKind_RestKeyword:        return cRestKeywordParameter;
    case ParameterKind_Forward:            return cForwardParameter;
    case ParameterKind_Block:              return cBlockParameter;
    default: rb_raise(rb_eRuntimeError, "Unknown ParameterKind: %d", kind);
    }
}

VALUE rdxi_signatures_to_ruby(SignatureArray *arr) {
    VALUE signatures = rb_ary_new_capa((long)arr->len);

    for (size_t i = 0; i < arr->len; i++) {
        SignatureEntry sig_entry = arr->items[i];

        VALUE parameters = rb_ary_new_capa((long)sig_entry.parameters_len);
        for (size_t j = 0; j < sig_entry.parameters_len; j++) {
            ParameterEntry param_entry = sig_entry.parameters[j];

            VALUE param_class = parameter_class_for_kind(param_entry.kind);
            VALUE name_sym = ID2SYM(rb_intern(param_entry.name));
            VALUE location = rdxi_build_location_value(param_entry.location);
            VALUE param_argv[] = {name_sym, location};
            VALUE param = rb_class_new_instance(2, param_argv, param_class);

            rb_ary_push(parameters, param);
        }

        VALUE signature = rb_class_new_instance(1, &parameters, cSignature);

        rb_ary_push(signatures, signature);
    }

    rdx_definition_signatures_free(arr);
    return signatures;
}

void rdxi_initialize_signature(VALUE mRubydex) {
    cSignature = rb_define_class_under(mRubydex, "Signature", rb_cObject);

    cParameter = rb_define_class_under(cSignature, "Parameter", rb_cObject);
    cPositionalParameter = rb_define_class_under(cSignature, "PositionalParameter", cParameter);
    cOptionalPositionalParameter = rb_define_class_under(cSignature, "OptionalPositionalParameter", cParameter);
    cRestPositionalParameter = rb_define_class_under(cSignature, "RestPositionalParameter", cParameter);
    cPostParameter = rb_define_class_under(cSignature, "PostParameter", cParameter);
    cKeywordParameter = rb_define_class_under(cSignature, "KeywordParameter", cParameter);
    cOptionalKeywordParameter = rb_define_class_under(cSignature, "OptionalKeywordParameter", cParameter);
    cRestKeywordParameter = rb_define_class_under(cSignature, "RestKeywordParameter", cParameter);
    cForwardParameter = rb_define_class_under(cSignature, "ForwardParameter", cParameter);
    cBlockParameter = rb_define_class_under(cSignature, "BlockParameter", cParameter);
}
