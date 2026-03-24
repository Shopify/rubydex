#include "signature.h"
#include "definition.h"
#include "handle.h"
#include "location.h"

VALUE cSignature;
VALUE cParameter;
VALUE cPositionalParameter;
VALUE cOptionalPositionalParameter;
VALUE cRestPositionalParameter;
VALUE cKeywordParameter;
VALUE cOptionalKeywordParameter;
VALUE cRestKeywordParameter;
VALUE cBlockParameter;
VALUE cForwardParameter;

static VALUE parameter_class_for_kind(ParameterKind kind) {
    switch (kind) {
    case ParameterKind_Req:     return cPositionalParameter;
    case ParameterKind_Opt:     return cOptionalPositionalParameter;
    case ParameterKind_Rest:    return cRestPositionalParameter;
    case ParameterKind_Keyreq:  return cKeywordParameter;
    case ParameterKind_Key:     return cOptionalKeywordParameter;
    case ParameterKind_Keyrest: return cRestKeywordParameter;
    case ParameterKind_Block:   return cBlockParameter;
    case ParameterKind_Forward: return cForwardParameter;
    default: rb_raise(rb_eRuntimeError, "Unknown ParameterKind: %d", kind);
    }
}

VALUE rdxi_signatures_to_ruby(SignatureArray *arr, VALUE graph_obj, VALUE default_method_def) {
    if (arr == NULL || arr->len == 0) {
        if (arr != NULL) {
            rdx_definition_signatures_free(arr);
        }
        return rb_ary_new();
    }

    VALUE signatures = rb_ary_new_capa((long)arr->len);

    for (size_t i = 0; i < arr->len; i++) {
        SignatureEntry sig_entry = arr->items[i];

        VALUE method_def;
        if (default_method_def != Qnil) {
            method_def = default_method_def;
        } else {
            VALUE def_argv[] = {graph_obj, ULL2NUM(sig_entry.definition_id)};
            method_def = rb_class_new_instance(2, def_argv, cMethodDefinition);
        }

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

        VALUE sig_kwargs = rb_hash_new();
        rb_hash_aset(sig_kwargs, ID2SYM(rb_intern("parameters")), parameters);
        rb_hash_aset(sig_kwargs, ID2SYM(rb_intern("method_definition")), method_def);
        VALUE signature = rb_class_new_instance_kw(1, &sig_kwargs, cSignature, RB_PASS_KEYWORDS);

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
    cKeywordParameter = rb_define_class_under(cSignature, "KeywordParameter", cParameter);
    cOptionalKeywordParameter = rb_define_class_under(cSignature, "OptionalKeywordParameter", cParameter);
    cRestKeywordParameter = rb_define_class_under(cSignature, "RestKeywordParameter", cParameter);
    cBlockParameter = rb_define_class_under(cSignature, "BlockParameter", cParameter);
    cForwardParameter = rb_define_class_under(cSignature, "ForwardParameter", cParameter);
}
