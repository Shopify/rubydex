#include "signature.h"
#include "definition.h"
#include "handle.h"
#include "location.h"

VALUE cSignature;

static VALUE parameter_kind_to_symbol(ParameterKind kind) {
    switch (kind) {
    case ParameterKind_Req:     return ID2SYM(rb_intern("req"));
    case ParameterKind_Opt:     return ID2SYM(rb_intern("opt"));
    case ParameterKind_Rest:    return ID2SYM(rb_intern("rest"));
    case ParameterKind_Keyreq:  return ID2SYM(rb_intern("keyreq"));
    case ParameterKind_Key:     return ID2SYM(rb_intern("key"));
    case ParameterKind_Keyrest: return ID2SYM(rb_intern("keyrest"));
    case ParameterKind_Block:   return ID2SYM(rb_intern("block"));
    case ParameterKind_Forward: return ID2SYM(rb_intern("forward"));
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

            VALUE kind_sym = parameter_kind_to_symbol(param_entry.kind);
            VALUE name_sym = ID2SYM(rb_intern(param_entry.name));
            VALUE location = rdxi_build_location_value(param_entry.location);

            rb_ary_push(parameters, rb_ary_new_from_args(3, kind_sym, name_sym, location));
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
}
