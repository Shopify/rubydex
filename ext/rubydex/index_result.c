#include "index_result.h"
#include "rustbindings.h"

VALUE cIndexResult;

static void index_result_free(void *ptr) {
    if (ptr) {
        rdx_index_result_free(ptr);
    }
}

const rb_data_type_t index_result_type = {"IndexResult", {0, index_result_free, 0}, 0, 0, RUBY_TYPED_FREE_IMMEDIATELY};

// IndexResult#definition_ids_length -> Integer
static VALUE rdxr_index_result_definition_ids_length(VALUE self) {
    void *index_result;
    TypedData_Get_Struct(self, void *, &index_result_type, index_result);
    return SIZET2NUM(rdx_index_result_definition_ids_len(index_result));
}

// IndexResult#reference_ids_length -> Integer
static VALUE rdxr_index_result_reference_ids_length(VALUE self) {
    void *index_result;
    TypedData_Get_Struct(self, void *, &index_result_type, index_result);
    return SIZET2NUM(rdx_index_result_reference_ids_len(index_result));
}

void rdxi_initialize_index_result(VALUE mRubydex) {
    cIndexResult = rb_define_class_under(mRubydex, "IndexResult", rb_cObject);
    rb_undef_alloc_func(cIndexResult);
    rb_define_method(cIndexResult, "definition_ids_length", rdxr_index_result_definition_ids_length, 0);
    rb_define_method(cIndexResult, "reference_ids_length", rdxr_index_result_reference_ids_length, 0);
}
