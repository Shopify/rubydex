#ifndef INDEX_RESULT_H
#define INDEX_RESULT_H

#include "ruby.h"

typedef void *IndexResultPointer;

extern VALUE cIndexResult;
extern const rb_data_type_t index_result_type;

void rdxi_initialize_index_result(VALUE mRubydex);

#endif
