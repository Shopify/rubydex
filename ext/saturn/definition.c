#include "definition.h"
#include "handle.h"

VALUE cDefinition;

void initialize_definition(VALUE mSaturn) {
    cDefinition = rb_define_class_under(mSaturn, "Definition", rb_cObject);

    rb_define_alloc_func(cDefinition, sr_handle_alloc);
    rb_define_private_method(cDefinition, "initialize", sr_handle_initialize, 2);

    rb_undef_method(CLASS_OF(cDefinition), "new");
}
