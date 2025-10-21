#include "declaration.h"
#include "handle.h"

VALUE cDeclaration;

void initialize_declaration(VALUE mSaturn) {
    cDeclaration = rb_define_class_under(mSaturn, "Declaration", rb_cObject);

    rb_define_alloc_func(cDeclaration, sr_handle_alloc);
    rb_define_private_method(cDeclaration, "initialize", sr_handle_initialize, 2);

    rb_undef_method(CLASS_OF(cDeclaration), "new");
}
