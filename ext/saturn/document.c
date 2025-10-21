#include "document.h"
#include "handle.h"

VALUE cDocument;

void initialize_document(VALUE mSaturn) {
    cDocument = rb_define_class_under(mSaturn, "Document", rb_cObject);

    rb_define_alloc_func(cDocument, sr_handle_alloc);
    rb_define_private_method(cDocument, "initialize", sr_handle_initialize, 2);

    rb_undef_method(CLASS_OF(cDocument), "new");
}
