#include "location.h"

VALUE cLocation;

VALUE build_location_value(Location *loc) {
    if (loc == NULL) {
        return Qnil;
    }

    VALUE uri = rb_utf8_str_new_cstr(loc->uri);

    VALUE kwargs = rb_hash_new_capa(5);
    rb_hash_aset(kwargs, ID2SYM(rb_intern("uri")), uri);
    rb_hash_aset(kwargs, ID2SYM(rb_intern("start_line")), UINT2NUM(loc->start_line));
    rb_hash_aset(kwargs, ID2SYM(rb_intern("end_line")), UINT2NUM(loc->end_line));
    rb_hash_aset(kwargs, ID2SYM(rb_intern("start_column")), UINT2NUM(loc->start_column));
    rb_hash_aset(kwargs, ID2SYM(rb_intern("end_column")), UINT2NUM(loc->end_column));

    return rb_class_new_instance_kw(1, &kwargs, cLocation, RB_PASS_KEYWORDS);
}

void initialize_location(VALUE mSaturn) { cLocation = rb_define_class_under(mSaturn, "Location", rb_cObject); }
