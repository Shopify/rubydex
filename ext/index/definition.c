#include "handle.h"
#include "ruby.h"
#include "rustbindings.h"

VALUE cDefinition;

static VALUE rb_definition_alloc(VALUE klass) {
    HandleData *data = ALLOC(HandleData);
    data->graph_obj = Qnil;
    data->id = 0;

    return TypedData_Wrap_Struct(klass, &handle_type, data);
}

static VALUE rb_definition_initialize(VALUE self, VALUE graph_obj,
                                      VALUE id_val) {
    HandleData *data;
    TypedData_Get_Struct(self, HandleData, &handle_type, data);
    data->graph_obj = graph_obj;
    data->id = NUM2LL(id_val);

    return self;
}

void initialize_definition(VALUE mIndex) {
    cDefinition = rb_define_class_under(mIndex, "Definition", rb_cObject);

    rb_define_alloc_func(cDefinition, rb_definition_alloc);
    rb_define_method(cDefinition, "initialize", rb_definition_initialize, 2);
}
