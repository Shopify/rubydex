#include "graph.h"
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

// DefinitionHandle#kind -> String
static VALUE rb_definition_kind(VALUE self) {
    HandleData *data;
    TypedData_Get_Struct(self, HandleData, &handle_type, data);

    void *graph;
    TypedData_Get_Struct(data->graph_obj, void *, &graph_type, graph);

    const char *kind = idx_definition_kind(graph, (int64_t)data->id);

    if (kind == NULL) {
        return Qnil;
    }

    VALUE str = rb_utf8_str_new_cstr(kind);
    free_c_string(kind);

    return str;
}

void initialize_definition(VALUE mIndex) {
    cDefinition = rb_define_class_under(mIndex, "Definition", rb_cObject);

    rb_define_alloc_func(cDefinition, rb_definition_alloc);
    rb_define_method(cDefinition, "initialize", rb_definition_initialize, 2);
    rb_define_method(cDefinition, "kind", rb_definition_kind, 0);
}
