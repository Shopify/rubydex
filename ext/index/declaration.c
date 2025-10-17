#include "graph.h"
#include "handle.h"
#include "ruby.h"
#include "rustbindings.h"

VALUE cDeclaration;

static VALUE rb_declaration_alloc(VALUE klass) {
    HandleData *data = ALLOC(HandleData);
    data->graph_obj = Qnil;
    data->id = 0;

    return TypedData_Wrap_Struct(klass, &handle_type, data);
}

static VALUE rb_declaration_initialize(VALUE self, VALUE graph_obj,
                                       VALUE id_val) {
    HandleData *data;
    TypedData_Get_Struct(self, HandleData, &handle_type, data);
    data->graph_obj = graph_obj;
    data->id = NUM2LL(id_val);

    return self;
}

// DeclarationHandle#name -> String
static VALUE rb_declaration_name(VALUE self) {
    HandleData *data;
    TypedData_Get_Struct(self, HandleData, &handle_type, data);

    void *graph;
    TypedData_Get_Struct(data->graph_obj, void *, &graph_type, graph);
    const char *name = idx_declaration_name(graph, (int64_t)data->id);

    if (name == NULL) {
        return Qnil;
    }

    VALUE str = rb_utf8_str_new_cstr(name);
    free_c_string(name);

    return str;
}

void initialize_declaration(VALUE mIndex) {
    cDeclaration = rb_define_class_under(mIndex, "Declaration", rb_cObject);

    rb_define_alloc_func(cDeclaration, rb_declaration_alloc);
    rb_define_method(cDeclaration, "initialize", rb_declaration_initialize, 2);
    rb_define_method(cDeclaration, "name", rb_declaration_name, 0);
}
