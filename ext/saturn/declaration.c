#include "declaration.h"
#include "graph.h"
#include "handle.h"
#include "rustbindings.h"

VALUE cDeclaration;

// Declaration#name -> String
static VALUE sr_declaration_name(VALUE self) {
    HandleData *data;
    TypedData_Get_Struct(self, HandleData, &handle_type, data);

    void *graph;
    TypedData_Get_Struct(data->graph_obj, void *, &graph_type, graph);
    const char *name = sat_declaration_name(graph, data->id);

    if (name == NULL) {
        return Qnil;
    }

    VALUE str = rb_utf8_str_new_cstr(name);
    free_c_string(name);

    return str;
}

void initialize_declaration(VALUE mSaturn) {
    cDeclaration = rb_define_class_under(mSaturn, "Declaration", rb_cObject);

    rb_define_alloc_func(cDeclaration, sr_handle_alloc);
    rb_define_private_method(cDeclaration, "initialize", sr_handle_initialize, 2);
    rb_define_method(cDeclaration, "name", sr_declaration_name, 0);

    rb_undef_method(CLASS_OF(cDeclaration), "new");
}
