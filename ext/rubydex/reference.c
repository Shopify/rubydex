#include "reference.h"
#include "graph.h"
#include "handle.h"
#include "location.h"
#include "rustbindings.h"

VALUE cReference;
VALUE cConstantReference;
VALUE cMethodReference;

// ConstantReference#name -> String
static VALUE sr_constant_reference_name(VALUE self) {
    HandleData *data;
    TypedData_Get_Struct(self, HandleData, &handle_type, data);

    void *graph;
    TypedData_Get_Struct(data->graph_obj, void *, &graph_type, graph);

    const char *name = sat_constant_reference_name(graph, data->id);
    if (name == NULL) {
        return Qnil;
    }

    VALUE str = rb_utf8_str_new_cstr(name);
    free_c_string(name);
    return str;
}

// ConstantReference#location -> Rubydex::Location
static VALUE sr_constant_reference_location(VALUE self) {
    HandleData *data;
    TypedData_Get_Struct(self, HandleData, &handle_type, data);

    void *graph;
    TypedData_Get_Struct(data->graph_obj, void *, &graph_type, graph);

    Location *loc = sat_constant_reference_location(graph, data->id);
    VALUE location = build_location_value(loc);
    sat_location_free(loc);
    return location;
}

// MethodReference#name -> String
static VALUE sr_method_reference_name(VALUE self) {
    HandleData *data;
    TypedData_Get_Struct(self, HandleData, &handle_type, data);

    void *graph;
    TypedData_Get_Struct(data->graph_obj, void *, &graph_type, graph);

    const char *name = sat_method_reference_name(graph, data->id);
    if (name == NULL) {
        return Qnil;
    }

    VALUE str = rb_utf8_str_new_cstr(name);
    free_c_string(name);
    return str;
}

// MethodReference#location -> Rubydex::Location
static VALUE sr_method_reference_location(VALUE self) {
    HandleData *data;
    TypedData_Get_Struct(self, HandleData, &handle_type, data);

    void *graph;
    TypedData_Get_Struct(data->graph_obj, void *, &graph_type, graph);

    Location *loc = sat_method_reference_location(graph, data->id);
    VALUE location = build_location_value(loc);
    sat_location_free(loc);
    return location;
}

// Keep this in sync with unresolved_reference_api.rs
VALUE reference_class_for_kind(ReferenceKind kind) {
    switch (kind) {
    case ReferenceKind_Constant:
        return cConstantReference;
    case ReferenceKind_Method:
        return cMethodReference;
    default:
        rb_raise(rb_eRuntimeError, "Unknown UnresolvedReferenceKind: %d", kind);
    }
}

void initialize_reference(VALUE mRubydex) {
    cReference = rb_define_class_under(mRubydex, "Reference", rb_cObject);
    rb_define_alloc_func(cReference, sr_handle_alloc);
    rb_define_method(cReference, "initialize", sr_handle_initialize, 2);
    rb_funcall(rb_singleton_class(cReference), rb_intern("private"), 1, ID2SYM(rb_intern("new")));

    cConstantReference = rb_define_class_under(mRubydex, "ConstantReference", cReference);
    rb_define_alloc_func(cConstantReference, sr_handle_alloc);
    rb_define_method(cConstantReference, "initialize", sr_handle_initialize, 2);
    rb_define_method(cConstantReference, "name", sr_constant_reference_name, 0);
    rb_define_method(cConstantReference, "location", sr_constant_reference_location, 0);

    cMethodReference = rb_define_class_under(mRubydex, "MethodReference", cReference);
    rb_define_alloc_func(cMethodReference, sr_handle_alloc);
    rb_define_method(cMethodReference, "initialize", sr_handle_initialize, 2);
    rb_define_method(cMethodReference, "name", sr_method_reference_name, 0);
    rb_define_method(cMethodReference, "location", sr_method_reference_location, 0);
}
