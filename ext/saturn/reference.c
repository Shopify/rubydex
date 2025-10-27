#include "reference.h"
#include "graph.h"
#include "handle.h"
#include "location.h"
#include "rustbindings.h"

VALUE cUnresolvedReference;
VALUE cUnresolvedConstantReference;
VALUE cUnresolvedMethodReference;

// UnresolvedReference#name -> String
static VALUE sr_unresolved_reference_name(VALUE self) {
    HandleData *data;
    TypedData_Get_Struct(self, HandleData, &handle_type, data);

    void *graph;
    TypedData_Get_Struct(data->graph_obj, void *, &graph_type, graph);

    const char *name = sat_unresolved_reference_name(graph, data->id);
    if (name == NULL) {
        return Qnil;
    }

    VALUE str = rb_utf8_str_new_cstr(name);
    free_c_string(name);
    return str;
}

// UnresolvedReference#location -> Saturn::Location
static VALUE sr_unresolved_reference_location(VALUE self) {
    HandleData *data;
    TypedData_Get_Struct(self, HandleData, &handle_type, data);

    void *graph;
    TypedData_Get_Struct(data->graph_obj, void *, &graph_type, graph);

    Location *loc = sat_unresolved_reference_location(graph, data->id);
    VALUE location = build_location_value(loc);
    sat_location_free(loc);
    return location;
}

// Keep this in sync with unresolved_reference_api.rs
VALUE reference_class_for_kind(UnresolvedReferenceKind kind) {
    switch (kind) {
    case UnresolvedReferenceKind_Constant:
        return cUnresolvedConstantReference;
    case UnresolvedReferenceKind_Method:
        return cUnresolvedMethodReference;
    default:
        rb_raise(rb_eRuntimeError, "Unknown UnresolvedReferenceKind: %d", kind);
    }
}

void initialize_reference(VALUE mSaturn) {
    cUnresolvedReference = rb_define_class_under(mSaturn, "UnresolvedReference", rb_cObject);
    rb_define_alloc_func(cUnresolvedReference, sr_handle_alloc);
    rb_define_method(cUnresolvedReference, "initialize", sr_handle_initialize, 2);
    rb_funcall(rb_singleton_class(cUnresolvedReference), rb_intern("private"), 1, ID2SYM(rb_intern("new")));
    rb_define_method(cUnresolvedReference, "name", sr_unresolved_reference_name, 0);
    rb_define_method(cUnresolvedReference, "location", sr_unresolved_reference_location, 0);

    cUnresolvedConstantReference = rb_define_class_under(mSaturn, "UnresolvedConstantReference", cUnresolvedReference);
    rb_define_alloc_func(cUnresolvedConstantReference, sr_handle_alloc);
    rb_define_method(cUnresolvedConstantReference, "initialize", sr_handle_initialize, 2);

    cUnresolvedMethodReference = rb_define_class_under(mSaturn, "UnresolvedMethodReference", cUnresolvedReference);
    rb_define_alloc_func(cUnresolvedMethodReference, sr_handle_alloc);
    rb_define_method(cUnresolvedMethodReference, "initialize", sr_handle_initialize, 2);
}
