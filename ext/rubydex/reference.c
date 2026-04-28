#include "reference.h"
#include "declaration.h"
#include "graph.h"
#include "handle.h"
#include "location.h"
#include "rustbindings.h"

VALUE cReference;
VALUE cConstantReference;
VALUE cUnresolvedConstantReference;
VALUE cResolvedConstantReference;
VALUE cMethodReference;

// ConstantReference#name -> String
static VALUE rdxr_constant_reference_name(VALUE self) {
    HandleData *data;
    TypedData_Get_Struct(self, HandleData, &handle_type, data);

    void *graph;
    TypedData_Get_Struct(data->graph_obj, void *, &graph_type, graph);

    const char *name = rdx_constant_reference_name(graph, data->id);
    if (name == NULL) {
        return Qnil;
    }

    VALUE str = rb_utf8_str_new_cstr(name);
    free_c_string(name);
    return str;
}

// ConstantReference#location -> Rubydex::Location
static VALUE rdxr_constant_reference_location(VALUE self) {
    HandleData *data;
    TypedData_Get_Struct(self, HandleData, &handle_type, data);

    void *graph;
    TypedData_Get_Struct(data->graph_obj, void *, &graph_type, graph);

    Location *loc = rdx_constant_reference_location(graph, data->id);
    VALUE location = rdxi_build_location_value(loc);
    rdx_location_free(loc);
    return location;
}

// MethodReference#name -> String
static VALUE rdxr_method_reference_name(VALUE self) {
    HandleData *data;
    TypedData_Get_Struct(self, HandleData, &handle_type, data);

    void *graph;
    TypedData_Get_Struct(data->graph_obj, void *, &graph_type, graph);

    const char *name = rdx_method_reference_name(graph, data->id);
    if (name == NULL) {
        return Qnil;
    }

    VALUE str = rb_utf8_str_new_cstr(name);
    free_c_string(name);
    return str;
}

// MethodReference#location -> Rubydex::Location
static VALUE rdxr_method_reference_location(VALUE self) {
    HandleData *data;
    TypedData_Get_Struct(self, HandleData, &handle_type, data);

    void *graph;
    TypedData_Get_Struct(data->graph_obj, void *, &graph_type, graph);

    Location *loc = rdx_method_reference_location(graph, data->id);
    VALUE location = rdxi_build_location_value(loc);
    rdx_location_free(loc);
    return location;
}

// MethodReference#receiver -> Rubydex::Declaration?
// Returns the resolved declaration for the receiver of the method call. Returns nil when the receiver is not a
// tracked constant or cannot be resolved.
static VALUE rdxr_method_reference_receiver(VALUE self) {
    HandleData *data;
    TypedData_Get_Struct(self, HandleData, &handle_type, data);

    void *graph;
    TypedData_Get_Struct(data->graph_obj, void *, &graph_type, graph);

    const struct CDeclaration *decl = rdx_method_reference_receiver_declaration(graph, data->id);
    if (decl == NULL) {
        return Qnil;
    }

    VALUE decl_class = rdxi_declaration_class_for_kind(decl->kind);
    VALUE argv[] = {data->graph_obj, ULL2NUM(decl->id)};
    free_c_declaration(decl);

    return rb_class_new_instance(2, argv, decl_class);
}

// ResolvedConstantReference#declaration -> Declaration
static VALUE rdxr_resolved_constant_reference_declaration(VALUE self) {
    HandleData *data;
    TypedData_Get_Struct(self, HandleData, &handle_type, data);

    void *graph;
    TypedData_Get_Struct(data->graph_obj, void *, &graph_type, graph);

    const struct CDeclaration *decl = rdx_resolved_constant_reference_declaration(graph, data->id);
    if (decl == NULL) {
        rb_raise(rb_eRuntimeError, "Invalid declaration for a resolved constant reference");
    }

    VALUE decl_class = rdxi_declaration_class_for_kind(decl->kind);
    VALUE argv[] = {data->graph_obj, ULL2NUM(decl->id)};
    free_c_declaration(decl);

    return rb_class_new_instance(2, argv, decl_class);
}

void rdxi_initialize_reference(VALUE mRubydex) {
    cReference = rb_define_class_under(mRubydex, "Reference", rb_cObject);
    rb_define_alloc_func(cReference, rdxr_handle_alloc);
    rb_define_method(cReference, "initialize", rdxr_handle_initialize, 2);
    rb_funcall(rb_singleton_class(cReference), rb_intern("private"), 1, ID2SYM(rb_intern("new")));

    cConstantReference = rb_define_class_under(mRubydex, "ConstantReference", cReference);
    rb_define_alloc_func(cConstantReference, rdxr_handle_alloc);
    rb_define_method(cConstantReference, "initialize", rdxr_handle_initialize, 2);
    rb_define_method(cConstantReference, "location", rdxr_constant_reference_location, 0);
    rb_funcall(rb_singleton_class(cConstantReference), rb_intern("private"), 1, ID2SYM(rb_intern("new")));

    cUnresolvedConstantReference = rb_define_class_under(mRubydex, "UnresolvedConstantReference", cConstantReference);
    rb_define_alloc_func(cUnresolvedConstantReference, rdxr_handle_alloc);
    rb_define_method(cUnresolvedConstantReference, "name", rdxr_constant_reference_name, 0);

    cResolvedConstantReference = rb_define_class_under(mRubydex, "ResolvedConstantReference", cConstantReference);
    rb_define_alloc_func(cResolvedConstantReference, rdxr_handle_alloc);
    rb_define_method(cResolvedConstantReference, "declaration", rdxr_resolved_constant_reference_declaration, 0);

    cMethodReference = rb_define_class_under(mRubydex, "MethodReference", cReference);
    rb_define_alloc_func(cMethodReference, rdxr_handle_alloc);
    rb_define_method(cMethodReference, "initialize", rdxr_handle_initialize, 2);
    rb_define_method(cMethodReference, "name", rdxr_method_reference_name, 0);
    rb_define_method(cMethodReference, "location", rdxr_method_reference_location, 0);
    rb_define_method(cMethodReference, "receiver", rdxr_method_reference_receiver, 0);
}
