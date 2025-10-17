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

// DefinitionHandle#name -> String
static VALUE rb_definition_name(VALUE self) {
    HandleData *data;
    TypedData_Get_Struct(self, HandleData, &handle_type, data);

    void *graph;
    TypedData_Get_Struct(data->graph_obj, void *, &graph_type, graph);

    const char *name = idx_definition_name(graph, (int64_t)data->id);
    if (name == NULL) {
        return Qnil;
    }
    VALUE str = rb_utf8_str_new_cstr(name);
    free_c_string(name);
    return str;
}

// DefinitionHandle#uri_path -> String
static VALUE rb_definition_uri_path(VALUE self) {
    HandleData *data;
    TypedData_Get_Struct(self, HandleData, &handle_type, data);

    void *graph;
    TypedData_Get_Struct(data->graph_obj, void *, &graph_type, graph);

    const char *uri = idx_definition_uri_path(graph, (int64_t)data->id);
    if (uri == NULL) {
        return Qnil;
    }
    VALUE str = rb_utf8_str_new_cstr(uri);
    free_c_string(uri);
    return str;
}

// DefinitionHandle#start_location -> Integer
static VALUE rb_definition_start_location(VALUE self) {
    HandleData *data;
    TypedData_Get_Struct(self, HandleData, &handle_type, data);

    void *graph;
    TypedData_Get_Struct(data->graph_obj, void *, &graph_type, graph);

    uint32_t start = idx_definition_start_location(graph, (int64_t)data->id);
    return UINT2NUM(start);
}

// DefinitionHandle#end_location -> Integer
static VALUE rb_definition_end_location(VALUE self) {
    HandleData *data;
    TypedData_Get_Struct(self, HandleData, &handle_type, data);

    void *graph;
    TypedData_Get_Struct(data->graph_obj, void *, &graph_type, graph);

    uint32_t end = idx_definition_end_location(graph, (int64_t)data->id);
    return UINT2NUM(end);
}

// DefinitionHandle#comments -> String
static VALUE rb_definition_comments(VALUE self) {
    HandleData *data;
    TypedData_Get_Struct(self, HandleData, &handle_type, data);

    void *graph;
    TypedData_Get_Struct(data->graph_obj, void *, &graph_type, graph);

    const char *comments = idx_definition_comments(graph, (int64_t)data->id);
    if (comments == NULL) {
        return rb_str_new("", 0);
    }
    VALUE str = rb_utf8_str_new_cstr(comments);
    free_c_string(comments);
    return str;
}

void initialize_definition(VALUE mIndex) {
    cDefinition = rb_define_class_under(mIndex, "Definition", rb_cObject);

    rb_define_alloc_func(cDefinition, rb_definition_alloc);
    rb_define_method(cDefinition, "initialize", rb_definition_initialize, 2);
    rb_define_method(cDefinition, "kind", rb_definition_kind, 0);
    rb_define_method(cDefinition, "name", rb_definition_name, 0);
    rb_define_method(cDefinition, "uri_path", rb_definition_uri_path, 0);
    rb_define_method(cDefinition, "start_location",
                     rb_definition_start_location, 0);
    rb_define_method(cDefinition, "end_location", rb_definition_end_location,
                     0);
    rb_define_method(cDefinition, "comments", rb_definition_comments, 0);
}
