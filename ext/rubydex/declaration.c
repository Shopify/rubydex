#include "declaration.h"
#include "definition.h"
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
    const char *name = rdx_declaration_name(graph, data->id);

    if (name == NULL) {
        return Qnil;
    }

    VALUE str = rb_utf8_str_new_cstr(name);
    free_c_string(name);

    return str;
}

// Declaration#unqualified_name -> String
static VALUE sr_declaration_unqualified_name(VALUE self) {
    HandleData *data;
    TypedData_Get_Struct(self, HandleData, &handle_type, data);

    void *graph;
    TypedData_Get_Struct(data->graph_obj, void *, &graph_type, graph);
    const char *name = rdx_declaration_unqualified_name(graph, data->id);

    if (name == NULL) {
        return Qnil;
    }

    VALUE str = rb_utf8_str_new_cstr(name);
    free_c_string(name);

    return str;
}

// Body function for rb_ensure in Declaration#definitions
static VALUE declaration_definitions_yield(VALUE args) {
    VALUE self = rb_ary_entry(args, 0);
    void *iter = (void *)(uintptr_t)NUM2ULL(rb_ary_entry(args, 1));

    HandleData *data;
    TypedData_Get_Struct(self, HandleData, &handle_type, data);

    int64_t id = 0;
    DefinitionKind kind;
    while (rdx_definitions_iter_next(iter, &id, &kind)) {
        VALUE argv[] = {data->graph_obj, LL2NUM(id)};
        VALUE defn_class = definition_class_for_kind(kind);
        VALUE handle = rb_class_new_instance(2, argv, defn_class);
        rb_yield(handle);
    }

    return Qnil;
}

// Ensure function for rb_ensure in Declaration#definitions to always free the
// iterator
static VALUE declaration_definitions_ensure(VALUE args) {
    void *iter = (void *)(uintptr_t)NUM2ULL(rb_ary_entry(args, 1));
    rdx_definitions_iter_free(iter);

    return Qnil;
}

// Size function for the Declaration#definitions enumerator
static VALUE declaration_definitions_size(VALUE self, VALUE _args, VALUE _eobj) {
    HandleData *data;
    TypedData_Get_Struct(self, HandleData, &handle_type, data);

    void *graph;
    TypedData_Get_Struct(data->graph_obj, void *, &graph_type, graph);
    struct DefinitionsIter *iter = rdx_declaration_definitions_iter_new(graph, data->id);
    size_t len = rdx_definitions_iter_len(iter);
    rdx_definitions_iter_free(iter);

    return SIZET2NUM(len);
}

// Declaration#definitions: () -> Enumerator[Definition]
// Returns an enumerator that yields all definitions for this declaration lazily
static VALUE sr_declaration_definitions(VALUE self) {
    if (!rb_block_given_p()) {
        return rb_enumeratorize_with_size(self, rb_str_new2("definitions"), 0, NULL, declaration_definitions_size);
    }

    HandleData *data;
    TypedData_Get_Struct(self, HandleData, &handle_type, data);

    void *graph;
    TypedData_Get_Struct(data->graph_obj, void *, &graph_type, graph);

    void *iter = rdx_declaration_definitions_iter_new(graph, data->id);
    VALUE args = rb_ary_new_from_args(2, self, ULL2NUM((uintptr_t)iter));
    rb_ensure(declaration_definitions_yield, args, declaration_definitions_ensure, args);

    return self;
}

// Body function for rb_ensure in Declaration#members
static VALUE declaration_members_yield(VALUE args) {
    VALUE self = rb_ary_entry(args, 0);
    void *iter = (void *)(uintptr_t)NUM2ULL(rb_ary_entry(args, 1));

    HandleData *data;
    TypedData_Get_Struct(self, HandleData, &handle_type, data);

    void *graph;
    TypedData_Get_Struct(data->graph_obj, void *, &graph_type, graph);

    int64_t id = 0;
    while (rdx_members_iter_next(iter, &id)) {
        VALUE argv[] = {data->graph_obj, LL2NUM(id)};
        VALUE handle = rb_class_new_instance(2, argv, cDeclaration);
        rb_yield(handle);
    }

    return Qnil;
}

// Ensure function to free iterator
static VALUE declaration_members_ensure(VALUE args) {
    void *iter = (void *)(uintptr_t)NUM2ULL(rb_ary_entry(args, 1));
    rdx_members_iter_free(iter);
    return Qnil;
}

// Size function for enumerator
static VALUE declaration_members_size(VALUE self, VALUE _args, VALUE _eobj) {
    HandleData *data;
    TypedData_Get_Struct(self, HandleData, &handle_type, data);

    void *graph;
    TypedData_Get_Struct(data->graph_obj, void *, &graph_type, graph);

    void *iter = rdx_declaration_members_iter_new(graph, data->id);
    size_t len = rdx_members_iter_len(iter);
    rdx_members_iter_free(iter);

    return SIZET2NUM(len);
}

// Declaration#members: () -> Enumerator[Declaration]
static VALUE sr_declaration_members(VALUE self) {
    if (!rb_block_given_p()) {
        return rb_enumeratorize_with_size(self, rb_str_new2("members"), 0, NULL, declaration_members_size);
    }

    HandleData *data;
    TypedData_Get_Struct(self, HandleData, &handle_type, data);

    void *graph;
    TypedData_Get_Struct(data->graph_obj, void *, &graph_type, graph);

    void *iter = rdx_declaration_members_iter_new(graph, data->id);
    VALUE args = rb_ary_new_from_args(2, self, ULL2NUM((uintptr_t)iter));
    rb_ensure(declaration_members_yield, args, declaration_members_ensure, args);

    return self;
}

void initialize_declaration(VALUE mRubydex) {
    cDeclaration = rb_define_class_under(mRubydex, "Declaration", rb_cObject);

    rb_define_alloc_func(cDeclaration, sr_handle_alloc);
    rb_define_method(cDeclaration, "initialize", sr_handle_initialize, 2);
    rb_define_method(cDeclaration, "name", sr_declaration_name, 0);
    rb_define_method(cDeclaration, "unqualified_name", sr_declaration_unqualified_name, 0);
    rb_define_method(cDeclaration, "definitions", sr_declaration_definitions, 0);
    rb_define_method(cDeclaration, "members", sr_declaration_members, 0);

    rb_funcall(rb_singleton_class(cDeclaration), rb_intern("private"), 1, ID2SYM(rb_intern("new")));
}
