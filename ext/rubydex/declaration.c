#include "declaration.h"
#include "definition.h"
#include "graph.h"
#include "handle.h"
#include "rustbindings.h"

VALUE cDeclaration;

// Declaration#name -> String
static VALUE rdxr_declaration_name(VALUE self) {
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
static VALUE rdxr_declaration_unqualified_name(VALUE self) {
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
        VALUE defn_class = rdxi_definition_class_for_kind(kind);
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
static VALUE rdxr_declaration_definitions(VALUE self) {
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

// Declaration#member: (String member) -> Declaration
// Returns a declaration handle for the given member
static VALUE rdxr_declaration_member(VALUE self, VALUE name) {
    HandleData *data;
    TypedData_Get_Struct(self, HandleData, &handle_type, data);

    void *graph;
    TypedData_Get_Struct(data->graph_obj, void *, &graph_type, graph);

    if (TYPE(name) != T_STRING) {
        rb_raise(rb_eTypeError, "expected String");
    }

    const int64_t *id_ptr = rdx_declaration_member(graph, data->id, StringValueCStr(name));
    if (id_ptr == NULL) {
        return Qnil;
    }

    int64_t id = *id_ptr;
    free_i64(id_ptr);
    VALUE argv[] = {data->graph_obj, LL2NUM(id)};

    return rb_class_new_instance(2, argv, cDeclaration);
}

void rdxi_initialize_declaration(VALUE mRubydex) {
    cDeclaration = rb_define_class_under(mRubydex, "Declaration", rb_cObject);

    rb_define_alloc_func(cDeclaration, rdxr_handle_alloc);
    rb_define_method(cDeclaration, "initialize", rdxr_handle_initialize, 2);
    rb_define_method(cDeclaration, "name", rdxr_declaration_name, 0);
    rb_define_method(cDeclaration, "unqualified_name", rdxr_declaration_unqualified_name, 0);
    rb_define_method(cDeclaration, "definitions", rdxr_declaration_definitions, 0);
    rb_define_method(cDeclaration, "member", rdxr_declaration_member, 1);

    rb_funcall(rb_singleton_class(cDeclaration), rb_intern("private"), 1, ID2SYM(rb_intern("new")));
}
