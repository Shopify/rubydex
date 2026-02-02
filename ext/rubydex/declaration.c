#include "declaration.h"
#include "definition.h"
#include "graph.h"
#include "handle.h"
#include "rustbindings.h"

VALUE cDeclaration;
VALUE cNamespace;
VALUE cClass;
VALUE cModule;
VALUE cSingletonClass;
VALUE cConstant;
VALUE cConstantAlias;
VALUE cMethod;
VALUE cGlobalVariable;
VALUE cInstanceVariable;
VALUE cClassVariable;

// Keep this in sync with declaration_api.rs
VALUE rdxi_declaration_class_for_kind(CDeclarationKind kind) {
    switch (kind) {
    case CDeclarationKind_Class:
        return cClass;
    case CDeclarationKind_Module:
        return cModule;
    case CDeclarationKind_SingletonClass:
        return cSingletonClass;
    case CDeclarationKind_Constant:
        return cConstant;
    case CDeclarationKind_ConstantAlias:
        return cConstantAlias;
    case CDeclarationKind_Method:
        return cMethod;
    case CDeclarationKind_GlobalVariable:
        return cGlobalVariable;
    case CDeclarationKind_InstanceVariable:
        return cInstanceVariable;
    case CDeclarationKind_ClassVariable:
        return cClassVariable;
    default:
        rb_raise(rb_eRuntimeError, "Unknown CDeclarationKind: %d", kind);
    }
}

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

    uint32_t id = 0;
    DefinitionKind kind;
    while (rdx_definitions_iter_next(iter, &id, &kind)) {
        VALUE argv[] = {data->graph_obj, UINT2NUM(id)};
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

    const CDeclaration *decl = rdx_declaration_member(graph, data->id, StringValueCStr(name));
    if (decl == NULL) {
        return Qnil;
    }

    VALUE decl_class = rdxi_declaration_class_for_kind(decl->kind);
    VALUE argv[] = {data->graph_obj, UINT2NUM(decl->id)};
    free_c_declaration(decl);

    return rb_class_new_instance(2, argv, decl_class);
}

// Declaration#singleton_class -> SingletonClass
static VALUE rdxr_declaration_singleton_class(VALUE self) {
    HandleData *data;
    TypedData_Get_Struct(self, HandleData, &handle_type, data);

    void *graph;
    TypedData_Get_Struct(data->graph_obj, void *, &graph_type, graph);
    const CDeclaration *decl = rdx_declaration_singleton_class(graph, data->id);

    if (decl == NULL) {
        return Qnil;
    }

    VALUE decl_class = rdxi_declaration_class_for_kind(decl->kind);
    VALUE argv[] = {data->graph_obj, UINT2NUM(decl->id)};
    free_c_declaration(decl);

    return rb_class_new_instance(2, argv, decl_class);
}

// Declaration#owner -> Declaration
static VALUE rdxr_declaration_owner(VALUE self) {
    HandleData *data;
    TypedData_Get_Struct(self, HandleData, &handle_type, data);

    void *graph;
    TypedData_Get_Struct(data->graph_obj, void *, &graph_type, graph);
    const CDeclaration *decl = rdx_declaration_owner(graph, data->id);

    if (decl == NULL) {
        rb_raise(rb_eRuntimeError, "owner can never be nil for any declarations");
    }

    VALUE decl_class = rdxi_declaration_class_for_kind(decl->kind);
    VALUE argv[] = {data->graph_obj, UINT2NUM(decl->id)};
    free_c_declaration(decl);

    return rb_class_new_instance(2, argv, decl_class);
}

void rdxi_initialize_declaration(VALUE mRubydex) {
    cDeclaration = rb_define_class_under(mRubydex, "Declaration", rb_cObject);
    cNamespace = rb_define_class_under(mRubydex, "Namespace", cDeclaration);
    cClass = rb_define_class_under(mRubydex, "Class", cNamespace);
    cModule = rb_define_class_under(mRubydex, "Module", cNamespace);
    cSingletonClass = rb_define_class_under(mRubydex, "SingletonClass", cNamespace);
    cConstant = rb_define_class_under(mRubydex, "Constant", cDeclaration);
    cConstantAlias = rb_define_class_under(mRubydex, "ConstantAlias", cDeclaration);
    cMethod = rb_define_class_under(mRubydex, "Method", cDeclaration);
    cGlobalVariable = rb_define_class_under(mRubydex, "GlobalVariable", cDeclaration);
    cInstanceVariable = rb_define_class_under(mRubydex, "InstanceVariable", cDeclaration);
    cClassVariable = rb_define_class_under(mRubydex, "ClassVariable", cDeclaration);

    rb_define_alloc_func(cDeclaration, rdxr_handle_alloc);
    rb_define_method(cDeclaration, "initialize", rdxr_handle_initialize, 2);
    rb_define_method(cDeclaration, "name", rdxr_declaration_name, 0);
    rb_define_method(cDeclaration, "unqualified_name", rdxr_declaration_unqualified_name, 0);
    rb_define_method(cDeclaration, "definitions", rdxr_declaration_definitions, 0);
    rb_define_method(cDeclaration, "owner", rdxr_declaration_owner, 0);

    // Namespace only methods
    rb_define_method(cNamespace, "member", rdxr_declaration_member, 1);
    rb_define_method(cNamespace, "singleton_class", rdxr_declaration_singleton_class, 0);

    rb_funcall(rb_singleton_class(cDeclaration), rb_intern("private"), 1, ID2SYM(rb_intern("new")));
}
