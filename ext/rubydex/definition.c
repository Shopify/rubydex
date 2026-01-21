#include "definition.h"
#include "graph.h"
#include "handle.h"
#include "location.h"
#include "ruby/internal/scan_args.h"
#include "rustbindings.h"

static VALUE mRubydex;
VALUE cComment;
VALUE cDefinition;
VALUE cClassDefinition;
VALUE cSingletonClassDefinition;
VALUE cModuleDefinition;
VALUE cConstantDefinition;
VALUE cConstantAliasDefinition;
VALUE cMethodDefinition;
VALUE cAttrAccessorDefinition;
VALUE cAttrReaderDefinition;
VALUE cAttrWriterDefinition;
VALUE cGlobalVariableDefinition;
VALUE cInstanceVariableDefinition;
VALUE cClassVariableDefinition;
VALUE cMethodAliasDefinition;
VALUE cGlobalVariableAliasDefinition;

// Keep this in sync with definition.rs
VALUE rdxi_definition_class_for_kind(DefinitionKind kind) {
    switch (kind) {
    case DefinitionKind_Class:
        return cClassDefinition;
    case DefinitionKind_SingletonClass:
        return cSingletonClassDefinition;
    case DefinitionKind_Module:
        return cModuleDefinition;
    case DefinitionKind_Constant:
        return cConstantDefinition;
    case DefinitionKind_ConstantAlias:
        return cConstantAliasDefinition;
    case DefinitionKind_Method:
        return cMethodDefinition;
    case DefinitionKind_AttrAccessor:
        return cAttrAccessorDefinition;
    case DefinitionKind_AttrReader:
        return cAttrReaderDefinition;
    case DefinitionKind_AttrWriter:
        return cAttrWriterDefinition;
    case DefinitionKind_GlobalVariable:
        return cGlobalVariableDefinition;
    case DefinitionKind_InstanceVariable:
        return cInstanceVariableDefinition;
    case DefinitionKind_ClassVariable:
        return cClassVariableDefinition;
    case DefinitionKind_MethodAlias:
        return cMethodAliasDefinition;
    case DefinitionKind_GlobalVariableAlias:
        return cGlobalVariableAliasDefinition;
    default:
        rb_raise(rb_eRuntimeError, "Unknown DefinitionKind: %d", kind);
    }
}

// Definition#location -> Rubydex::Location
static VALUE rdxr_definition_location(VALUE self) {
    HandleData *data;
    TypedData_Get_Struct(self, HandleData, &handle_type, data);

    void *graph;
    TypedData_Get_Struct(data->graph_obj, void *, &graph_type, graph);

    Location *loc = rdx_definition_location(graph, data->id);
    VALUE location = rdxi_build_location_value(loc);
    rdx_location_free(loc);

    return location;
}

// Definition#comments -> [Rubydex::Comment]
static VALUE rdxr_definition_comments(VALUE self) {
    HandleData *data;
    TypedData_Get_Struct(self, HandleData, &handle_type, data);

    void *graph;
    TypedData_Get_Struct(data->graph_obj, void *, &graph_type, graph);

    CommentArray *arr = rdx_definition_comments(graph, data->id);
    if (arr == NULL || arr->len == 0) {
        if (arr != NULL) {
            rdx_definition_comments_free(arr);
        }
        return rb_ary_new();
    }

    VALUE ary = rb_ary_new_capa((long)arr->len);
    for (size_t i = 0; i < arr->len; i++) {
        CommentEntry entry = arr->items[i];

        VALUE string = rb_utf8_str_new_cstr(entry.string);

        Location *loc = entry.location;
        VALUE location = rdxi_build_location_value(loc);

        VALUE comment_kwargs = rb_hash_new();
        rb_hash_aset(comment_kwargs, ID2SYM(rb_intern("string")), string);
        rb_hash_aset(comment_kwargs, ID2SYM(rb_intern("location")), location);
        VALUE comment = rb_class_new_instance_kw(1, &comment_kwargs, cComment, RB_PASS_KEYWORDS);

        rb_ary_push(ary, comment);
    }

    // Free the array and all inner allocations on the Rust side
    rdx_definition_comments_free(arr);
    return ary;
}

// Definition#name -> String
static VALUE rdxr_definition_name(VALUE self) {
    HandleData *data;
    TypedData_Get_Struct(self, HandleData, &handle_type, data);

    void *graph;
    TypedData_Get_Struct(data->graph_obj, void *, &graph_type, graph);

    const char *name = rdx_definition_name(graph, data->id);
    if (name == NULL) {
        return Qnil;
    }
    VALUE str = rb_utf8_str_new_cstr(name);
    free_c_string(name);
    return str;
}

// Definition#deprecated? -> bool
static VALUE rdxr_definition_deprecated(VALUE self) {
    HandleData *data;
    TypedData_Get_Struct(self, HandleData, &handle_type, data);

    void *graph;
    TypedData_Get_Struct(data->graph_obj, void *, &graph_type, graph);

    bool deprecated = rdx_definition_is_deprecated(graph, data->id);
    return deprecated ? Qtrue : Qfalse;
}

void rdxi_initialize_definition(VALUE mod) {
    mRubydex = mod;

    cComment = rb_define_class_under(mRubydex, "Comment", rb_cObject);

    cDefinition = rb_define_class_under(mRubydex, "Definition", rb_cObject);
    rb_define_alloc_func(cDefinition, rdxr_handle_alloc);
    rb_define_method(cDefinition, "initialize", rdxr_handle_initialize, 2);
    rb_funcall(rb_singleton_class(cDefinition), rb_intern("private"), 1, ID2SYM(rb_intern("new")));
    rb_define_method(cDefinition, "location", rdxr_definition_location, 0);
    rb_define_method(cDefinition, "comments", rdxr_definition_comments, 0);
    rb_define_method(cDefinition, "name", rdxr_definition_name, 0);
    rb_define_method(cDefinition, "deprecated?", rdxr_definition_deprecated, 0);

    cClassDefinition = rb_define_class_under(mRubydex, "ClassDefinition", cDefinition);
    rb_define_alloc_func(cClassDefinition, rdxr_handle_alloc);
    rb_define_method(cClassDefinition, "initialize", rdxr_handle_initialize, 2);

    cSingletonClassDefinition = rb_define_class_under(mRubydex, "SingletonClassDefinition", cDefinition);
    rb_define_alloc_func(cSingletonClassDefinition, rdxr_handle_alloc);
    rb_define_method(cSingletonClassDefinition, "initialize", rdxr_handle_initialize, 2);

    cModuleDefinition = rb_define_class_under(mRubydex, "ModuleDefinition", cDefinition);
    rb_define_alloc_func(cModuleDefinition, rdxr_handle_alloc);
    rb_define_method(cModuleDefinition, "initialize", rdxr_handle_initialize, 2);

    cConstantDefinition = rb_define_class_under(mRubydex, "ConstantDefinition", cDefinition);
    rb_define_alloc_func(cConstantDefinition, rdxr_handle_alloc);
    rb_define_method(cConstantDefinition, "initialize", rdxr_handle_initialize, 2);

    cConstantAliasDefinition = rb_define_class_under(mRubydex, "ConstantAliasDefinition", cDefinition);
    rb_define_alloc_func(cConstantAliasDefinition, rdxr_handle_alloc);
    rb_define_method(cConstantAliasDefinition, "initialize", rdxr_handle_initialize, 2);

    cMethodDefinition = rb_define_class_under(mRubydex, "MethodDefinition", cDefinition);
    rb_define_alloc_func(cMethodDefinition, rdxr_handle_alloc);
    rb_define_method(cMethodDefinition, "initialize", rdxr_handle_initialize, 2);

    cAttrAccessorDefinition = rb_define_class_under(mRubydex, "AttrAccessorDefinition", cDefinition);
    rb_define_alloc_func(cAttrAccessorDefinition, rdxr_handle_alloc);
    rb_define_method(cAttrAccessorDefinition, "initialize", rdxr_handle_initialize, 2);

    cAttrReaderDefinition = rb_define_class_under(mRubydex, "AttrReaderDefinition", cDefinition);
    rb_define_alloc_func(cAttrReaderDefinition, rdxr_handle_alloc);
    rb_define_method(cAttrReaderDefinition, "initialize", rdxr_handle_initialize, 2);

    cAttrWriterDefinition = rb_define_class_under(mRubydex, "AttrWriterDefinition", cDefinition);
    rb_define_alloc_func(cAttrWriterDefinition, rdxr_handle_alloc);
    rb_define_method(cAttrWriterDefinition, "initialize", rdxr_handle_initialize, 2);

    cGlobalVariableDefinition = rb_define_class_under(mRubydex, "GlobalVariableDefinition", cDefinition);
    rb_define_alloc_func(cGlobalVariableDefinition, rdxr_handle_alloc);
    rb_define_method(cGlobalVariableDefinition, "initialize", rdxr_handle_initialize, 2);

    cInstanceVariableDefinition = rb_define_class_under(mRubydex, "InstanceVariableDefinition", cDefinition);
    rb_define_alloc_func(cInstanceVariableDefinition, rdxr_handle_alloc);
    rb_define_method(cInstanceVariableDefinition, "initialize", rdxr_handle_initialize, 2);

    cClassVariableDefinition = rb_define_class_under(mRubydex, "ClassVariableDefinition", cDefinition);
    rb_define_alloc_func(cClassVariableDefinition, rdxr_handle_alloc);
    rb_define_method(cClassVariableDefinition, "initialize", rdxr_handle_initialize, 2);

    cMethodAliasDefinition = rb_define_class_under(mRubydex, "MethodAliasDefinition", cDefinition);
    rb_define_alloc_func(cMethodAliasDefinition, rdxr_handle_alloc);
    rb_define_method(cMethodAliasDefinition, "initialize", rdxr_handle_initialize, 2);

    cGlobalVariableAliasDefinition = rb_define_class_under(mRubydex, "GlobalVariableAliasDefinition", cDefinition);
    rb_define_alloc_func(cGlobalVariableAliasDefinition, rdxr_handle_alloc);
    rb_define_method(cGlobalVariableAliasDefinition, "initialize", rdxr_handle_initialize, 2);
}
