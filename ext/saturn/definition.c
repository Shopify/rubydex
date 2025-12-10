#include "definition.h"
#include "graph.h"
#include "handle.h"
#include "location.h"
#include "ruby/internal/scan_args.h"
#include "rustbindings.h"

static VALUE mSaturn;
VALUE cComment;
VALUE cDefinition;
VALUE cClassDefinition;
VALUE cSingletonClassDefinition;
VALUE cModuleDefinition;
VALUE cConstantDefinition;
VALUE cMethodDefinition;
VALUE cAttrAccessorDefinition;
VALUE cAttrReaderDefinition;
VALUE cAttrWriterDefinition;
VALUE cGlobalVariableDefinition;
VALUE cInstanceVariableDefinition;
VALUE cClassVariableDefinition;
VALUE cMethodAliasDefinition;

// Keep this in sync with definition.rs
VALUE definition_class_for_kind(DefinitionKind kind) {
    switch (kind) {
    case DefinitionKind_Class:
        return cClassDefinition;
    case DefinitionKind_SingletonClass:
        return cSingletonClassDefinition;
    case DefinitionKind_Module:
        return cModuleDefinition;
    case DefinitionKind_Constant:
        return cConstantDefinition;
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
    default:
        rb_raise(rb_eRuntimeError, "Unknown DefinitionKind: %d", kind);
    }
}

// Definition#location -> Saturn::Location
static VALUE sr_definition_location(VALUE self) {
    HandleData *data;
    TypedData_Get_Struct(self, HandleData, &handle_type, data);

    void *graph;
    TypedData_Get_Struct(data->graph_obj, void *, &graph_type, graph);

    Location *loc = sat_definition_location(graph, data->id);
    VALUE location = build_location_value(loc);
    sat_location_free(loc);

    return location;
}

// Definition#comments -> [Saturn::Comment]
static VALUE sr_definition_comments(VALUE self) {
    HandleData *data;
    TypedData_Get_Struct(self, HandleData, &handle_type, data);

    void *graph;
    TypedData_Get_Struct(data->graph_obj, void *, &graph_type, graph);

    CommentArray *arr = sat_definition_comments(graph, data->id);
    if (arr == NULL || arr->len == 0) {
        if (arr != NULL) {
            sat_definition_comments_free(arr);
        }
        return rb_ary_new();
    }

    VALUE ary = rb_ary_new_capa((long)arr->len);
    for (size_t i = 0; i < arr->len; i++) {
        CommentEntry entry = arr->items[i];

        VALUE string = rb_utf8_str_new_cstr(entry.string);

        Location *loc = entry.location;
        VALUE location = build_location_value(loc);

        VALUE comment_kwargs = rb_hash_new();
        rb_hash_aset(comment_kwargs, ID2SYM(rb_intern("string")), string);
        rb_hash_aset(comment_kwargs, ID2SYM(rb_intern("location")), location);
        VALUE comment = rb_class_new_instance_kw(1, &comment_kwargs, cComment, RB_PASS_KEYWORDS);

        rb_ary_push(ary, comment);
    }

    // Free the array and all inner allocations on the Rust side
    sat_definition_comments_free(arr);
    return ary;
}

// Definition#name -> String
static VALUE sr_definition_name(VALUE self) {
    HandleData *data;
    TypedData_Get_Struct(self, HandleData, &handle_type, data);

    void *graph;
    TypedData_Get_Struct(data->graph_obj, void *, &graph_type, graph);

    const char *name = sat_definition_name(graph, data->id);
    if (name == NULL) {
        return Qnil;
    }
    VALUE str = rb_utf8_str_new_cstr(name);
    free_c_string(name);
    return str;
}

void initialize_definition(VALUE mod) {
    mSaturn = mod;

    cComment = rb_define_class_under(mSaturn, "Comment", rb_cObject);

    cDefinition = rb_define_class_under(mSaturn, "Definition", rb_cObject);
    rb_define_alloc_func(cDefinition, sr_handle_alloc);
    rb_define_method(cDefinition, "initialize", sr_handle_initialize, 2);
    rb_funcall(rb_singleton_class(cDefinition), rb_intern("private"), 1, ID2SYM(rb_intern("new")));
    rb_define_method(cDefinition, "location", sr_definition_location, 0);
    rb_define_method(cDefinition, "comments", sr_definition_comments, 0);
    rb_define_method(cDefinition, "name", sr_definition_name, 0);

    cClassDefinition = rb_define_class_under(mSaturn, "ClassDefinition", cDefinition);
    rb_define_alloc_func(cClassDefinition, sr_handle_alloc);
    rb_define_method(cClassDefinition, "initialize", sr_handle_initialize, 2);

    cSingletonClassDefinition = rb_define_class_under(mSaturn, "SingletonClassDefinition", cDefinition);
    rb_define_alloc_func(cSingletonClassDefinition, sr_handle_alloc);
    rb_define_method(cSingletonClassDefinition, "initialize", sr_handle_initialize, 2);

    cModuleDefinition = rb_define_class_under(mSaturn, "ModuleDefinition", cDefinition);
    rb_define_alloc_func(cModuleDefinition, sr_handle_alloc);
    rb_define_method(cModuleDefinition, "initialize", sr_handle_initialize, 2);

    cConstantDefinition = rb_define_class_under(mSaturn, "ConstantDefinition", cDefinition);
    rb_define_alloc_func(cConstantDefinition, sr_handle_alloc);
    rb_define_method(cConstantDefinition, "initialize", sr_handle_initialize, 2);

    cMethodDefinition = rb_define_class_under(mSaturn, "MethodDefinition", cDefinition);
    rb_define_alloc_func(cMethodDefinition, sr_handle_alloc);
    rb_define_method(cMethodDefinition, "initialize", sr_handle_initialize, 2);

    cAttrAccessorDefinition = rb_define_class_under(mSaturn, "AttrAccessorDefinition", cDefinition);
    rb_define_alloc_func(cAttrAccessorDefinition, sr_handle_alloc);
    rb_define_method(cAttrAccessorDefinition, "initialize", sr_handle_initialize, 2);

    cAttrReaderDefinition = rb_define_class_under(mSaturn, "AttrReaderDefinition", cDefinition);
    rb_define_alloc_func(cAttrReaderDefinition, sr_handle_alloc);
    rb_define_method(cAttrReaderDefinition, "initialize", sr_handle_initialize, 2);

    cAttrWriterDefinition = rb_define_class_under(mSaturn, "AttrWriterDefinition", cDefinition);
    rb_define_alloc_func(cAttrWriterDefinition, sr_handle_alloc);
    rb_define_method(cAttrWriterDefinition, "initialize", sr_handle_initialize, 2);

    cGlobalVariableDefinition = rb_define_class_under(mSaturn, "GlobalVariableDefinition", cDefinition);
    rb_define_alloc_func(cGlobalVariableDefinition, sr_handle_alloc);
    rb_define_method(cGlobalVariableDefinition, "initialize", sr_handle_initialize, 2);

    cInstanceVariableDefinition = rb_define_class_under(mSaturn, "InstanceVariableDefinition", cDefinition);
    rb_define_alloc_func(cInstanceVariableDefinition, sr_handle_alloc);
    rb_define_method(cInstanceVariableDefinition, "initialize", sr_handle_initialize, 2);

    cClassVariableDefinition = rb_define_class_under(mSaturn, "ClassVariableDefinition", cDefinition);
    rb_define_alloc_func(cClassVariableDefinition, sr_handle_alloc);
    rb_define_method(cClassVariableDefinition, "initialize", sr_handle_initialize, 2);

    cMethodAliasDefinition = rb_define_class_under(mSaturn, "MethodAliasDefinition", cDefinition);
    rb_define_alloc_func(cMethodAliasDefinition, sr_handle_alloc);
    rb_define_method(cMethodAliasDefinition, "initialize", sr_handle_initialize, 2);
}
