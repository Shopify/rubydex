#include "definition.h"
#include "declaration.h"
#include "graph.h"
#include "handle.h"
#include "location.h"
#include "reference.h"
#include "signature.h"
#include "ruby/internal/scan_args.h"
#include "rustbindings.h"
#include "utils.h"

/*
 * RDoc parser workaround for https://github.com/ruby/rdoc/issues/1744:
 * mRubydex = rb_define_module("Rubydex")
 */

static VALUE mRubydex;
static VALUE cInclude;
static VALUE cPrepend;
static VALUE cExtend;
VALUE cComment;
VALUE cDefinition;
VALUE cClassDefinition;
VALUE cSingletonClassDefinition;
VALUE cModuleDefinition;
VALUE cConstantDefinition;
VALUE cConstantAliasDefinition;
VALUE cConstantVisibilityDefinition;
VALUE cMethodVisibilityDefinition;
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
    case DefinitionKind_ConstantVisibility:
        return cConstantVisibilityDefinition;
    case DefinitionKind_MethodVisibility:
        return cMethodVisibilityDefinition;
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

/*
 * call-seq:
 *   location -> Rubydex::Location
 *
 * Returns the source location for this definition.
 */
static VALUE rdxr_definition_location(VALUE self) {
    HandleData *data;
    void *graph = rdxi_graph_from_handle(self, &data);

    Location *loc = rdx_definition_location(graph, data->id);
    VALUE location = rdxi_build_location_value(loc);
    rdx_location_free(loc);

    return location;
}

/*
 * call-seq:
 *   comments -> Array[Rubydex::Comment]
 *
 * Returns the source comments associated with this definition.
 */
static VALUE rdxr_definition_comments(VALUE self) {
    HandleData *data;
    void *graph = rdxi_graph_from_handle(self, &data);

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

/*
 * call-seq:
 *   name -> String?
 *
 * Returns the definition name.
 */
static VALUE rdxr_definition_name(VALUE self) {
    HandleData *data;
    void *graph = rdxi_graph_from_handle(self, &data);

    const char *name = rdx_definition_name(graph, data->id);
    return rdxi_owned_c_string_to_ruby(name);
}

/*
 * call-seq:
 *   deprecated? -> bool
 *
 * Returns whether this definition is marked as deprecated.
 */
static VALUE rdxr_definition_deprecated(VALUE self) {
    HandleData *data;
    void *graph = rdxi_graph_from_handle(self, &data);

    bool deprecated = rdx_definition_is_deprecated(graph, data->id);
    return deprecated ? Qtrue : Qfalse;
}

/*
 * call-seq:
 *   name_location -> Rubydex::Location?
 *
 * For class, module, singleton class, and method definitions, returns the location of just the name, such as "Bar" in
 * "class Foo::Bar" or "foo" in "def foo". For other definition types, returns nil.
 */
static VALUE rdxr_definition_name_location(VALUE self) {
    HandleData *data;
    void *graph = rdxi_graph_from_handle(self, &data);

    Location *loc = rdx_definition_name_location(graph, data->id);
    if (loc == NULL) {
        return Qnil;
    }
    VALUE location = rdxi_build_location_value(loc);
    rdx_location_free(loc);

    return location;
}

/*
 * call-seq:
 *   declaration -> Rubydex::Declaration?
 *
 * Returns the declaration this definition belongs to or nil when it cannot be located.
 */
static VALUE rdxr_definition_declaration(VALUE self) {
    HandleData *data;
    void *graph = rdxi_graph_from_handle(self, &data);

    const struct CDeclaration *decl = rdx_definition_declaration(graph, data->id);
    if (decl == NULL) {
        return Qnil;
    }

    VALUE decl_class = rdxi_declaration_class_for_kind(decl->kind);
    VALUE argv[] = {data->graph_obj, ULL2NUM(decl->id)};
    free_c_declaration(decl);

    return rb_class_new_instance(2, argv, decl_class);
}

static VALUE rdxi_build_definition(VALUE graph_obj, void *graph, uint64_t definition_id) {
    DefinitionKind kind = rdx_definition_kind(graph, definition_id);
    VALUE defn_class = rdxi_definition_class_for_kind(kind);
    VALUE argv[] = {graph_obj, ULL2NUM(definition_id)};

    return rb_class_new_instance(2, argv, defn_class);
}

/*
 * call-seq:
 *   lexical_owner -> Rubydex::Definition?
 *
 * Returns the lexically enclosing definition, if any.
 */
static VALUE rdxr_definition_lexical_owner(VALUE self) {
    HandleData *data;
    void *graph = rdxi_graph_from_handle(self, &data);

    const uint64_t *owner_id = rdx_definition_lexical_nesting_id(graph, data->id);
    if (owner_id == NULL) {
        return Qnil;
    }

    VALUE owner = rdxi_build_definition(data->graph_obj, graph, *owner_id);
    free_u64(owner_id);

    return owner;
}

/*
 * call-seq:
 *   lexical_nesting -> Array[Rubydex::Definition]
 *
 * Returns the lexical nesting from the direct owner up to the root.
 */
static VALUE rdxr_definition_lexical_nesting(VALUE self) {
    HandleData *data;
    void *graph = rdxi_graph_from_handle(self, &data);

    VALUE nesting = rb_ary_new();
    uint64_t definition_id = data->id;

    while (true) {
        const uint64_t *owner_id = rdx_definition_lexical_nesting_id(graph, definition_id);
        if (owner_id == NULL) {
            break;
        }

        rb_ary_push(nesting, rdxi_build_definition(data->graph_obj, graph, *owner_id));
        definition_id = *owner_id;
        free_u64(owner_id);
    }

    return nesting;
}

static VALUE rdxi_build_constant_reference(VALUE graph_obj, const CConstantReference *cref) {
    VALUE ref_class = (cref->declaration_id == 0)
        ? cUnresolvedConstantReference
        : cResolvedConstantReference;

    VALUE argv[] = {graph_obj, ULL2NUM(cref->id)};
    return rb_class_new_instance(2, argv, ref_class);
}

/*
 * call-seq:
 *   superclass -> Rubydex::ConstantReference?
 *
 * Returns the superclass constant reference, or nil if this class definition has no explicit superclass.
 */
static VALUE rdxr_class_definition_superclass(VALUE self) {
    HandleData *data;
    void *graph = rdxi_graph_from_handle(self, &data);

    const CConstantReference *ref = rdx_class_definition_superclass(graph, data->id);
    if (ref == NULL) {
        return Qnil;
    }

    VALUE result = rdxi_build_constant_reference(data->graph_obj, ref);
    free_c_constant_reference(ref);
    return result;
}

static VALUE rdxi_mixin_class_for_kind(MixinKind kind) {
    switch (kind) {
    case MixinKind_Include:
        return cInclude;
    case MixinKind_Prepend:
        return cPrepend;
    case MixinKind_Extend:
        return cExtend;
    default:
        rb_raise(rb_eRuntimeError, "Unknown MixinKind: %d", kind);
    }
}

/*
 * call-seq:
 *   mixins -> Array[Rubydex::Mixin]
 *
 * Returns mixins attached to this definition.
 */
static VALUE rdxr_definition_mixins(VALUE self) {
    HandleData *data;
    void *graph = rdxi_graph_from_handle(self, &data);

    MixinsIter *iter = rdx_definition_mixins(graph, data->id);
    if (iter == NULL) {
        rb_raise(rb_eRuntimeError, "Tried to get mixins for a definition that isn't a namespace");
    }

    size_t len = rdx_mixins_iter_len(iter);
    VALUE ary = rb_ary_new_capa((long)len);

    CMixin entry;
    while (rdx_mixins_iter_next(iter, &entry)) {
        VALUE constant_ref = rdxi_build_constant_reference(data->graph_obj, &entry.constant_reference);
        VALUE mixin_class = rdxi_mixin_class_for_kind(entry.kind);
        VALUE mixin = rb_class_new_instance(1, &constant_ref, mixin_class);
        rb_ary_push(ary, mixin);
    }

    rdx_mixins_iter_free(iter);
    return ary;
}

/*
 * call-seq:
 *   signatures -> Array[Rubydex::Signature]
 *
 * Returns signatures for this method definition.
 */
static VALUE rdxr_method_definition_signatures(VALUE self) {
    HandleData *data;
    void *graph = rdxi_graph_from_handle(self, &data);

    SignatureArray *arr = rdx_definition_signatures(graph, data->id);
    return rdxi_signatures_to_ruby(arr);
}

/*
 * call-seq:
 *   signatures -> Array[Rubydex::Signature]
 *
 * Returns signatures for this method alias definition.
 */
static VALUE rdxr_method_alias_definition_signatures(VALUE self) {
    HandleData *data;
    void *graph = rdxi_graph_from_handle(self, &data);

    SignatureArray *arr = rdx_method_alias_definition_signatures(graph, data->id);
    return rdxi_signatures_to_ruby(arr);
}

/*
 * call-seq:
 *   target -> Rubydex::Method?
 *
 * Returns the resolved target method declaration by following the alias chain, or nil if the chain could not be
 * resolved. Raises Rubydex::AliasCycleError when the alias chain forms a cycle.
 */
static VALUE rdxr_method_alias_definition_target(VALUE self) {
    HandleData *data;
    void *graph = rdxi_graph_from_handle(self, &data);

    CMethodAliasTargetResult result = rdx_method_alias_definition_target(graph, data->id);

    switch (result.status) {
    case CMethodAliasResolution_Resolved: {
        VALUE decl_class = rdxi_declaration_class_for_kind(result.declaration->kind);
        VALUE argv[] = {data->graph_obj, ULL2NUM(result.declaration->id)};

        free_c_declaration(result.declaration);
        return rb_class_new_instance(2, argv, decl_class);
    }
    case CMethodAliasResolution_NotFound:
        return Qnil;
    case CMethodAliasResolution_Cycle:
        rb_raise(rb_const_get(mRubydex, rb_intern("AliasCycleError")), "method alias chain forms a cycle");
    default:
        rb_raise(rb_eRuntimeError, "Unknown CMethodAliasResolution: %d", result.status);
    }
}

void rdxi_initialize_definition(VALUE mod) {
    mRubydex = mod;

    cInclude = rb_const_get(mRubydex, rb_intern("Include"));
    cPrepend = rb_const_get(mRubydex, rb_intern("Prepend"));
    cExtend = rb_const_get(mRubydex, rb_intern("Extend"));

    cComment = rb_define_class_under(mRubydex, "Comment", rb_cObject);

    cDefinition = rb_define_class_under(mRubydex, "Definition", rb_cObject);
    rb_define_alloc_func(cDefinition, rdxr_handle_alloc);
    rb_define_method(cDefinition, "initialize", rdxr_handle_initialize, 2);
    rb_funcall(rb_singleton_class(cDefinition), rb_intern("private"), 1, ID2SYM(rb_intern("new")));
    rb_define_method(cDefinition, "location", rdxr_definition_location, 0);
    rb_define_method(cDefinition, "comments", rdxr_definition_comments, 0);
    rb_define_method(cDefinition, "name", rdxr_definition_name, 0);
    rb_define_method(cDefinition, "deprecated?", rdxr_definition_deprecated, 0);
    rb_define_method(cDefinition, "name_location", rdxr_definition_name_location, 0);
    rb_define_method(cDefinition, "declaration", rdxr_definition_declaration, 0);
    rb_define_method(cDefinition, "lexical_owner", rdxr_definition_lexical_owner, 0);
    rb_define_method(cDefinition, "lexical_nesting", rdxr_definition_lexical_nesting, 0);

    cClassDefinition = rb_define_class_under(mRubydex, "ClassDefinition", cDefinition);
    rb_define_method(cClassDefinition, "superclass", rdxr_class_definition_superclass, 0);
    rb_define_method(cClassDefinition, "mixins", rdxr_definition_mixins, 0);

    cSingletonClassDefinition = rb_define_class_under(mRubydex, "SingletonClassDefinition", cDefinition);
    rb_define_method(cSingletonClassDefinition, "mixins", rdxr_definition_mixins, 0);

    cModuleDefinition = rb_define_class_under(mRubydex, "ModuleDefinition", cDefinition);
    rb_define_method(cModuleDefinition, "mixins", rdxr_definition_mixins, 0);

    cConstantDefinition = rb_define_class_under(mRubydex, "ConstantDefinition", cDefinition);
    cConstantAliasDefinition = rb_define_class_under(mRubydex, "ConstantAliasDefinition", cDefinition);
    cConstantVisibilityDefinition = rb_define_class_under(mRubydex, "ConstantVisibilityDefinition", cDefinition);
    cMethodVisibilityDefinition = rb_define_class_under(mRubydex, "MethodVisibilityDefinition", cDefinition);
    cMethodDefinition = rb_define_class_under(mRubydex, "MethodDefinition", cDefinition);
    rb_define_method(cMethodDefinition, "signatures", rdxr_method_definition_signatures, 0);
    cAttrAccessorDefinition = rb_define_class_under(mRubydex, "AttrAccessorDefinition", cDefinition);
    cAttrReaderDefinition = rb_define_class_under(mRubydex, "AttrReaderDefinition", cDefinition);
    cAttrWriterDefinition = rb_define_class_under(mRubydex, "AttrWriterDefinition", cDefinition);
    cGlobalVariableDefinition = rb_define_class_under(mRubydex, "GlobalVariableDefinition", cDefinition);
    cInstanceVariableDefinition = rb_define_class_under(mRubydex, "InstanceVariableDefinition", cDefinition);
    cClassVariableDefinition = rb_define_class_under(mRubydex, "ClassVariableDefinition", cDefinition);
    cMethodAliasDefinition = rb_define_class_under(mRubydex, "MethodAliasDefinition", cDefinition);
    rb_define_method(cMethodAliasDefinition, "signatures", rdxr_method_alias_definition_signatures, 0);
    rb_define_method(cMethodAliasDefinition, "target", rdxr_method_alias_definition_target, 0);
    cGlobalVariableAliasDefinition = rb_define_class_under(mRubydex, "GlobalVariableAliasDefinition", cDefinition);
}
