#include "document.h"
#include "definition.h"
#include "graph.h"
#include "handle.h"
#include "rustbindings.h"

VALUE cDocument;

// Document#uri -> String
static VALUE rdxr_document_uri(VALUE self) {
    HandleData *data;
    TypedData_Get_Struct(self, HandleData, &handle_type, data);

    void *graph;
    TypedData_Get_Struct(data->graph_obj, void *, &graph_type, graph);
    const char *uri = rdx_document_uri(graph, data->id);

    if (uri == NULL) {
        return Qnil;
    }

    VALUE str = rb_utf8_str_new_cstr(uri);
    free_c_string(uri);

    return str;
}

// Body function for rb_ensure in Document#definitions
static VALUE document_definitions_yield(VALUE args) {
    VALUE self = rb_ary_entry(args, 0);
    void *iter = (void *)(uintptr_t)NUM2ULL(rb_ary_entry(args, 1));

    HandleData *data;
    TypedData_Get_Struct(self, HandleData, &handle_type, data);

    uint64_t id = 0;
    DefinitionKind kind;
    while (rdx_definitions_iter_next(iter, &id, &kind)) {
        VALUE argv[] = {data->graph_obj, ULL2NUM(id)};
        VALUE defn_class = rdxi_definition_class_for_kind(kind);
        VALUE handle = rb_class_new_instance(2, argv, defn_class);
        rb_yield(handle);
    }

    return Qnil;
}

// Ensure function for rb_ensure in Document#definitions to always free the iterator
static VALUE document_definitions_ensure(VALUE args) {
    void *iter = (void *)(uintptr_t)NUM2ULL(rb_ary_entry(args, 1));
    rdx_definitions_iter_free(iter);

    return Qnil;
}

// Size function for the Document#definitions enumerator
static VALUE document_definitions_size(VALUE self, VALUE _args, VALUE _eobj) {
    HandleData *data;
    TypedData_Get_Struct(self, HandleData, &handle_type, data);

    void *graph;
    TypedData_Get_Struct(data->graph_obj, void *, &graph_type, graph);
    struct DefinitionsIter *iter = rdx_document_definitions_iter_new(graph, data->id);
    size_t len = rdx_definitions_iter_len(iter);
    rdx_definitions_iter_free(iter);

    return SIZET2NUM(len);
}

// Document#definitions: () -> Enumerator[Definition]
// Returns an enumerator that yields all definitions for this document lazily
static VALUE rdxr_document_definitions(VALUE self) {
    if (!rb_block_given_p()) {
        return rb_enumeratorize_with_size(self, rb_str_new2("definitions"), 0, NULL, document_definitions_size);
    }

    HandleData *data;
    TypedData_Get_Struct(self, HandleData, &handle_type, data);

    void *graph;
    TypedData_Get_Struct(data->graph_obj, void *, &graph_type, graph);
    void *iter = rdx_document_definitions_iter_new(graph, data->id);
    VALUE args = rb_ary_new_from_args(2, self, ULL2NUM((uintptr_t)iter));
    rb_ensure(document_definitions_yield, args, document_definitions_ensure, args);

    return self;
}

void rdxi_initialize_document(VALUE mRubydex) {
    cDocument = rb_define_class_under(mRubydex, "Document", rb_cObject);

    rb_define_alloc_func(cDocument, rdxr_handle_alloc);
    rb_define_method(cDocument, "initialize", rdxr_handle_initialize, 2);
    rb_define_method(cDocument, "uri", rdxr_document_uri, 0);
    rb_define_method(cDocument, "definitions", rdxr_document_definitions, 0);

    rb_funcall(rb_singleton_class(cDocument), rb_intern("private"), 1, ID2SYM(rb_intern("new")));
}
