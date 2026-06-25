#include "document.h"
#include "definition.h"
#include "graph.h"
#include "handle.h"
#include "rustbindings.h"
#include "utils.h"

/*
 * RDoc parser workaround for https://github.com/ruby/rdoc/issues/1744:
 * mRubydex = rb_define_module("Rubydex")
 */

VALUE cDocument;

/*
 * call-seq:
 *   uri -> String?
 *
 * Returns the document URI.
 */
static VALUE rdxr_document_uri(VALUE self) {
    HandleData *data;
    void *graph = rdxi_graph_from_handle(self, &data);
    const char *uri = rdx_document_uri(graph, data->id);

    return rdxi_owned_c_string_to_ruby(uri);
}

// Body function for rb_ensure in Document#definitions
static VALUE document_definitions_yield(VALUE args) {
    VALUE self = rb_ary_entry(args, 0);
    void *iter = (void *)(uintptr_t)NUM2ULL(rb_ary_entry(args, 1));

    HandleData *data;
    TypedData_Get_Struct(self, HandleData, &handle_type, data);

    CDefinition defn;
    while (rdx_definitions_iter_next(iter, &defn)) {
        VALUE argv[] = {data->graph_obj, ULL2NUM(defn.id)};
        VALUE defn_class = rdxi_definition_class_for_kind(defn.kind);
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
    void *graph = rdxi_graph_from_handle(self, &data);
    struct DefinitionsIter *iter = rdx_document_definitions_iter_new(graph, data->id);
    size_t len = rdx_definitions_iter_len(iter);
    rdx_definitions_iter_free(iter);

    return SIZET2NUM(len);
}

/*
 * call-seq:
 *   definitions -> Enumerator[Rubydex::Definition]
 *
 * Returns an enumerator that yields all definitions for this document lazily.
 */
static VALUE rdxr_document_definitions(VALUE self) {
    if (!rb_block_given_p()) {
        return rb_enumeratorize_with_size(self, rb_str_new2("definitions"), 0, NULL, document_definitions_size);
    }

    HandleData *data;
    void *graph = rdxi_graph_from_handle(self, &data);
    void *iter = rdx_document_definitions_iter_new(graph, data->id);
    VALUE args = rb_ary_new_from_args(2, self, ULL2NUM((uintptr_t)iter));
    rb_ensure(document_definitions_yield, args, document_definitions_ensure, args);

    return self;
}

// Size function for the Document#method_references enumerator
static VALUE document_method_references_size(VALUE self, VALUE _args, VALUE _eobj) {
    HandleData *data;
    void *graph = rdxi_graph_from_handle(self, &data);
    struct MethodReferencesIter *iter = rdx_document_method_references_iter_new(graph, data->id);
    size_t len = rdx_method_references_iter_len(iter);
    rdx_method_references_iter_free(iter);

    return SIZET2NUM(len);
}

// Document#method_references: () -> Enumerator[MethodReference]
// Returns an enumerator that yields all method references for this document lazily
static VALUE rdxr_document_method_references(VALUE self) {
    if (!rb_block_given_p()) {
        return rb_enumeratorize_with_size(self, rb_str_new2("method_references"), 0, NULL,
                                          document_method_references_size);
    }

    HandleData *data;
    void *graph = rdxi_graph_from_handle(self, &data);
    void *iter = rdx_document_method_references_iter_new(graph, data->id);
    VALUE args = rb_ary_new_from_args(2, data->graph_obj, ULL2NUM((uintptr_t)iter));
    rb_ensure(rdxi_method_references_yield, args, rdxi_method_references_ensure, args);

    return self;
}

void rdxi_initialize_document(VALUE mRubydex) {
    cDocument = rb_define_class_under(mRubydex, "Document", rb_cObject);

    rb_define_alloc_func(cDocument, rdxr_handle_alloc);
    rb_define_method(cDocument, "initialize", rdxr_handle_initialize, 2);
    rb_define_method(cDocument, "uri", rdxr_document_uri, 0);
    rb_define_method(cDocument, "definitions", rdxr_document_definitions, 0);
    rb_define_method(cDocument, "method_references", rdxr_document_method_references, 0);

    rb_funcall(rb_singleton_class(cDocument), rb_intern("private"), 1, ID2SYM(rb_intern("new")));
}
