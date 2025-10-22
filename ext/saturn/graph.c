#include "graph.h"
#include "declaration.h"
#include "document.h"
#include "rustbindings.h"
#include "utils.h"

static VALUE cGraph;

// Free function for the custom Graph allocator. We always have to call into Rust to free data allocated by it
static void graph_free(void *ptr) {
    if (ptr) {
        sat_graph_free(ptr);
    }
}

const rb_data_type_t graph_type = {"Graph", {0, graph_free, 0}, 0, 0, RUBY_TYPED_FREE_IMMEDIATELY};

// Custom allocator for the Graph class. Calls into Rust to create a new `Arc<Mutex<Graph>>` that gets stored internally
// as a void pointer
static VALUE sr_graph_alloc(VALUE klass) {
    void *graph = sat_graph_new();
    return TypedData_Wrap_Struct(klass, &graph_type, graph);
}

// Graph#index_all: (Array[String] file_paths) -> String?
// Returns the error messages concatenated as a single string if anything failed during indexing or `nil`
static VALUE sr_graph_index_all(VALUE self, VALUE file_paths) {
    check_array_of_strings(file_paths);

    // Convert the given file paths into a char** array, so that we can pass to Rust
    size_t length = RARRAY_LEN(file_paths);
    char **converted_file_paths = str_array_to_char(file_paths, length);

    // Get the underying graph pointer and then invoke the Rust index all implementation
    void *graph;
    TypedData_Get_Struct(self, void *, &graph_type, graph);
    const char *error_messages = sat_index_all(graph, (const char **)converted_file_paths, length);

    // Free the converted file paths and allow the GC to collect them
    for (size_t i = 0; i < length; i++) {
        free(converted_file_paths[i]);
    }
    free(converted_file_paths);

    // If indexing errors were returned, turn them into a Ruby string, call Rust to free the CString it allocated and
    // return the Ruby string
    if (error_messages != NULL) {
        VALUE error_string = rb_utf8_str_new_cstr(error_messages);
        free_c_string(error_messages);
        return error_string;
    }

    return Qnil;
}

static VALUE sr_graph_set_configuration(VALUE self, VALUE db_path) {
    Check_Type(db_path, T_STRING);

    void *graph;
    TypedData_Get_Struct(self, void *, &graph_type, graph);

    if (!sat_graph_set_configuration(graph, StringValueCStr(db_path))) {
        rb_raise(rb_eRuntimeError, "Failed to set the database configuration");
    }

    return Qnil;
}

// Body function for rb_ensure in Graph#declarations
static VALUE graph_declarations_yield(VALUE args) {
    VALUE self = rb_ary_entry(args, 0);
    void *iter = (void *)(uintptr_t)NUM2ULL(rb_ary_entry(args, 1));

    int64_t id = 0;
    while (sat_graph_declarations_iter_next(iter, &id)) {
        VALUE argv[] = {self, LL2NUM(id)};
        VALUE handle = rb_class_new_instance(2, argv, cDeclaration);
        rb_yield(handle);
    }

    return Qnil;
}

// Ensure function for rb_ensure in Graph#declarations to always free the iterator
static VALUE graph_declarations_ensure(VALUE args) {
    void *iter = (void *)(uintptr_t)NUM2ULL(rb_ary_entry(args, 1));
    sat_graph_declarations_iter_free(iter);

    return Qnil;
}

// Size function for the declarations enumerator
static VALUE graph_declarations_size(VALUE self, VALUE _args, VALUE _eobj) {
    void *graph;
    TypedData_Get_Struct(self, void *, &graph_type, graph);

    DeclarationsIter *iter = sat_graph_declarations_iter_new(graph);
    size_t len = sat_graph_declarations_iter_len(iter);
    sat_graph_declarations_iter_free(iter);

    return SIZET2NUM(len);
}

// Graph#declarations: () -> Enumerator[Declaration]
// Returns an enumerator that yields all declarations lazily
static VALUE sr_graph_declarations(VALUE self) {
    if (!rb_block_given_p()) {
        return rb_enumeratorize_with_size(self, rb_str_new2("declarations"), 0, NULL, graph_declarations_size);
    }

    void *graph;
    TypedData_Get_Struct(self, void *, &graph_type, graph);

    void *iter = sat_graph_declarations_iter_new(graph);
    VALUE args = rb_ary_new_from_args(2, self, ULL2NUM((uintptr_t)iter));
    rb_ensure(graph_declarations_yield, args, graph_declarations_ensure, args);

    return self;
}

// Body function for rb_ensure in Graph#documents
static VALUE graph_documents_yield(VALUE args) {
    VALUE self = rb_ary_entry(args, 0);
    void *iter = (void *)(uintptr_t)NUM2ULL(rb_ary_entry(args, 1));

    int64_t id = 0;
    while (sat_graph_documents_iter_next(iter, &id)) {
        VALUE argv[] = {self, LL2NUM(id)};
        VALUE handle = rb_class_new_instance(2, argv, cDocument);
        rb_yield(handle);
    }

    return Qnil;
}

// Ensure function for rb_ensure in Graph#documents to always free the iterator
static VALUE graph_documents_ensure(VALUE args) {
    void *iter = (void *)(uintptr_t)NUM2ULL(rb_ary_entry(args, 1));
    sat_graph_documents_iter_free(iter);

    return Qnil;
}

// Size function for the documents enumerator
static VALUE graph_documents_size(VALUE self, VALUE _args, VALUE _eobj) {
    void *graph;
    TypedData_Get_Struct(self, void *, &graph_type, graph);

    DocumentsIter *iter = sat_graph_documents_iter_new(graph);
    size_t len = sat_graph_documents_iter_len(iter);
    sat_graph_documents_iter_free(iter);

    return SIZET2NUM(len);
}

// Graph#documents: () -> Enumerator[Document]
// Returns an enumerator that yields all documents lazily
static VALUE sr_graph_documents(VALUE self) {
    if (!rb_block_given_p()) {
        return rb_enumeratorize_with_size(self, rb_str_new2("documents"), 0, NULL, graph_documents_size);
    }

    void *graph;
    TypedData_Get_Struct(self, void *, &graph_type, graph);

    void *iter = sat_graph_documents_iter_new(graph);
    VALUE args = rb_ary_new_from_args(2, self, ULL2NUM((uintptr_t)iter));
    rb_ensure(graph_documents_yield, args, graph_documents_ensure, args);

    return self;
}

// Graph#[]: (String fully_qualified_name) -> Declaration
// Returns a declaration handle for the given ID
static VALUE sr_graph_aref(VALUE self, VALUE key) {
    void *graph;
    TypedData_Get_Struct(self, void *, &graph_type, graph);

    if (TYPE(key) != T_STRING) {
        rb_raise(rb_eTypeError, "expected String");
    }

    const int64_t *id_ptr = sat_graph_get_declaration(graph, StringValueCStr(key));
    if (id_ptr == NULL) {
        return Qnil;
    }

    int64_t id = *id_ptr;
    free_i64(id_ptr);
    VALUE argv[] = {self, LL2NUM(id)};

    return rb_class_new_instance(2, argv, cDeclaration);
}

void initialize_graph(VALUE mSaturn) {
    cGraph = rb_define_class_under(mSaturn, "Graph", rb_cObject);

    rb_define_alloc_func(cGraph, sr_graph_alloc);
    rb_define_method(cGraph, "index_all", sr_graph_index_all, 1);
    rb_define_method(cGraph, "set_configuration", sr_graph_set_configuration, 1);
    rb_define_method(cGraph, "declarations", sr_graph_declarations, 0);
    rb_define_method(cGraph, "documents", sr_graph_documents, 0);
    rb_define_method(cGraph, "[]", sr_graph_aref, 1);
}
