#include "graph.h"
#include "declaration.h"
#include "diagnostic.h"
#include "document.h"
#include "location.h"
#include "reference.h"
#include "ruby/internal/globals.h"
#include "rustbindings.h"
#include "utils.h"

static VALUE cGraph;

// Free function for the custom Graph allocator. We always have to call into Rust to free data allocated by it
static void graph_free(void *ptr) {
    if (ptr) {
        rdx_graph_free(ptr);
    }
}

const rb_data_type_t graph_type = {"Graph", {0, graph_free, 0}, 0, 0, RUBY_TYPED_FREE_IMMEDIATELY};

// Custom allocator for the Graph class. Calls into Rust to create a new `Arc<Mutex<Graph>>` that gets stored internally
// as a void pointer
static VALUE rdxr_graph_alloc(VALUE klass) {
    void *graph = rdx_graph_new();
    return TypedData_Wrap_Struct(klass, &graph_type, graph);
}

// Graph#index_all: (Array[String] file_paths) -> Array[String]
// Returns an array of IO error messages encountered during indexing
static VALUE rdxr_graph_index_all(VALUE self, VALUE file_paths) {
    rdxi_check_array_of_strings(file_paths);

    // Convert the given file paths into a char** array, so that we can pass to Rust
    size_t length = RARRAY_LEN(file_paths);
    char **converted_file_paths = rdxi_str_array_to_char(file_paths, length);

    // Get the underlying graph pointer and then invoke the Rust index all implementation
    void *graph;
    TypedData_Get_Struct(self, void *, &graph_type, graph);

    size_t error_count = 0;
    const char *const *errors = rdx_index_all(graph, (const char **)converted_file_paths, length, &error_count);

    // Free the converted file paths and allow the GC to collect them
    for (size_t i = 0; i < length; i++) {
        free(converted_file_paths[i]);
    }
    free(converted_file_paths);

    if (errors == NULL) {
        return rb_ary_new();
    }

    VALUE array = rb_ary_new_capa((long)error_count);
    for (size_t i = 0; i < error_count; i++) {
        rb_ary_push(array, rb_utf8_str_new_cstr(errors[i]));
    }

    free_c_string_array(errors, error_count);
    return array;
}

// Indexes a single source string in memory, dispatching to the appropriate indexer based on language_id
//
// Graph#index_source: (String uri, String source, String language_id) -> void
static VALUE rdxr_graph_index_source(VALUE self, VALUE uri, VALUE source, VALUE language_id) {
    Check_Type(uri, T_STRING);
    Check_Type(source, T_STRING);
    Check_Type(language_id, T_STRING);

    void *graph;
    TypedData_Get_Struct(self, void *, &graph_type, graph);

    const char *uri_str = StringValueCStr(uri);
    const char *source_str = StringValueCStr(source);
    const char *language_id_str = StringValueCStr(language_id);

    if (!rdx_index_source(graph, uri_str, source_str, language_id_str)) {
        rb_raise(rb_eArgError, "unsupported language_id `%s`", language_id_str);
    }

    return Qnil;
}

// Size function for the declarations enumerator
static VALUE graph_declarations_size(VALUE self, VALUE _args, VALUE _eobj) {
    void *graph;
    TypedData_Get_Struct(self, void *, &graph_type, graph);

    DeclarationsIter *iter = rdx_graph_declarations_iter_new(graph);
    size_t len = rdx_graph_declarations_iter_len(iter);
    rdx_graph_declarations_iter_free(iter);

    return SIZET2NUM(len);
}

// Graph#declarations: () -> Enumerator[Declaration]
// Returns an enumerator that yields all declarations lazily
static VALUE rdxr_graph_declarations(VALUE self) {
    if (!rb_block_given_p()) {
        return rb_enumeratorize_with_size(self, rb_str_new2("declarations"), 0, NULL, graph_declarations_size);
    }

    void *graph;
    TypedData_Get_Struct(self, void *, &graph_type, graph);

    void *iter = rdx_graph_declarations_iter_new(graph);
    VALUE args = rb_ary_new_from_args(2, self, ULL2NUM((uintptr_t)iter));
    rb_ensure(rdxi_declarations_yield, args, rdxi_declarations_ensure, args);

    return self;
}

// Graph#search: () -> Enumerator[Declaration]
// Returns an enumerator that yields all declarations lazily
static VALUE rdxr_graph_search(VALUE self, VALUE query) {
    if (!rb_block_given_p()) {
        return rb_enumeratorize(self, rb_str_new2("search"), 1, &query);
    }

    void *graph;
    TypedData_Get_Struct(self, void *, &graph_type, graph);

    const char *c_query = StringValueCStr(query);

    void *iter = rdx_graph_declarations_search(graph, c_query);

    if (iter == NULL) {
        // The only case where the iterator will be NULL instead of a list is if the query cannot be converted to a Rust
        // string
        rb_raise(rb_eRuntimeError, "Converting query to Rust string failed");
    }

    VALUE args = rb_ary_new_from_args(2, self, ULL2NUM((uintptr_t)iter));
    rb_ensure(rdxi_declarations_yield, args, rdxi_declarations_ensure, args);

    return self;
}

// Body function for rb_ensure in Graph#documents
static VALUE graph_documents_yield(VALUE args) {
    VALUE self = rb_ary_entry(args, 0);
    void *iter = (void *)(uintptr_t)NUM2ULL(rb_ary_entry(args, 1));

    uint64_t id = 0;
    while (rdx_graph_documents_iter_next(iter, &id)) {
        VALUE argv[] = {self, ULL2NUM(id)};
        VALUE handle = rb_class_new_instance(2, argv, cDocument);
        rb_yield(handle);
    }

    return Qnil;
}

// Ensure function for rb_ensure in Graph#documents to always free the iterator
static VALUE graph_documents_ensure(VALUE args) {
    void *iter = (void *)(uintptr_t)NUM2ULL(rb_ary_entry(args, 1));
    rdx_graph_documents_iter_free(iter);

    return Qnil;
}

// Size function for the documents enumerator
static VALUE graph_documents_size(VALUE self, VALUE _args, VALUE _eobj) {
    void *graph;
    TypedData_Get_Struct(self, void *, &graph_type, graph);

    DocumentsIter *iter = rdx_graph_documents_iter_new(graph);
    size_t len = rdx_graph_documents_iter_len(iter);
    rdx_graph_documents_iter_free(iter);

    return SIZET2NUM(len);
}

// Graph#documents: () -> Enumerator[Document]
// Returns an enumerator that yields all documents lazily
static VALUE rdxr_graph_documents(VALUE self) {
    if (!rb_block_given_p()) {
        return rb_enumeratorize_with_size(self, rb_str_new2("documents"), 0, NULL, graph_documents_size);
    }

    void *graph;
    TypedData_Get_Struct(self, void *, &graph_type, graph);

    void *iter = rdx_graph_documents_iter_new(graph);
    VALUE args = rb_ary_new_from_args(2, self, ULL2NUM((uintptr_t)iter));
    rb_ensure(graph_documents_yield, args, graph_documents_ensure, args);

    return self;
}

// Graph#[]: (String fully_qualified_name) -> Declaration
// Returns a declaration handle for the given ID
static VALUE rdxr_graph_aref(VALUE self, VALUE key) {
    void *graph;
    TypedData_Get_Struct(self, void *, &graph_type, graph);

    if (TYPE(key) != T_STRING) {
        rb_raise(rb_eTypeError, "expected String");
    }

    const CDeclaration *decl = rdx_graph_get_declaration(graph, StringValueCStr(key));
    if (decl == NULL) {
        return Qnil;
    }

    VALUE decl_class = rdxi_declaration_class_for_kind(decl->kind);
    VALUE argv[] = {self, ULL2NUM(decl->id)};
    free_c_declaration(decl);

    return rb_class_new_instance(2, argv, decl_class);
}

// Body function for rb_ensure for the reference enumerators
static VALUE graph_references_yield(VALUE args) {
    VALUE self = rb_ary_entry(args, 0);
    void *iter = (void *)(uintptr_t)NUM2ULL(rb_ary_entry(args, 1));

    uint64_t id = 0;
    ReferenceKind kind;
    while (rdx_references_iter_next(iter, &id, &kind)) {
        VALUE ref_class = rdxi_reference_class_for_kind(kind);
        VALUE argv[] = {self, ULL2NUM(id)};
        VALUE obj = rb_class_new_instance(2, argv, ref_class);
        rb_yield(obj);
    }

    return Qnil;
}

// Ensure function for rb_ensure for the reference enumerators to always free the iterator
static VALUE graph_references_ensure(VALUE args) {
    void *iter = (void *)(uintptr_t)NUM2ULL(rb_ary_entry(args, 1));
    rdx_references_iter_free(iter);

    return Qnil;
}

// Size function for the constant_references enumerator
static VALUE graph_constant_references_size(VALUE self, VALUE _args, VALUE _eobj) {
    void *graph;
    TypedData_Get_Struct(self, void *, &graph_type, graph);

    ReferencesIter *iter = rdx_graph_constant_references_iter_new(graph);
    size_t len = rdx_references_iter_len(iter);
    rdx_references_iter_free(iter);

    return SIZET2NUM(len);
}

// Graph#constant_references: () -> Enumerator[ConstantReference]
// Returns an enumerator that yields constant references lazily
static VALUE rdxr_graph_constant_references(VALUE self) {
    if (!rb_block_given_p()) {
        return rb_enumeratorize_with_size(self, rb_str_new2("constant_references"), 0, NULL,
                                          graph_constant_references_size);
    }

    void *graph;
    TypedData_Get_Struct(self, void *, &graph_type, graph);

    void *iter = rdx_graph_constant_references_iter_new(graph);
    VALUE args = rb_ary_new_from_args(2, self, ULL2NUM((uintptr_t)iter));
    rb_ensure(graph_references_yield, args, graph_references_ensure, args);

    return self;
}

// Size function for the method_references enumerator
static VALUE graph_method_references_size(VALUE self, VALUE _args, VALUE _eobj) {
    void *graph;
    TypedData_Get_Struct(self, void *, &graph_type, graph);

    ReferencesIter *iter = rdx_graph_method_references_iter_new(graph);
    size_t len = rdx_references_iter_len(iter);
    rdx_references_iter_free(iter);

    return SIZET2NUM(len);
}

// Graph#method_references: () -> Enumerator[MethodReference]
// Returns an enumerator that yields method references lazily
static VALUE rdxr_graph_method_references(VALUE self) {
    if (!rb_block_given_p()) {
        return rb_enumeratorize_with_size(self, rb_str_new2("method_references"), 0, NULL,
                                          graph_method_references_size);
    }

    void *graph;
    TypedData_Get_Struct(self, void *, &graph_type, graph);

    void *iter = rdx_graph_method_references_iter_new(graph);
    VALUE args = rb_ary_new_from_args(2, self, ULL2NUM((uintptr_t)iter));
    rb_ensure(graph_references_yield, args, graph_references_ensure, args);

    return self;
}

// Graph#delete_document: (String uri) -> Document?
// Deletes a document and all of its definitions from the graph.
// Returns the removed Document or nil if it doesn't exist.
static VALUE rdxr_graph_delete_document(VALUE self, VALUE uri) {
    Check_Type(uri, T_STRING);

    void *graph;
    TypedData_Get_Struct(self, void *, &graph_type, graph);
    const uint64_t *uri_id = rdx_graph_delete_document(graph, StringValueCStr(uri));

    if (uri_id == NULL) {
        return Qnil;
    }

    VALUE argv[] = {self, ULL2NUM(*uri_id)};
    free_u64(uri_id);
    return rb_class_new_instance(2, argv, cDocument);
}

// Graph#resolve: () -> self
// Runs the resolver to compute declarations and ownership
static VALUE rdxr_graph_resolve(VALUE self) {
    void *graph;
    TypedData_Get_Struct(self, void *, &graph_type, graph);
    rdx_graph_resolve(graph);
    return self;
}

// Graph#set_encoding: (String) -> void
// Sets the encoding used for transforming byte offsets into LSP code unit line/column positions
static VALUE rdxr_graph_set_encoding(VALUE self, VALUE encoding) {
    Check_Type(encoding, T_STRING);

    void *graph;
    TypedData_Get_Struct(self, void *, &graph_type, graph);

    char *encoding_string = StringValueCStr(encoding);
    if (!rdx_graph_set_encoding(graph, encoding_string)) {
        rb_raise(rb_eArgError, "invalid encoding `%s` (should be utf8, utf16 or utf32)", encoding_string);
    }

    return Qnil;
}

// Graph#resolve_constant: (String, Array[String]) -> Declaration?
// Runs the resolver on a single constant reference to determine what it points to
static VALUE rdxr_graph_resolve_constant(VALUE self, VALUE const_name, VALUE nesting) {
    Check_Type(const_name, T_STRING);
    rdxi_check_array_of_strings(nesting);

    // Convert the given file paths into a char** array, so that we can pass to Rust
    size_t length = RARRAY_LEN(nesting);
    char **converted_file_paths = rdxi_str_array_to_char(nesting, length);

    void *graph;
    TypedData_Get_Struct(self, void *, &graph_type, graph);

    const CDeclaration *decl =
        rdx_graph_resolve_constant(graph, StringValueCStr(const_name), (const char **)converted_file_paths, length);

    for (size_t i = 0; i < length; i++) {
        free(converted_file_paths[i]);
    }
    free(converted_file_paths);

    if (decl == NULL) {
        return Qnil;
    }

    VALUE decl_class = rdxi_declaration_class_for_kind(decl->kind);
    VALUE argv[] = {self, ULL2NUM(decl->id)};
    free_c_declaration(decl);

    return rb_class_new_instance(2, argv, decl_class);
}

// Graph#resolve_require_path: (String require_path, Array[String] load_paths) -> Document?
// Resolves a require path to its Document.
static VALUE rdxr_graph_resolve_require_path(VALUE self, VALUE require_path, VALUE load_paths) {
    Check_Type(require_path, T_STRING);
    rdxi_check_array_of_strings(load_paths);

    void *graph;
    TypedData_Get_Struct(self, void *, &graph_type, graph);
    const char *path_str = StringValueCStr(require_path);

    size_t paths_len = RARRAY_LEN(load_paths);
    char **converted_paths = rdxi_str_array_to_char(load_paths, paths_len);

    const uint64_t *uri_id = rdx_resolve_require_path(graph, path_str, (const char **)converted_paths, paths_len);

    for (size_t i = 0; i < paths_len; i++) {
        free(converted_paths[i]);
    }
    free(converted_paths);

    if (uri_id == NULL) {
        return Qnil;
    }

    VALUE argv[] = {self, ULL2NUM(*uri_id)};
    free_u64(uri_id);
    return rb_class_new_instance(2, argv, cDocument);
}

// Graph#require_paths: (Array[String] load_path) -> Array[String]
// Returns all require paths for completion.
static VALUE rdxr_graph_require_paths(VALUE self, VALUE load_path) {
    rdxi_check_array_of_strings(load_path);

    void *graph;
    TypedData_Get_Struct(self, void *, &graph_type, graph);

    size_t paths_len = RARRAY_LEN(load_path);
    char **converted_paths = rdxi_str_array_to_char(load_path, paths_len);

    size_t out_count = 0;
    const char *const *results = rdx_require_paths(graph, (const char **)converted_paths, paths_len, &out_count);

    for (size_t i = 0; i < paths_len; i++) {
        free(converted_paths[i]);
    }
    free(converted_paths);

    if (results == NULL) {
        return rb_ary_new();
    }

    VALUE array = rb_ary_new_capa((long)out_count);
    for (size_t i = 0; i < out_count; i++) {
        rb_ary_push(array, rb_utf8_str_new_cstr(results[i]));
    }

    free_c_string_array(results, out_count);
    return array;
}

// Graph#diagnostics -> Array[Rubydex::Diagnostic]
static VALUE rdxr_graph_diagnostics(VALUE self) {
    void *graph;
    TypedData_Get_Struct(self, void *, &graph_type, graph);

    DiagnosticArray *array = rdx_graph_diagnostics(graph);
    if (array == NULL || array->len == 0) {
        if (array != NULL) {
            rdx_diagnostics_free(array);
        }
        return rb_ary_new();
    }

    VALUE diagnostics = rb_ary_new_capa((long)array->len);
    for (size_t i = 0; i < array->len; i++) {
        DiagnosticEntry entry = array->items[i];
        VALUE message = entry.message == NULL ? Qnil : rb_utf8_str_new_cstr(entry.message);
        VALUE rule = rb_str_new2(entry.rule);
        VALUE location = rdxi_build_location_value(entry.location);

        VALUE kwargs = rb_hash_new();
        rb_hash_aset(kwargs, ID2SYM(rb_intern("rule")), rule);
        rb_hash_aset(kwargs, ID2SYM(rb_intern("message")), message);
        rb_hash_aset(kwargs, ID2SYM(rb_intern("location")), location);

        VALUE diagnostic = rb_class_new_instance_kw(1, &kwargs, cDiagnostic, RB_PASS_KEYWORDS);
        rb_ary_push(diagnostics, diagnostic);
    }

    rdx_diagnostics_free(array);
    return diagnostics;
}

void rdxi_initialize_graph(VALUE mRubydex) {
    cGraph = rb_define_class_under(mRubydex, "Graph", rb_cObject);
    rb_define_alloc_func(cGraph, rdxr_graph_alloc);
    rb_define_method(cGraph, "index_all", rdxr_graph_index_all, 1);
    rb_define_method(cGraph, "index_source", rdxr_graph_index_source, 3);
    rb_define_method(cGraph, "delete_document", rdxr_graph_delete_document, 1);
    rb_define_method(cGraph, "resolve", rdxr_graph_resolve, 0);
    rb_define_method(cGraph, "resolve_constant", rdxr_graph_resolve_constant, 2);
    rb_define_method(cGraph, "declarations", rdxr_graph_declarations, 0);
    rb_define_method(cGraph, "documents", rdxr_graph_documents, 0);
    rb_define_method(cGraph, "constant_references", rdxr_graph_constant_references, 0);
    rb_define_method(cGraph, "method_references", rdxr_graph_method_references, 0);
    rb_define_method(cGraph, "diagnostics", rdxr_graph_diagnostics, 0);
    rb_define_method(cGraph, "[]", rdxr_graph_aref, 1);
    rb_define_method(cGraph, "search", rdxr_graph_search, 1);
    rb_define_method(cGraph, "set_encoding", rdxr_graph_set_encoding, 1);
    rb_define_method(cGraph, "resolve_require_path", rdxr_graph_resolve_require_path, 2);
    rb_define_method(cGraph, "require_paths", rdxr_graph_require_paths, 1);
}
