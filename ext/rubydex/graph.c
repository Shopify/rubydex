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
static VALUE eIndexingError;

// Free function for the custom Graph allocator. We always have to call into Rust to free data allocated by it
static void graph_free(void *ptr) {
    if (ptr) {
        rdx_graph_free(ptr);
    }
}

const rb_data_type_t graph_type = {"Graph", {0, graph_free, 0}, 0, 0, RUBY_TYPED_FREE_IMMEDIATELY};

// Custom allocator for the Graph class. Calls into Rust to create a new `Arc<Mutex<Graph>>` that gets stored internally
// as a void pointer
static VALUE sr_graph_alloc(VALUE klass) {
    void *graph = rdx_graph_new();
    return TypedData_Wrap_Struct(klass, &graph_type, graph);
}

// Graph#index_all: (Array[String] file_paths) -> nil
// Raises IndexingError if anything failed during indexing
static VALUE sr_graph_index_all(VALUE self, VALUE file_paths) {
    check_array_of_strings(file_paths);

    // Convert the given file paths into a char** array, so that we can pass to Rust
    size_t length = RARRAY_LEN(file_paths);
    char **converted_file_paths = str_array_to_char(file_paths, length);

    // Get the underying graph pointer and then invoke the Rust index all implementation
    void *graph;
    TypedData_Get_Struct(self, void *, &graph_type, graph);
    const char *error_messages = rdx_index_all(graph, (const char **)converted_file_paths, length);

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
        rb_raise(eIndexingError, "%s", StringValueCStr(error_string));
    }

    return Qnil;
}

// Body function for rb_ensure in Graph#declarations
static VALUE graph_declarations_yield(VALUE args) {
    VALUE self = rb_ary_entry(args, 0);
    void *iter = (void *)(uintptr_t)NUM2ULL(rb_ary_entry(args, 1));

    int64_t id = 0;
    while (rdx_graph_declarations_iter_next(iter, &id)) {
        VALUE argv[] = {self, LL2NUM(id)};
        VALUE handle = rb_class_new_instance(2, argv, cDeclaration);
        rb_yield(handle);
    }

    return Qnil;
}

// Ensure function for rb_ensure in Graph#declarations to always free the iterator
static VALUE graph_declarations_ensure(VALUE args) {
    void *iter = (void *)(uintptr_t)NUM2ULL(rb_ary_entry(args, 1));
    rdx_graph_declarations_iter_free(iter);

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
static VALUE sr_graph_declarations(VALUE self) {
    if (!rb_block_given_p()) {
        return rb_enumeratorize_with_size(self, rb_str_new2("declarations"), 0, NULL, graph_declarations_size);
    }

    void *graph;
    TypedData_Get_Struct(self, void *, &graph_type, graph);

    void *iter = rdx_graph_declarations_iter_new(graph);
    VALUE args = rb_ary_new_from_args(2, self, ULL2NUM((uintptr_t)iter));
    rb_ensure(graph_declarations_yield, args, graph_declarations_ensure, args);

    return self;
}

// Graph#search: () -> Enumerator[Declaration]
// Returns an enumerator that yields all declarations lazily
static VALUE sr_graph_search(VALUE self, VALUE query) {
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
    rb_ensure(graph_declarations_yield, args, graph_declarations_ensure, args);

    return self;
}

// Body function for rb_ensure in Graph#documents
static VALUE graph_documents_yield(VALUE args) {
    VALUE self = rb_ary_entry(args, 0);
    void *iter = (void *)(uintptr_t)NUM2ULL(rb_ary_entry(args, 1));

    int64_t id = 0;
    while (rdx_graph_documents_iter_next(iter, &id)) {
        VALUE argv[] = {self, LL2NUM(id)};
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
static VALUE sr_graph_documents(VALUE self) {
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
static VALUE sr_graph_aref(VALUE self, VALUE key) {
    void *graph;
    TypedData_Get_Struct(self, void *, &graph_type, graph);

    if (TYPE(key) != T_STRING) {
        rb_raise(rb_eTypeError, "expected String");
    }

    const int64_t *id_ptr = rdx_graph_get_declaration(graph, StringValueCStr(key));
    if (id_ptr == NULL) {
        return Qnil;
    }

    int64_t id = *id_ptr;
    free_i64(id_ptr);
    VALUE argv[] = {self, LL2NUM(id)};

    return rb_class_new_instance(2, argv, cDeclaration);
}

// Body function for rb_ensure for the reference enumerators
static VALUE graph_references_yield(VALUE args) {
    VALUE self = rb_ary_entry(args, 0);
    void *iter = (void *)(uintptr_t)NUM2ULL(rb_ary_entry(args, 1));

    int64_t id = 0;
    ReferenceKind kind;
    while (rdx_references_iter_next(iter, &id, &kind)) {
        VALUE ref_class = reference_class_for_kind(kind);
        VALUE argv[] = {self, LL2NUM(id)};
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
static VALUE sr_graph_constant_references(VALUE self) {
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
static VALUE sr_graph_method_references(VALUE self) {
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

// Graph#resolve: () -> self
// Runs the resolver to compute declarations and ownership
static VALUE sr_graph_resolve(VALUE self) {
    void *graph;
    TypedData_Get_Struct(self, void *, &graph_type, graph);
    rdx_graph_resolve(graph);
    return self;
}

// Graph#set_encoding: (String) -> void
// Sets the encoding used for transforming byte offsets into LSP code unit line/column positions
static VALUE sr_graph_set_encoding(VALUE self, VALUE encoding) {
    Check_Type(encoding, T_STRING);

    void *graph;
    TypedData_Get_Struct(self, void *, &graph_type, graph);

    char *encoding_string = StringValueCStr(encoding);
    if (!rdx_graph_set_encoding(graph, encoding_string)) {
        rb_raise(rb_eArgError, "invalid encoding `%s` (should be utf8, utf16 or utf32)", encoding_string);
    }

    return Qnil;
}

// Graph#add_method: (owner:, name:, file_path:, line:, column:) -> bool
static VALUE sr_graph_add_method(int argc, VALUE *argv, VALUE self) {
    VALUE kwargs;
    rb_scan_args(argc, argv, ":", &kwargs);

    VALUE owner = rb_hash_aref(kwargs, ID2SYM(rb_intern("owner")));
    VALUE name = rb_hash_aref(kwargs, ID2SYM(rb_intern("name")));
    VALUE file_path = rb_hash_aref(kwargs, ID2SYM(rb_intern("file_path")));
    VALUE line = rb_hash_aref(kwargs, ID2SYM(rb_intern("line")));
    VALUE column = rb_hash_aref(kwargs, ID2SYM(rb_intern("column")));

    if (NIL_P(owner) || NIL_P(name) || NIL_P(file_path) || NIL_P(line) || NIL_P(column)) {
        rb_raise(rb_eArgError, "missing required keyword arguments: owner, name, file_path, line, column");
    }

    void *graph;
    TypedData_Get_Struct(self, void *, &graph_type, graph);

    bool success = rdx_graph_add_method(
        graph,
        StringValueCStr(owner),
        StringValueCStr(name),
        StringValueCStr(file_path),
        NUM2UINT(line),
        NUM2UINT(column)
    );

    return success ? Qtrue : Qfalse;
}

// Graph#add_class: (name:, parent: nil, file_path:, line:, column:) -> bool
static VALUE sr_graph_add_class(int argc, VALUE *argv, VALUE self) {
    VALUE kwargs;
    rb_scan_args(argc, argv, ":", &kwargs);

    VALUE name = rb_hash_aref(kwargs, ID2SYM(rb_intern("name")));
    VALUE parent = rb_hash_aref(kwargs, ID2SYM(rb_intern("parent")));
    VALUE file_path = rb_hash_aref(kwargs, ID2SYM(rb_intern("file_path")));
    VALUE line = rb_hash_aref(kwargs, ID2SYM(rb_intern("line")));
    VALUE column = rb_hash_aref(kwargs, ID2SYM(rb_intern("column")));

    if (NIL_P(name) || NIL_P(file_path) || NIL_P(line) || NIL_P(column)) {
        rb_raise(rb_eArgError, "missing required keyword arguments");
    }

    void *graph;
    TypedData_Get_Struct(self, void *, &graph_type, graph);

    bool success = rdx_graph_add_class(
        graph,
        StringValueCStr(name),
        NIL_P(parent) ? NULL : StringValueCStr(parent),
        StringValueCStr(file_path),
        NUM2UINT(line),
        NUM2UINT(column)
    );

    return success ? Qtrue : Qfalse;
}

// Graph#add_module: (name:, file_path:, line:, column:) -> bool
static VALUE sr_graph_add_module(int argc, VALUE *argv, VALUE self) {
    VALUE kwargs;
    rb_scan_args(argc, argv, ":", &kwargs);

    VALUE name = rb_hash_aref(kwargs, ID2SYM(rb_intern("name")));
    VALUE file_path = rb_hash_aref(kwargs, ID2SYM(rb_intern("file_path")));
    VALUE line = rb_hash_aref(kwargs, ID2SYM(rb_intern("line")));
    VALUE column = rb_hash_aref(kwargs, ID2SYM(rb_intern("column")));

    if (NIL_P(name) || NIL_P(file_path) || NIL_P(line) || NIL_P(column)) {
        rb_raise(rb_eArgError, "missing required keyword arguments");
    }

    void *graph;
    TypedData_Get_Struct(self, void *, &graph_type, graph);

    bool success = rdx_graph_add_module(
        graph,
        StringValueCStr(name),
        StringValueCStr(file_path),
        NUM2UINT(line),
        NUM2UINT(column)
    );

    return success ? Qtrue : Qfalse;
}

// Graph#add_mixin: (target:, module_name:, type:) -> bool
static VALUE sr_graph_add_mixin(int argc, VALUE *argv, VALUE self) {
    VALUE kwargs;
    rb_scan_args(argc, argv, ":", &kwargs);

    VALUE target = rb_hash_aref(kwargs, ID2SYM(rb_intern("target")));
    VALUE module_name = rb_hash_aref(kwargs, ID2SYM(rb_intern("module_name")));
    VALUE type = rb_hash_aref(kwargs, ID2SYM(rb_intern("type")));

    if (NIL_P(target) || NIL_P(module_name) || NIL_P(type)) {
        rb_raise(rb_eArgError, "missing required keyword arguments: target, module_name, type");
    }

    void *graph;
    TypedData_Get_Struct(self, void *, &graph_type, graph);

    // Convert Ruby symbol to MixinType enum
    enum MixinType mixin_type;
    if (rb_intern("include") == SYM2ID(type)) {
        mixin_type = MixinType_Include;
    } else if (rb_intern("prepend") == SYM2ID(type)) {
        mixin_type = MixinType_Prepend;
    } else if (rb_intern("extend") == SYM2ID(type)) {
        mixin_type = MixinType_Extend;
    } else {
        rb_raise(rb_eArgError, "type must be :include, :prepend, or :extend");
    }

    bool success = rdx_graph_add_mixin(
        graph,
        StringValueCStr(target),
        StringValueCStr(module_name),
        mixin_type
    );

    return success ? Qtrue : Qfalse;
}

// Graph#register_included_hook: (module_name:, extend_module:) -> bool
static VALUE sr_graph_register_included_hook(int argc, VALUE *argv, VALUE self) {
    VALUE kwargs;
    rb_scan_args(argc, argv, ":", &kwargs);

    VALUE module_name = rb_hash_aref(kwargs, ID2SYM(rb_intern("module_name")));
    VALUE extend_module = rb_hash_aref(kwargs, ID2SYM(rb_intern("extend_module")));

    if (NIL_P(module_name) || NIL_P(extend_module)) {
        rb_raise(rb_eArgError, "missing required keyword arguments: module_name, extend_module");
    }

    void *graph;
    TypedData_Get_Struct(self, void *, &graph_type, graph);

    bool success = rdx_graph_register_included_hook(
        graph,
        StringValueCStr(module_name),
        StringValueCStr(extend_module)
    );

    return success ? Qtrue : Qfalse;
}

// Graph#diagnostics -> Array[Rubydex::Diagnostic]
static VALUE sr_graph_diagnostics(VALUE self) {
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
        VALUE location = build_location_value(entry.location);

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

void initialize_graph(VALUE mRubydex) {
    VALUE eRubydexError = rb_const_get(mRubydex, rb_intern("Error"));
    eIndexingError = rb_define_class_under(mRubydex, "IndexingError", eRubydexError);

    cGraph = rb_define_class_under(mRubydex, "Graph", rb_cObject);
    rb_define_alloc_func(cGraph, sr_graph_alloc);
    rb_define_method(cGraph, "index_all", sr_graph_index_all, 1);
    rb_define_method(cGraph, "resolve", sr_graph_resolve, 0);
    rb_define_method(cGraph, "declarations", sr_graph_declarations, 0);
    rb_define_method(cGraph, "documents", sr_graph_documents, 0);
    rb_define_method(cGraph, "constant_references", sr_graph_constant_references, 0);
    rb_define_method(cGraph, "method_references", sr_graph_method_references, 0);
    rb_define_method(cGraph, "diagnostics", sr_graph_diagnostics, 0);
    rb_define_method(cGraph, "[]", sr_graph_aref, 1);
    rb_define_method(cGraph, "search", sr_graph_search, 1);
    rb_define_method(cGraph, "set_encoding", sr_graph_set_encoding, 1);
    rb_define_method(cGraph, "add_method", sr_graph_add_method, -1);
    rb_define_method(cGraph, "add_class", sr_graph_add_class, -1);
    rb_define_method(cGraph, "add_module", sr_graph_add_module, -1);
    rb_define_method(cGraph, "add_mixin", sr_graph_add_mixin, -1);
    rb_define_method(cGraph, "register_included_hook", sr_graph_register_included_hook, -1);
}
