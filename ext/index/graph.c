#include "graph.h"
#include "declaration.h"
#include "ruby/internal/value_type.h"
#include "rustbindings.h"
#include "stdlib.h"
#include "string.h"

static VALUE cGraph;

// Free function for the custom Graph allocator. We always have to call into
// Rust to free data allocated by it
static void graph_free(void *ptr) {
    if (ptr) {
        idx_graph_free(ptr);
    }
}

// Single storage for the Graph data type shared across translation units
const rb_data_type_t graph_type = {
    "Graph", {0, graph_free, 0}, 0, 0, RUBY_TYPED_FREE_IMMEDIATELY};

// Custom allocator for the Graph class. Calls into Rust to create a new
// `Arc<Mutex<Graph>>` that gets stored internally as a void pointer
static VALUE rb_graph_alloc(VALUE klass) {
    void *graph = idx_graph_new();
    return TypedData_Wrap_Struct(klass, &graph_type, graph);
}

// Convert a Ruby array of strings into a double char pointer so that we can
// pass that to Rust. This copies the data so it must be freed
static char **str_array_to_char(VALUE array, size_t length) {
    char **converted_array = malloc(length * sizeof(char *));

    for (size_t i = 0; i < length; i++) {
        VALUE item = rb_ary_entry(array, i);
        const char *string = StringValueCStr(item);

        converted_array[i] = malloc(strlen(string) + 1);
        strcpy(converted_array[i], string);
    }

    return converted_array;
}

// Verify that the Ruby object is an array of strings or raise `TypeError`
static void check_array_of_strings(VALUE array) {
    Check_Type(array, T_ARRAY);

    for (long i = 0; i < RARRAY_LEN(array); i++) {
        VALUE item = rb_ary_entry(array, i);
        Check_Type(item, T_STRING);
    }
}

// Graph#index_all: (Array[String] file_paths) -> String?
// Returns the error messages concatenated as a single string if anything failed
// during indexing or `nil`
static VALUE rb_graph_index_all(VALUE self, VALUE file_paths) {
    check_array_of_strings(file_paths);

    // Convert the given file paths into a char** array, so that we can pass to
    // Rust
    size_t length = RARRAY_LEN(file_paths);
    char **converted_file_paths = str_array_to_char(file_paths, length);

    // Get the underying graph pointer and then invoke the Rust index all
    // implementation
    void *graph;
    TypedData_Get_Struct(self, void *, &graph_type, graph);
    const char *error_messages =
        idx_index_all_c(graph, (const char **)converted_file_paths, length);

    // Free the converted file paths and allow the GC to collect them
    for (size_t i = 0; i < length; i++) {
        free(converted_file_paths[i]);
    }
    free(converted_file_paths);

    // If indexing errors were returned, turn them into a Ruby string, call Rust
    // to free the CString it allocated and return the Ruby string
    if (error_messages != NULL) {
        VALUE error_string = rb_utf8_str_new_cstr(error_messages);
        free_c_string(error_messages);
        return error_string;
    }

    return Qnil;
}

static VALUE rb_graph_set_configuration(VALUE self, VALUE db_path) {
    Check_Type(db_path, T_STRING);

    void *graph;
    TypedData_Get_Struct(self, void *, &graph_type, graph);

    if (!idx_graph_set_configuration(graph, StringValueCStr(db_path))) {
        rb_raise(rb_eRuntimeError, "Failed to set the database configuration");
    }

    return Qnil;
}

// Graph#declarations: () -> Array[Declaration]
// Returns an array of all declarations in the graph
static VALUE rb_graph_declarations(VALUE self) {
    void *graph;
    TypedData_Get_Struct(self, void *, &graph_type, graph);

    size_t len = 0;
    const int64_t *ids = idx_graph_declaration_ids(graph, &len);
    VALUE arr = rb_ary_new_capa((long)len);
    for (size_t i = 0; i < len; i++) {
        VALUE handle =
            rb_funcall(cDeclaration, rb_intern("new"), 2, self, LL2NUM(ids[i]));
        rb_ary_push(arr, handle);
    }
    free_i64_array(ids, len);
    return arr;
}

// TODO: this should use name resolution instead of direct id lookup
// Graph#[]: (String name) -> Declaration?
// Returns the declaration resolved to the given name or `nil`
static VALUE rb_graph_aref(VALUE self, VALUE name) {
    Check_Type(name, T_STRING);
    void *graph;
    TypedData_Get_Struct(self, void *, &graph_type, graph);
    int64_t id = idx_graph_resolve_name(graph, StringValueCStr(name));

    if (id == 0) {
        return Qnil;
    }

    return rb_funcall(cDeclaration, rb_intern("new"), 2, self, LL2NUM(id));
}

void initialize_graph(VALUE mIndex) {
    cGraph = rb_define_class_under(mIndex, "Graph", rb_cObject);

    rb_define_alloc_func(cGraph, rb_graph_alloc);
    rb_define_method(cGraph, "index_all", rb_graph_index_all, 1);
    rb_define_method(cGraph, "set_configuration", rb_graph_set_configuration,
                     1);
    rb_define_method(cGraph, "declarations", rb_graph_declarations, 0);
    rb_define_method(cGraph, "[]", rb_graph_aref, 1);
}
