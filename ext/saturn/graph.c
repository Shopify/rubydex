#include "graph.h"
#include "ruby/internal/value_type.h"
#include "utils.h"

static VALUE cGraph;

// Free function for the custom Graph allocator. We always have to call into
// Rust to free data allocated by it
static void graph_free(void *ptr) {
    if (ptr) {
        sat_graph_free(ptr);
    }
}

static const rb_data_type_t graph_type = {
    "Graph", {0, graph_free, 0}, 0, 0, RUBY_TYPED_FREE_IMMEDIATELY};

// Custom allocator for the Graph class. Calls into Rust to create a new
// `Arc<Mutex<Graph>>` that gets stored internally as a void pointer
static VALUE rb_graph_alloc(VALUE klass) {
    void *graph = sat_graph_new();
    return TypedData_Wrap_Struct(klass, &graph_type, graph);
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
        sat_index_all(graph, (const char **)converted_file_paths, length);

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

    if (!sat_graph_set_configuration(graph, StringValueCStr(db_path))) {
        rb_raise(rb_eRuntimeError, "Failed to set the database configuration");
    }

    return Qnil;
}

void initialize_graph(VALUE mSaturn) {
    cGraph = rb_define_class_under(mSaturn, "Graph", rb_cObject);

    rb_define_alloc_func(cGraph, rb_graph_alloc);
    rb_define_method(cGraph, "index_all", rb_graph_index_all, 1);
    rb_define_method(cGraph, "set_configuration", rb_graph_set_configuration,
                     1);
}
