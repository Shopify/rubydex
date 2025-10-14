#include "graph.h"
#include "ruby/internal/value_type.h"

static VALUE cGraph;

// Free function for the custom Graph allocator. We always have to call into
// Rust to free data allocated by it
static void graph_free(void *ptr) {
  if (ptr) {
    idx_graph_free(ptr);
  }
}

static const rb_data_type_t graph_type = {
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

static VALUE rb_graph_declarations(VALUE self) {
  void *graph;
  TypedData_Get_Struct(self, void *, &graph_type, graph);

  const char **declarations = idx_graph_declarations(graph);

  VALUE declarations_array = rb_ary_new();
  for (size_t i = 0; declarations[i] != NULL; i++) {
    rb_ary_push(declarations_array, rb_str_new_cstr(declarations[i]));
  }

  return declarations_array;
}

static VALUE rb_graph_definitions_for(VALUE self, VALUE declaration) {
  void *graph;
  TypedData_Get_Struct(self, void *, &graph_type, graph);
  const char **definitions =
      idx_graph_definitions_for(graph, StringValueCStr(declaration));
  return rb_str_new_cstr(definitions);
}

void initialize_graph(VALUE mIndex) {
  cGraph = rb_define_class_under(mIndex, "Graph", rb_cObject);

  rb_define_alloc_func(cGraph, rb_graph_alloc);
  rb_define_method(cGraph, "index_all", rb_graph_index_all, 1);
  rb_define_method(cGraph, "set_configuration", rb_graph_set_configuration, 1);
  rb_define_method(cGraph, "declarations", rb_graph_declarations, 0);
  rb_define_method(cGraph, "definitions_for", rb_graph_definitions_for, 1);
}
