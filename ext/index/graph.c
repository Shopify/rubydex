#include "graph.h"
#include "ruby/internal/value_type.h"

VALUE cGraph;
VALUE cDeclaration;
VALUE cDefinition;

// Free function for the custom Graph allocator. We always have to call into
// Rust to free data allocated by it
static void graph_free(void *ptr) {
  if (ptr) {
    idx_graph_free(ptr);
  }
}

static const rb_data_type_t graph_type = {
    "Graph", {0, graph_free, 0}, 0, 0, RUBY_TYPED_FREE_IMMEDIATELY};

typedef struct {
  VALUE graph_obj; // Ruby Graph object to keep it alive
  long long id;    // NameId or DefinitionId (i64)
} HandleData;

static void handle_mark(void *ptr) {
  if (ptr) {
    HandleData *data = (HandleData *)ptr;
    rb_gc_mark(data->graph_obj);
  }
}

static void handle_free(void *ptr) {
  if (ptr) {
    xfree(ptr);
  }
}

static const rb_data_type_t handle_type = {"IndexHandle",
                                           {handle_mark, handle_free, 0},
                                           0,
                                           0,
                                           RUBY_TYPED_FREE_IMMEDIATELY};

static VALUE declaration_handle_alloc(VALUE klass) {
  HandleData *data = ALLOC(HandleData);
  data->graph_obj = Qnil;
  data->id = 0;
  return TypedData_Wrap_Struct(klass, &handle_type, data);
}

static VALUE definition_handle_alloc(VALUE klass) {
  HandleData *data = ALLOC(HandleData);
  data->graph_obj = Qnil;
  data->id = 0;
  return TypedData_Wrap_Struct(klass, &handle_type, data);
}

static VALUE declaration_handle_initialize(VALUE self, VALUE graph_obj,
                                           VALUE id_val) {
  HandleData *data;
  TypedData_Get_Struct(self, HandleData, &handle_type, data);

  data->graph_obj = graph_obj;
  data->id = NUM2LL(id_val);
  return self;
}

static VALUE definition_handle_initialize(VALUE self, VALUE graph_obj,
                                          VALUE id_val) {
  HandleData *data;
  TypedData_Get_Struct(self, HandleData, &handle_type, data);

  data->graph_obj = graph_obj;
  data->id = NUM2LL(id_val);
  return self;
}

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

// DeclarationHandle#name -> String
static VALUE declaration_handle_name(VALUE self) {
  HandleData *data;
  TypedData_Get_Struct(self, HandleData, &handle_type, data);
  void *graph;
  TypedData_Get_Struct(data->graph_obj, void *, &graph_type, graph);
  const char *name = idx_graph_name_for(graph, (int64_t)data->id);
  if (name == NULL)
    return Qnil;
  VALUE str = rb_utf8_str_new_cstr(name);
  free_c_string(name);
  return str;
}

// DefinitionHandle#kind -> String
static VALUE definition_handle_kind(VALUE self) {
  HandleData *data;
  TypedData_Get_Struct(self, HandleData, &handle_type, data);
  void *graph;
  TypedData_Get_Struct(data->graph_obj, void *, &graph_type, graph);
  const char *kind = idx_graph_definition_kind(graph, (int64_t)data->id);
  if (kind == NULL)
    return Qnil;
  VALUE str = rb_utf8_str_new_cstr(kind);
  free_c_string(kind);
  return str;
}

// DefinitionHandle#uri -> String?
static VALUE definition_handle_uri(VALUE self) {
  HandleData *data;
  TypedData_Get_Struct(self, HandleData, &handle_type, data);
  void *graph;
  TypedData_Get_Struct(data->graph_obj, void *, &graph_type, graph);
  uint32_t start = 0, end = 0;
  const char *uri =
      idx_graph_definition_location(graph, (int64_t)data->id, &start, &end);
  if (uri == NULL)
    return Qnil;
  VALUE str = rb_utf8_str_new_cstr(uri);
  free_c_string(uri);
  return str;
}

// DefinitionHandle#location -> [Integer, Integer]?
static VALUE definition_handle_location(VALUE self) {
  HandleData *data;
  TypedData_Get_Struct(self, HandleData, &handle_type, data);
  void *graph;
  TypedData_Get_Struct(data->graph_obj, void *, &graph_type, graph);
  uint32_t start = 0, end = 0;
  const char *uri =
      idx_graph_definition_location(graph, (int64_t)data->id, &start, &end);
  if (uri == NULL)
    return Qnil;
  free_c_string(uri);
  VALUE arr = rb_ary_new_capa(2);
  rb_ary_push(arr, UINT2NUM(start));
  rb_ary_push(arr, UINT2NUM(end));
  return arr;
}

// DeclarationHandle#definitions -> [DefinitionHandle]
static VALUE declaration_handle_definitions(VALUE self) {
  HandleData *data;
  TypedData_Get_Struct(self, HandleData, &handle_type, data);

  void *graph;
  TypedData_Get_Struct(data->graph_obj, void *, &graph_type, graph);

  size_t len = 0;
  const int64_t *def_ids =
      idx_graph_definition_ids_for(graph, (int64_t)data->id, &len);
  VALUE arr = rb_ary_new_capa((long)len);
  for (size_t i = 0; i < len; i++) {
    VALUE handle = rb_funcall(cDefinition, rb_intern("new"), 2, data->graph_obj,
                              LL2NUM(def_ids[i]));
    rb_ary_push(arr, handle);
  }
  free_i64_array(def_ids, len);
  return arr;
}

// Pretty inspect for handles
static VALUE declaration_handle_inspect(VALUE self) {
  HandleData *data;
  TypedData_Get_Struct(self, HandleData, &handle_type, data);
  return rb_sprintf("#<NameId:%lld>", data->id);
}

static VALUE definition_handle_inspect(VALUE self) {
  HandleData *data;
  TypedData_Get_Struct(self, HandleData, &handle_type, data);
  return rb_sprintf("#<DefinitionId:%lld>", data->id);
}

void initialize_graph(VALUE mIndex) {
  cGraph = rb_define_class_under(mIndex, "Graph", rb_cObject);

  // Lightweight handle classes that only store ids
  cDeclaration = rb_define_class_under(mIndex, "Declaration", rb_cObject);
  rb_define_alloc_func(cDeclaration, declaration_handle_alloc);
  rb_define_method(cDeclaration, "initialize", declaration_handle_initialize,
                   2);
  rb_define_method(cDeclaration, "name", declaration_handle_name, 0);
  rb_define_method(cDeclaration, "definitions", declaration_handle_definitions,
                   0);
  rb_define_method(cDeclaration, "inspect", declaration_handle_inspect, 0);

  cDefinition = rb_define_class_under(mIndex, "Definition", rb_cObject);
  rb_define_alloc_func(cDefinition, definition_handle_alloc);
  rb_define_method(cDefinition, "initialize", definition_handle_initialize, 2);
  rb_define_method(cDefinition, "kind", definition_handle_kind, 0);
  rb_define_method(cDefinition, "uri", definition_handle_uri, 0);
  rb_define_method(cDefinition, "location", definition_handle_location, 0);
  rb_define_method(cDefinition, "inspect", definition_handle_inspect, 0);

  rb_define_alloc_func(cGraph, rb_graph_alloc);
  rb_define_method(cGraph, "index_all", rb_graph_index_all, 1);
  rb_define_method(cGraph, "set_configuration", rb_graph_set_configuration, 1);
  rb_define_method(cGraph, "declarations", rb_graph_declarations, 0);
}
