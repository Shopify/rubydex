#include "ruby.h"
#include "rustbindings.h"

static VALUE cRepository;

static void repository_free(void *ptr) {
    if (ptr) {
        free_repo(ptr);
    }
}

static const rb_data_type_t repository_type = {
    "Repository",
    { 0, repository_free, 0 },
    0, 0, RUBY_TYPED_FREE_IMMEDIATELY
};

static VALUE rb_repository_alloc(VALUE klass) {
    void* repository = new_repo();
    return TypedData_Wrap_Struct(klass, &repository_type, repository);
}

// Convert a Ruby array of strings into a double char pointer so that we can pass that to Rust. This copies the data
// so it must be freed
static char** str_array_to_char(VALUE array, long length) {
    char **converted_array = malloc(length * sizeof(char*));

    for (long i = 0; i < length; i++) {
        VALUE item = rb_ary_entry(array, i);
        const char* string = StringValueCStr(item);

        converted_array[i] = malloc(strlen(string) + 1);
        strcpy(converted_array[i], string);
    }

    return converted_array;
}

static VALUE rb_repository_index_all(VALUE self, VALUE file_paths) {
    rb_gc_register_address(&file_paths);
    long length = RARRAY_LEN(file_paths);
    char **converted_file_paths = str_array_to_char(file_paths, length);

    void* repository;
    TypedData_Get_Struct(self, void*, &repository_type, repository);
    index_all_c(repository, converted_file_paths, length);

    for (long i = 0; i < length; i++) {
        free(converted_file_paths[i]);
    }
    free(converted_file_paths);
    rb_gc_unregister_address(&file_paths);
    return Qnil;
}

static VALUE rb_repository_size(VALUE self) {
    void* repository;
    TypedData_Get_Struct(self, void*, &repository_type, repository);
    size_t size = repo_size(repository);
    return SIZET2NUM(size);
}

void initialize_repository(VALUE mIndex) {
    cRepository = rb_define_class_under(mIndex, "Repository", rb_cObject);

    rb_define_alloc_func(cRepository, rb_repository_alloc);
    rb_define_method(cRepository, "index_all", rb_repository_index_all, 1);
    rb_define_method(cRepository, "size", rb_repository_size, 0);
}
