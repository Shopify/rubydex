#include "ruby.h"
#include "rustbindings.h"

static VALUE cRepository;
static VALUE cDeclaration;

static void repository_free(void *ptr) {
    if (ptr) {
        idx_repository_free(ptr);
    }
}

static const rb_data_type_t repository_type = {
    "Repository",
    { 0, repository_free, 0 },
    0, 0, RUBY_TYPED_FREE_IMMEDIATELY
};

static VALUE rb_repository_alloc(VALUE klass) {
    void* repository = idx_repository_new();
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
    idx_index_all_c(repository, converted_file_paths, length);

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
    size_t size = idx_repository_size(repository);
    return SIZET2NUM(size);
}

static VALUE rb_repository_add_class(VALUE self, VALUE name, VALUE start_offset, VALUE end_offset) {
    void* repository;
    TypedData_Get_Struct(self, void*, &repository_type, repository);

    char* c_name = StringValueCStr(name);
    uint32_t c_start_offset = NUM2UINT(start_offset);
    uint32_t c_end_offset = NUM2UINT(end_offset);
    idx_repository_add_class_declaration(repository, c_name, c_start_offset, c_end_offset);
    return Qnil;
}

static VALUE instantiate_declaration(const Declaration* declaration) {
    uint32_t start_offset, end_offset;
    uint8_t tag = declaration->tag;

    switch (tag) {
    case CLASS:
        start_offset = declaration->class_.location.start_offset;
        end_offset = declaration->class_.location.end_offset;
        break;
    case MODULE:
        start_offset = declaration->module.location.start_offset;
        end_offset = declaration->module.location.end_offset;
        break;
    default:
        rb_raise(rb_eArgError, "Unknown declaration type");
        return Qnil;
    }

    VALUE args[] = { UINT2NUM(start_offset), UINT2NUM(end_offset) };
    return rb_class_new_instance(2, args, cDeclaration);
}

static VALUE rb_repository_get(VALUE self, VALUE name) {
    void* repository;
    TypedData_Get_Struct(self, void*, &repository_type, repository);

    char* c_name = StringValueCStr(name);
    const Declaration* declaration = idx_repository_get(repository, c_name);

    if (declaration == NULL) {
        return Qnil;
    }

    VALUE ruby_declaration = instantiate_declaration(declaration);
    idx_declaration_free(declaration);
    return ruby_declaration;
}

void initialize_repository(VALUE mIndex) {
    cRepository  = rb_define_class_under(mIndex, "Repository",  rb_cObject);
    cDeclaration = rb_define_class_under(mIndex, "Declaration", rb_cObject);

    rb_define_alloc_func(cRepository, rb_repository_alloc);
    rb_define_method(cRepository, "index_all", rb_repository_index_all, 1);
    rb_define_method(cRepository, "size", rb_repository_size, 0);
    rb_define_method(cRepository, "add_class", rb_repository_add_class, 3);
    rb_define_method(cRepository, "get", rb_repository_get, 1);
}
