#include "ruby.h"
#include "rustbindings.h"
#include <stdlib.h>
#include <string.h>

// Store the class references
static VALUE cRepository;

static VALUE rb_index_all(VALUE self, VALUE file_paths_array) {
    Check_Type(file_paths_array, T_ARRAY);
    
    long count = RARRAY_LEN(file_paths_array);
    const char **c_file_paths = malloc(count * sizeof(char*));
    
    for (long i = 0; i < count; i++) {
        VALUE file_path = rb_ary_entry(file_paths_array, i);
        c_file_paths[i] = StringValueCStr(file_path);
    }
    
    index_all_c(c_file_paths, (size_t)count);
    
    free(c_file_paths);
    return Qnil;
}

// Initialization function for the Ruby extension
void Init_index(void) {
    VALUE mIndex = rb_define_module("Index");
    
    cRepository = rb_define_class_under(mIndex, "Repository", rb_cObject);
    rb_define_singleton_method(cRepository, "index_all", rb_index_all, 1);
} 
