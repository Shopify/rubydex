#include "ruby.h"
#include "rustbindings.h"
#include <stdlib.h>
#include <string.h>

// Store the class references
static VALUE cRepository;
static VALUE cEntry;

static void index_free(void *ptr) {
    if (ptr) {
        dealloc_repository((CRepository*)ptr);
    }
}

static const rb_data_type_t index_type = {
    "Index",
    {0, index_free, 0,},
    0, 0, RUBY_TYPED_FREE_IMMEDIATELY
};

static VALUE rb_index_new(VALUE klass) {
    CRepository* index = get_repository();
    // TODO: Need to determine a potential way to allow interaction with the index without
    // wrapping the object
    return TypedData_Wrap_Struct(klass, &index_type, index);
}

static VALUE rb_repository_add_entry(VALUE self, VALUE name, VALUE value) {
    CRepository* index;
    TypedData_Get_Struct(self, CRepository, &index_type, index);

    const char *c_name = StringValueCStr(name);
    const char *c_value = StringValueCStr(value);

    repository_add_entry(index, c_name, c_value);
    return Qnil;
}

static VALUE rb_repository_get_entry(VALUE self, VALUE name) {
    CRepository* index;
    TypedData_Get_Struct(self, CRepository, &index_type, index);

    const char *c_name = StringValueCStr(name);
    CEntry *c_entry = repository_get_entry(index, c_name);

    if (!c_entry) {
        return Qnil;
    }

    const char *entry_name = entry_get_name(c_entry);
    const char *entry_value = entry_get_value(c_entry);

    // Create a plain Ruby Entry object and set instance variables
    // instead of using TypedData_Wrap_Struct
    VALUE ruby_entry = rb_class_new_instance(0, NULL, cEntry);
    rb_ivar_set(ruby_entry, rb_intern("@name"), entry_name ? rb_str_new_cstr(entry_name) : Qnil);
    rb_ivar_set(ruby_entry, rb_intern("@value"), entry_value ? rb_str_new_cstr(entry_value) : Qnil);

    delloc_entry(c_entry);

    return ruby_entry;
}

static VALUE rb_entry_name(VALUE self) {
    return rb_ivar_get(self, rb_intern("@name"));
}

static VALUE rb_entry_value(VALUE self) {
    return rb_ivar_get(self, rb_intern("@value"));
}
static VALUE rb_dump_to_cache(VALUE self) {
  CRepository *index;
  TypedData_Get_Struct(self, CRepository, &index_type, index);
  dump_to_cache(index);
  return Qnil;
}

// Initialization function for the Ruby extension
void Init_index(void) {
    VALUE mIndex = rb_define_module("Index");

    cRepository = rb_define_class_under(mIndex, "Repository", rb_cObject);
    rb_define_singleton_method(cRepository, "new", rb_index_new, 0);
    rb_define_method(cRepository, "add_entry", rb_repository_add_entry, 2);
    rb_define_method(cRepository, "get_entry", rb_repository_get_entry, 1);

    cEntry = rb_define_class_under(mIndex, "Entry", rb_cObject);
    rb_define_method(cEntry, "name", rb_entry_name, 0);
    rb_define_method(cEntry, "value", rb_entry_value, 0);

    rb_define_attr(cEntry, "name", 1, 1);
    rb_define_attr(cEntry, "value", 1, 1);
    rb_define_method(cRepository, "dump_to_cache", rb_dump_to_cache, 0);
}
