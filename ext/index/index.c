#include "ruby.h"
#include "rustbindings.h"
#include <stdlib.h>
#include <string.h>

// Store the class references
static VALUE cRepository;
static VALUE cEntry;
static VALUE cPoint;
static VALUE cMessage;

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

// Integer functions for boundary performance testing
static VALUE rb_get_constant_number(VALUE self) {
    uint32_t result = get_constant_number();
    return UINT2NUM(result);
}

static VALUE rb_increment_number(VALUE self, VALUE input) {
    uint32_t c_input = NUM2UINT(input);
    uint32_t result = increment_number(c_input);
    return UINT2NUM(result);
}

// Point functions for object creation benchmarking
static void point_free(void *ptr) {
    if (ptr) {
        dealloc_point((CPoint*)ptr);
    }
}

static const rb_data_type_t point_type = {
    "Point",
    {0, point_free, 0,},
    0, 0, RUBY_TYPED_FREE_IMMEDIATELY
};

// Wrap a native CPoint* in a Ruby object
static VALUE wrap_point(CPoint* point) {
    return TypedData_Wrap_Struct(cPoint, &point_type, point);
}

static void initialize_point_ivars(VALUE ruby_point, uint32_t x, uint32_t y) {
    rb_ivar_set(ruby_point, rb_intern("@x"), UINT2NUM(x));
    rb_ivar_set(ruby_point, rb_intern("@y"), UINT2NUM(y));
}

static VALUE rb_create_point(VALUE klass, VALUE x, VALUE y) {
    uint32_t c_x = NUM2UINT(x);
    uint32_t c_y = NUM2UINT(y);
    CPoint* point = create_point(c_x, c_y);
    
    VALUE ruby_point = wrap_point(point);
    
    initialize_point_ivars(ruby_point, c_x, c_y);
    
    return ruby_point;
}

static void message_free(void *ptr) {
    if (ptr) {
        dealloc_message((CMessage*)ptr);
    }
}

static const rb_data_type_t message_type = {
    "Message",
    {0, message_free, 0,},
    0, 0, RUBY_TYPED_FREE_IMMEDIATELY
};

static VALUE wrap_message(CMessage* message) {
    return TypedData_Wrap_Struct(cMessage, &message_type, message);
}

static void initialize_message_ivars(VALUE ruby_message, VALUE content) {
    rb_ivar_set(ruby_message, rb_intern("@content"), content);
}

static VALUE rb_create_message(VALUE klass, VALUE content) {
    const char *c_content = StringValueCStr(content);
    CMessage* message = create_message(c_content);
    VALUE ruby_message = wrap_message(message);
    initialize_message_ivars(ruby_message, content);
    return ruby_message;
}

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

    rb_define_singleton_method(mIndex, "get_constant_number", rb_get_constant_number, 0);
    rb_define_singleton_method(mIndex, "increment_number", rb_increment_number, 1);

    cPoint = rb_define_class_under(mIndex, "Point", rb_cObject);
    rb_define_singleton_method(cPoint, "new", rb_create_point, 2);
    rb_define_attr(cPoint, "x", 1, 0);  
    rb_define_attr(cPoint, "y", 1, 0);  

    cMessage = rb_define_class_under(mIndex, "Message", rb_cObject);
    // NOTE: Choosing to use definition via singleton method instead of 
    // defining an alloc function becasue rb_define_alloc_func doesn't 
    // take any arguments after the klass.
    rb_define_singleton_method(cMessage, "new", rb_create_message, 1);
    rb_define_attr(cMessage, "content", 1, 0);  // readable, not writable
}
