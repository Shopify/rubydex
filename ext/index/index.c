#include "ruby.h"
#include "rustbindings.h"
#include <stdlib.h>
#include <string.h>

// Store the class references
static VALUE cRepository;
static VALUE cEntry;
static VALUE cMember;
static VALUE cNestedMember;

// This method is for debugging ruby objects created within C.
static void d(VALUE v) {
    ID sym_puts = rb_intern("puts");
    ID sym_inspect = rb_intern("inspect");
    rb_funcall(rb_mKernel, sym_puts, 1,
        rb_funcall(v, sym_inspect, 0));
}

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
    
    // Access fields directly from the CEntry struct
    const char *entry_name = c_entry->name;
    const char *entry_value = c_entry->value;
    
    // Create a plain Ruby Entry object and set instance variables
    VALUE ruby_entry = rb_class_new_instance(0, NULL, cEntry);
    rb_ivar_set(ruby_entry, rb_intern("@name"), entry_name ? rb_str_new_cstr(entry_name) : Qnil);
    rb_ivar_set(ruby_entry, rb_intern("@value"), entry_value ? rb_str_new_cstr(entry_value) : Qnil);

    // Store the raw member pointer for lazy resolution
    rb_ivar_set(ruby_entry, rb_intern("@member_ptr"), ULONG2NUM((unsigned long)c_entry->member));
    
    // Don't free the c_entry immediately - we need the member pointer to remain valid
    // The member pointer points to memory that will be freed when we call dealloc_entry
    // For now, we'll keep the entry alive and defer cleanup
    // TODO: Implement proper lifecycle management - maybe store c_entry pointer too
    // and free it when the Ruby object is garbage collected
    // dealloc_entry(c_entry);
    
    return ruby_entry;
}

static VALUE rb_entry_member(VALUE self) {
    // Get the stored pointer and convert it back to CMember*
    VALUE member_ptr_val = rb_ivar_get(self, rb_intern("@member_ptr"));
    
    if (NIL_P(member_ptr_val)) {
        return Qnil;
    }
    
    // Convert the Ruby number back to a pointer
    CMember* c_member = (CMember*)NUM2ULONG(member_ptr_val);
    
    if (!c_member) {
        return Qnil;
    }
    
    // Access the actual member data from C/Rust
    VALUE ruby_member = rb_class_new_instance(0, NULL, cMember);
    rb_ivar_set(ruby_member, rb_intern("@value"), 
                c_member->value ? rb_str_new_cstr(c_member->value) : Qnil);
    
    // Store the raw nested_member pointer for lazy resolution
    rb_ivar_set(ruby_member, rb_intern("@nested_member_ptr"), 
                ULONG2NUM((unsigned long)c_member->nested_member));
    
    return ruby_member;
}

static VALUE rb_member_nested_member(VALUE self) {
    // Get the stored pointer and convert it back to CNestedMember*
    VALUE nested_member_ptr_val = rb_ivar_get(self, rb_intern("@nested_member_ptr"));
    
    if (NIL_P(nested_member_ptr_val)) {
        return Qnil;
    }
    
    // Convert the Ruby number back to a pointer
    CNestedMember* c_nested_member = (CNestedMember*)NUM2ULONG(nested_member_ptr_val);
    
    if (!c_nested_member) {
        return Qnil;
    }
    
    // Access the actual nested member data from C/Rust
    VALUE ruby_nested_member = rb_class_new_instance(0, NULL, cNestedMember);
    rb_ivar_set(ruby_nested_member, rb_intern("@value"), 
                c_nested_member->value ? rb_str_new_cstr(c_nested_member->value) : Qnil);
    
    return ruby_nested_member;
}

// Initialization function for the Ruby extension
void Init_index(void) {
    VALUE mIndex = rb_define_module("Index");
    
    cRepository = rb_define_class_under(mIndex, "Repository", rb_cObject);
    rb_define_singleton_method(cRepository, "new", rb_index_new, 0);
    rb_define_method(cRepository, "add_entry", rb_repository_add_entry, 2);
    rb_define_method(cRepository, "get_entry", rb_repository_get_entry, 1);
    
    cEntry = rb_define_class_under(mIndex, "Entry", rb_cObject);
    rb_define_method(cEntry, "member", rb_entry_member, 0);
    
    // Define attributes - Ruby automatically creates getters/setters
    rb_define_attr(cEntry, "name", 1, 1);
    rb_define_attr(cEntry, "value", 1, 1);
    
    cMember = rb_define_class_under(mIndex, "Member", rb_cObject);
    rb_define_method(cMember, "nested_member", rb_member_nested_member, 0);
    rb_define_attr(cMember, "value", 1, 1);
    
    cNestedMember = rb_define_class_under(mIndex, "NestedMember", rb_cObject);
    rb_define_attr(cNestedMember, "value", 1, 1);
} 
