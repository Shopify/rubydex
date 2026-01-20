#ifndef RUBYDEX_HANDLE_H
#define RUBYDEX_HANDLE_H

#include "ruby.h"

typedef struct {
    VALUE graph_obj; // Ruby Graph object to keep it alive
    int64_t id;      // Canonical ID (i64) mapping to a DeclarationId, DefinitionId, UriId, etc. See `ids.rs`.
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

static const rb_data_type_t handle_type = {
    "RubydexHandle", {handle_mark, handle_free, 0}, 0, 0, RUBY_TYPED_FREE_IMMEDIATELY};

static VALUE rdxr_handle_alloc(VALUE klass) {
    HandleData *data = ALLOC(HandleData);
    data->graph_obj = Qnil;
    data->id = 0;

    return TypedData_Wrap_Struct(klass, &handle_type, data);
}

static VALUE rdxr_handle_initialize(VALUE self, VALUE graph_obj, VALUE id_val) {
    HandleData *data;
    TypedData_Get_Struct(self, HandleData, &handle_type, data);
    data->graph_obj = graph_obj;
    data->id = NUM2LL(id_val);

    return self;
}

#endif // RUBYDEX_HANDLE_H
