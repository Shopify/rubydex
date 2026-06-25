#ifndef RUBYDEX_HANDLE_H
#define RUBYDEX_HANDLE_H

#include "graph.h"
#include "ruby.h"

typedef struct {
    VALUE graph_obj; // Ruby Graph object to keep it alive
    uint64_t id;     // Canonical ID mapping to a DeclarationId, DefinitionId, UriId, etc. See `ids.rs`.
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
    .wrap_struct_name = "RubydexHandle",
    .function = {
        .dmark = handle_mark,
        .dfree = handle_free,
        .dsize = NULL,
        .dcompact = NULL,
    },
    .parent = NULL,
    .data = NULL,
    .flags = RUBY_TYPED_FREE_IMMEDIATELY,
};

static inline void *rdxi_graph_from_handle(VALUE self, HandleData **out_data) {
    HandleData *data;
    TypedData_Get_Struct(self, HandleData, &handle_type, data);

    void *graph;
    TypedData_Get_Struct(data->graph_obj, void *, &graph_type, graph);

    *out_data = data;

    return graph;
}

static VALUE rdxr_handle_alloc(VALUE klass) {
    HandleData *data = ALLOC(HandleData);

    *data = (HandleData) {
        .graph_obj = Qnil,
        .id = 0,
    };

    return TypedData_Wrap_Struct(klass, &handle_type, data);
}

static VALUE rdxr_handle_initialize(VALUE self, VALUE graph_obj, VALUE id_val) {
    HandleData *data;
    TypedData_Get_Struct(self, HandleData, &handle_type, data);

    *data = (HandleData) {
        .graph_obj = graph_obj,
        .id = NUM2ULL(id_val),
    };

    return self;
}

#endif // RUBYDEX_HANDLE_H
