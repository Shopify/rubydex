#ifndef IDX_HANDLE_H
#define IDX_HANDLE_H

#include "ruby.h"

typedef struct {
    VALUE graph_obj; // Ruby Graph object to keep it alive
    long long id;    // Canonical ID (i64)
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

#endif
