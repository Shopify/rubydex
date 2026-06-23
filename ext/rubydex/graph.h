#ifndef RUBYDEX_GRAPH_H
#define RUBYDEX_GRAPH_H

#include "ruby.h"

extern const rb_data_type_t graph_type;

static inline void *rdxi_graph_from_object(VALUE graph_obj) {
    void *graph;
    TypedData_Get_Struct(graph_obj, void *, &graph_type, graph);
    return graph;
}

void rdxi_initialize_graph(VALUE mRubydex);

#endif // RUBYDEX_GRAPH_H
