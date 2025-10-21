#include "declaration.h"
#include "graph.h"

VALUE mSaturn;

void Init_saturn(void) {
    rb_ext_ractor_safe(true);

    mSaturn = rb_define_module("Saturn");
    initialize_graph(mSaturn);
    initialize_declaration(mSaturn);
}
