#include "ruby.h"
#include "rustbindings.h"
#include "graph.h"

VALUE mIndex;
void Init_index(void) {
    rb_ext_ractor_safe(true);

    mIndex = rb_define_module("Index");
    initialize_graph(mIndex);
}
