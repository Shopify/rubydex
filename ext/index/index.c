#include "declaration.h"
#include "graph.h"
#include "ruby.h"
#include "rustbindings.h"

VALUE mIndex;
void Init_index(void) {
    rb_ext_ractor_safe(true);

    mIndex = rb_define_module("Index");
    initialize_graph(mIndex);
    initialize_declaration(mIndex);
}
