#include "declaration.h"
#include "definition.h"
#include "document.h"
#include "graph.h"
#include "location.h"
#include "reference.h"

VALUE mSaturn;

void Init_saturn(void) {
    rb_ext_ractor_safe(true);

    mSaturn = rb_define_module("Saturn");
    initialize_graph(mSaturn);
    initialize_declaration(mSaturn);
    initialize_document(mSaturn);
    initialize_definition(mSaturn);
    initialize_location(mSaturn);
    initialize_reference(mSaturn);
}
