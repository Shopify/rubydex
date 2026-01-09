#include "declaration.h"
#include "definition.h"
#include "diagnostic.h"
#include "document.h"
#include "graph.h"
#include "location.h"
#include "reference.h"

VALUE mRubydex;

void Init_rubydex(void) {
    rb_ext_ractor_safe(true);

    mRubydex = rb_define_module("Rubydex");
    initialize_graph(mRubydex);
    initialize_declaration(mRubydex);
    initialize_document(mRubydex);
    initialize_definition(mRubydex);
    initialize_location(mRubydex);
    initialize_diagnostic(mRubydex);
    initialize_reference(mRubydex);
}
