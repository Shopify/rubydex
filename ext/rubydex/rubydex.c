#include "declaration.h"
#include "definition.h"
#include "diagnostic.h"
#include "document.h"
#include "graph.h"
#include "location.h"
#include "reference.h"
#include "index_result.h"

VALUE mRubydex;

void Init_rubydex(void) {
    rb_ext_ractor_safe(true);

    mRubydex = rb_define_module("Rubydex");
    rdxi_initialize_index_result(mRubydex);
    rdxi_initialize_graph(mRubydex);
    rdxi_initialize_declaration(mRubydex);
    rdxi_initialize_document(mRubydex);
    rdxi_initialize_definition(mRubydex);
    rdxi_initialize_location(mRubydex);
    rdxi_initialize_diagnostic(mRubydex);
    rdxi_initialize_reference(mRubydex);
}
