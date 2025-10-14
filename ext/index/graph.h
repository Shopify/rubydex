#ifndef GRAPH_H
#define GRAPH_H

#include "ruby.h"
#include "rustbindings.h"
#include "stdlib.h"
#include "string.h"

void initialize_graph(VALUE mIndex);

// Ruby handle classes for zero-copy graph access
extern VALUE cGraph;
extern VALUE cDeclarationHandle;
extern VALUE cDefinitionHandle;

#endif
