#ifndef RUBYDEX_DEFINITION_H
#define RUBYDEX_DEFINITION_H

#include "ruby.h"
#include "rustbindings.h"

extern VALUE cDefinition;
extern VALUE cClassDefinition;
extern VALUE cSingletonClassDefinition;
extern VALUE cModuleDefinition;
extern VALUE cConstantDefinition;
extern VALUE cConstantAliasDefinition;
extern VALUE cMethodDefinition;
extern VALUE cAttrAccessorDefinition;
extern VALUE cAttrReaderDefinition;
extern VALUE cAttrWriterDefinition;
extern VALUE cGlobalVariableDefinition;
extern VALUE cInstanceVariableDefinition;
extern VALUE cClassVariableDefinition;
extern VALUE cMethodAliasDefinition;
extern VALUE cGlobalVariableAliasDefinition;

void rdxi_initialize_definition(VALUE mRubydex);

// Returns the definition ID after verifying that the definition belongs to `graph_obj`.
uint64_t rdxi_definition_id_for_graph(VALUE definition, VALUE graph_obj);

// Returns the Ruby class for a given DefinitionKind without calling back into Rust
VALUE rdxi_definition_class_for_kind(DefinitionKind kind);

#endif // RUBYDEX_DEFINITION_H
