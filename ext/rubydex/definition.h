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
extern VALUE cGlobalVariableDefinition;
extern VALUE cInstanceVariableDefinition;
extern VALUE cClassVariableDefinition;
extern VALUE cMethodAliasDefinition;
extern VALUE cGlobalVariableAliasDefinition;

void initialize_definition(VALUE mRubydex);

// Returns the Ruby class for a given DefinitionKind without calling back into Rust
VALUE definition_class_for_kind(DefinitionKind kind);

#endif // RUBYDEX_DEFINITION_H
