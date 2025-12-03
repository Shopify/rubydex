#ifndef SATURN_DEFINITION_H
#define SATURN_DEFINITION_H

#include "ruby.h"
#include "rustbindings.h"

extern VALUE cDefinition;
extern VALUE cClassDefinition;
extern VALUE cSingletonClassDefinition;
extern VALUE cModuleDefinition;
extern VALUE cConstantDefinition;
extern VALUE cMethodDefinition;
extern VALUE cAttrAccessorDefinition;
extern VALUE cAttrReaderDefinition;
extern VALUE cAttrWriterDefinition;
extern VALUE cGlobalVariableDefinition;
extern VALUE cInstanceVariableDefinition;
extern VALUE cClassInstanceVariableDefinition;
extern VALUE cClassVariableDefinition;

void initialize_definition(VALUE mSaturn);

// Returns the Ruby class for a given DefinitionKind without calling back into Rust
VALUE definition_class_for_kind(DefinitionKind kind);

#endif // SATURN_DEFINITION_H
