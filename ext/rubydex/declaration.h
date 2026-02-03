#ifndef RUBYDEX_DECLARATION_H
#define RUBYDEX_DECLARATION_H

#include "ruby.h"
#include "rustbindings.h"

extern VALUE cDeclaration;
extern VALUE cNamespace;
extern VALUE cClass;
extern VALUE cModule;
extern VALUE cSingletonClass;
extern VALUE cConstant;
extern VALUE cConstantAlias;
extern VALUE cMethod;
extern VALUE cGlobalVariable;
extern VALUE cInstanceVariable;
extern VALUE cClassVariable;

VALUE rdxi_declaration_class_for_kind(CDeclarationKind kind);
void rdxi_initialize_declaration(VALUE mRubydex);

#endif // RUBYDEX_DECLARATION_H
