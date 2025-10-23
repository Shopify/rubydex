#include "definition.h"
#include "handle.h"
#include "rustbindings.h"

VALUE cDefinition;
VALUE cClassDefinition;
VALUE cModuleDefinition;
VALUE cConstantDefinition;
VALUE cMethodDefinition;
VALUE cAttrAccessorDefinition;
VALUE cAttrReaderDefinition;
VALUE cAttrWriterDefinition;
VALUE cGlobalVariableDefinition;
VALUE cInstanceVariableDefinition;
VALUE cClassVariableDefinition;

// Keep this in sync with definition.rs
VALUE definition_class_for_kind(DefinitionKind kind) {
    switch (kind) {
    case DefinitionKind_Class:
        return cClassDefinition;
    case DefinitionKind_Module:
        return cModuleDefinition;
    case DefinitionKind_Constant:
        return cConstantDefinition;
    case DefinitionKind_Method:
        return cMethodDefinition;
    case DefinitionKind_AttrAccessor:
        return cAttrAccessorDefinition;
    case DefinitionKind_AttrReader:
        return cAttrReaderDefinition;
    case DefinitionKind_AttrWriter:
        return cAttrWriterDefinition;
    case DefinitionKind_GlobalVariable:
        return cGlobalVariableDefinition;
    case DefinitionKind_InstanceVariable:
        return cInstanceVariableDefinition;
    case DefinitionKind_ClassVariable:
        return cClassVariableDefinition;
    default:
        rb_raise(rb_eRuntimeError, "Unknown DefinitionKind: %d", kind);
    }
}

void initialize_definition(VALUE mSaturn) {
    cDefinition = rb_define_class_under(mSaturn, "Definition", rb_cObject);
    rb_define_alloc_func(cDefinition, sr_handle_alloc);
    rb_define_method(cDefinition, "initialize", sr_handle_initialize, 2);
    rb_funcall(rb_singleton_class(cDefinition), rb_intern("private"), 1, ID2SYM(rb_intern("new")));

    cClassDefinition = rb_define_class_under(mSaturn, "ClassDefinition", cDefinition);
    rb_define_alloc_func(cClassDefinition, sr_handle_alloc);
    rb_define_method(cClassDefinition, "initialize", sr_handle_initialize, 2);

    cModuleDefinition = rb_define_class_under(mSaturn, "ModuleDefinition", cDefinition);
    rb_define_alloc_func(cModuleDefinition, sr_handle_alloc);
    rb_define_method(cModuleDefinition, "initialize", sr_handle_initialize, 2);

    cConstantDefinition = rb_define_class_under(mSaturn, "ConstantDefinition", cDefinition);
    rb_define_alloc_func(cConstantDefinition, sr_handle_alloc);
    rb_define_method(cConstantDefinition, "initialize", sr_handle_initialize, 2);

    cMethodDefinition = rb_define_class_under(mSaturn, "MethodDefinition", cDefinition);
    rb_define_alloc_func(cMethodDefinition, sr_handle_alloc);
    rb_define_method(cMethodDefinition, "initialize", sr_handle_initialize, 2);

    cAttrAccessorDefinition = rb_define_class_under(mSaturn, "AttrAccessorDefinition", cDefinition);
    rb_define_alloc_func(cAttrAccessorDefinition, sr_handle_alloc);
    rb_define_method(cAttrAccessorDefinition, "initialize", sr_handle_initialize, 2);

    cAttrReaderDefinition = rb_define_class_under(mSaturn, "AttrReaderDefinition", cDefinition);
    rb_define_alloc_func(cAttrReaderDefinition, sr_handle_alloc);
    rb_define_method(cAttrReaderDefinition, "initialize", sr_handle_initialize, 2);

    cAttrWriterDefinition = rb_define_class_under(mSaturn, "AttrWriterDefinition", cDefinition);
    rb_define_alloc_func(cAttrWriterDefinition, sr_handle_alloc);
    rb_define_method(cAttrWriterDefinition, "initialize", sr_handle_initialize, 2);

    cGlobalVariableDefinition = rb_define_class_under(mSaturn, "GlobalVariableDefinition", cDefinition);
    rb_define_alloc_func(cGlobalVariableDefinition, sr_handle_alloc);
    rb_define_method(cGlobalVariableDefinition, "initialize", sr_handle_initialize, 2);

    cInstanceVariableDefinition = rb_define_class_under(mSaturn, "InstanceVariableDefinition", cDefinition);
    rb_define_alloc_func(cInstanceVariableDefinition, sr_handle_alloc);
    rb_define_method(cInstanceVariableDefinition, "initialize", sr_handle_initialize, 2);

    cClassVariableDefinition = rb_define_class_under(mSaturn, "ClassVariableDefinition", cDefinition);
    rb_define_alloc_func(cClassVariableDefinition, sr_handle_alloc);
    rb_define_method(cClassVariableDefinition, "initialize", sr_handle_initialize, 2);
}
