#include "diagnostic.h"
#include "rustbindings.h"

/*
 * RDoc parser workaround for https://github.com/ruby/rdoc/issues/1744:
 * mRubydex = rb_define_module("Rubydex")
 */

VALUE cDiagnostic;

/*
 * call-seq:
 *   Rubydex::Diagnostic.graph_rule_names -> Array[String]
 *
 * Returns the names of all diagnostics that the graph can emit.
 */
static VALUE rdxr_graph_diagnostic_names(VALUE klass) {
    (void)klass;

    const char *const *names = NULL;
    size_t count = rdx_graph_diagnostic_names(&names);
    VALUE rule_names = rb_ary_new_capa((long)count);

    for (size_t i = 0; i < count; i++) {
        rb_ary_push(rule_names, rb_str_freeze(rb_utf8_str_new_cstr(names[i])));
    }

    free_c_string_array(names, count);
    return rb_obj_freeze(rule_names);
}

VALUE rdxi_build_diagnostic_severity_value(VALUE mRubydex, DiagnosticSeverity severity) {
    VALUE mSeverity = rb_const_get(mRubydex, rb_intern("Severity"));

    switch (severity) {
    case DiagnosticSeverity_Error:
        return rb_const_get(mSeverity, rb_intern("Error"));
    case DiagnosticSeverity_Warning:
        return rb_const_get(mSeverity, rb_intern("Warning"));
    case DiagnosticSeverity_Information:
        return rb_const_get(mSeverity, rb_intern("Information"));
    case DiagnosticSeverity_Hint:
        return rb_const_get(mSeverity, rb_intern("Hint"));
    default:
        rb_raise(rb_eRuntimeError, "Unknown DiagnosticSeverity: %d", severity);
    }

    return Qnil;
}

void rdxi_initialize_diagnostic(VALUE mRubydex) {
    cDiagnostic = rb_define_class_under(mRubydex, "Diagnostic", rb_cObject);
    rb_define_singleton_method(cDiagnostic, "graph_rule_names", rdxr_graph_diagnostic_names, 0);
}
