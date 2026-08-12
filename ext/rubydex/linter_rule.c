#include "linter_rule.h"
#include "rustbindings.h"

/*
 * RDoc parser workaround for https://github.com/ruby/rdoc/issues/1744:
 * mRubydex = rb_define_module("Rubydex")
 */

struct BuiltInRuleNames {
    const char *const *names;
    size_t count;
};

static VALUE linter_rule_build_built_in_rules_names(VALUE opaque_names) {
    struct BuiltInRuleNames *names = (struct BuiltInRuleNames *)(uintptr_t)opaque_names;
    VALUE rule_names = rb_ary_new_capa((long)names->count);

    for (size_t i = 0; i < names->count; i++) {
        rb_ary_push(rule_names, rb_str_freeze(rb_utf8_str_new_cstr(names->names[i])));
    }

    return rb_obj_freeze(rule_names);
}

static VALUE linter_rule_free_built_in_rules_names(VALUE opaque_names) {
    struct BuiltInRuleNames *names = (struct BuiltInRuleNames *)(uintptr_t)opaque_names;
    free_c_string_array(names->names, names->count);
    return Qnil;
}

/*
 * call-seq:
 *   Rubydex::Linter::Rule.built_in_rules_names -> Array[String]
 *
 * Returns the names of all built-in linter rules.
 */
static VALUE rdxr_linter_rule_built_in_rules_names(VALUE klass) {
    (void)klass;

    struct BuiltInRuleNames names = {.names = NULL, .count = 0};
    names.count = rdx_graph_diagnostic_names(&names.names);
    VALUE opaque_names = (VALUE)(uintptr_t)&names;

    return rb_ensure(
        linter_rule_build_built_in_rules_names,
        opaque_names,
        linter_rule_free_built_in_rules_names,
        opaque_names
    );
}

void rdxi_initialize_linter_rule(VALUE mRubydex) {
    VALUE mLinter = rb_define_module_under(mRubydex, "Linter");
    VALUE cRule = rb_define_class_under(mLinter, "Rule", rb_cObject);
    rb_define_singleton_method(cRule, "built_in_rules_names", rdxr_linter_rule_built_in_rules_names, 0);
}
