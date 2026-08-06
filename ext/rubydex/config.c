#include "config.h"
#include "diagnostic.h"
#include "rustbindings.h"
#include "utils.h"

/*
 * RDoc parser workaround for https://github.com/ruby/rdoc/issues/1744:
 * mRubydex = rb_define_module("Rubydex")
 */

static VALUE mRubydex;
// Defined here so that the configuration can build them, but implemented in Ruby (`lib/rubydex/config.rb`), like the
// other value objects handed back to Ruby.
static VALUE cLinterConfig;
static VALUE cRuleConfig;
static ID id_linter;

// Free function for Rubydex::Config: releases the parsed configuration allocated by Rust.
static void config_free(void *ptr) {
    if (ptr) {
        rdx_config_free(ptr);
    }
}

// Body function for rb_ensure while building a Rubydex::LinterConfig.
static VALUE config_linter_build(VALUE opaque_rule_array) {
    CLinterRuleArray *rule_array = (CLinterRuleArray *)(uintptr_t)opaque_rule_array;
    VALUE rules = rb_hash_new_capa((long)rule_array->len);

    for (size_t i = 0; i < rule_array->len; i++) {
        CLinterRule rule = rule_array->items[i];
        VALUE rule_name = rb_str_freeze(rb_utf8_str_new(rule.name, (long)rule.name_length));
        VALUE exclude_patterns = rb_ary_new_capa((long)rule.exclude_patterns_length);
        for (size_t j = 0; j < rule.exclude_patterns_length; j++) {
            CConfigString pattern = rule.exclude_patterns[j];
            rb_ary_push(exclude_patterns, rb_str_freeze(rb_utf8_str_new(pattern.data, (long)pattern.length)));
        }
        rb_obj_freeze(exclude_patterns);

        VALUE severity = rule.severity == NULL
            ? Qnil
            : rdxi_build_diagnostic_severity_value(mRubydex, *rule.severity);
        VALUE argv[] = {rule_name, rule.enabled ? Qtrue : Qfalse, exclude_patterns, severity};

        rb_hash_aset(rules, rule_name, rb_class_new_instance(4, argv, cRuleConfig));
    }

    return rb_class_new_instance(1, &rules, cLinterConfig);
}

// Ensure function for rb_ensure while building a Rubydex::LinterConfig.
static VALUE config_linter_ensure(VALUE opaque_rule_array) {
    CLinterRuleArray *rule_array = (CLinterRuleArray *)(uintptr_t)opaque_rule_array;
    rdx_config_linter_rules_free(*rule_array);

    return Qnil;
}

static VALUE config_linter(VALUE config_obj) {
    CLinterRuleArray rule_array = rdx_config_linter_rules(rdxi_config_from_object(config_obj));
    VALUE opaque_rule_array = (VALUE)(uintptr_t)&rule_array;

    return rb_ensure(config_linter_build, opaque_rule_array, config_linter_ensure, opaque_rule_array);
}

const rb_data_type_t config_type = {
    .wrap_struct_name = "Rubydex::Config",
    .function = {
        .dmark = NULL,
        .dfree = config_free,
        .dsize = NULL,
        .dcompact = NULL,
    },
    .parent = NULL,
    .data = NULL,
    .flags = RUBY_TYPED_FREE_IMMEDIATELY | RUBY_TYPED_FROZEN_SHAREABLE,
};

/*
 * call-seq:
 *   Rubydex::Config.load(workspace_path) -> Rubydex::Config
 *
 * Loads the configuration of the workspace rooted at +workspace_path+, which is where its `rubydex.toml` is expected to
 * be. A workspace without a configuration file gets an empty configuration. Raises Rubydex::ConfigError if the file
 * exists, but cannot be read or is malformed.
 */
static VALUE rdxr_config_load(VALUE klass, VALUE workspace_path) {
    Check_Type(workspace_path, T_STRING);

    struct CConfigResult result = rdx_config_load(StringValueCStr(workspace_path));
    if (result.error != NULL) {
        VALUE message = rb_utf8_str_new_cstr(result.error);
        free_c_string(result.error);

        VALUE config_error = rb_const_get(mRubydex, rb_intern("ConfigError"));
        rb_exc_raise(rb_exc_new_str(config_error, message));
    }

    VALUE config = TypedData_Wrap_Struct(klass, &config_type, result.config);
    rb_ivar_set(config, id_linter, config_linter(config));

    return config;
}

/*
 * call-seq:
 *   workspace_path -> String
 *
 * Returns the root directory of the workspace this configuration was loaded for.
 */
static VALUE rdxr_config_workspace_path(VALUE self) {
    const char *result = rdx_config_workspace_path(rdxi_config_from_object(self));
    // A configuration always has a workspace, so there is no absent case to hand back to Ruby. NULL only means the
    // conversion itself failed, which is why this raises instead of returning nil.
    if (result == NULL) {
        rb_raise(rb_eRuntimeError, "Converting workspace path to Ruby string failed");
    }

    return rdxi_owned_c_string_to_ruby(result);
}

void rdxi_initialize_config(VALUE moduleRubydex) {
    mRubydex = moduleRubydex;

    cLinterConfig = rb_define_class_under(mRubydex, "LinterConfig", rb_cObject);
    cRuleConfig = rb_define_class_under(mRubydex, "RuleConfig", rb_cObject);
    id_linter = rb_intern("@linter");

    VALUE cConfig = rb_define_class_under(mRubydex, "Config", rb_cObject);
    rb_undef_alloc_func(cConfig);

    // A configuration can only be obtained through `load`; `new` would create an object with no Rust data behind it.
    rb_undef_method(rb_singleton_class(cConfig), "new");

    rb_define_singleton_method(cConfig, "load", rdxr_config_load, 1);
    rb_define_method(cConfig, "workspace_path", rdxr_config_workspace_path, 0);

    /* Returns the linter settings read from the `[linter]` section. Its rules are empty when the section is absent. */
    rb_define_attr(cConfig, "linter", true, false);
}
