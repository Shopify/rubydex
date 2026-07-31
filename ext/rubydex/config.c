#include "config.h"
#include "rustbindings.h"
#include "utils.h"

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

    return TypedData_Wrap_Struct(klass, &config_type, result.config);
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

/*
 * call-seq:
 *   linter -> Rubydex::LinterConfig
 *
 * Returns the linter's settings, read from the `[linter]` section of the configuration file. The rules are keyed by
 * name and empty when the section is absent.
 */
static VALUE rdxr_config_linter(VALUE self) {
    // Return early if we already fetched the rule config from Rust and built the Ruby objects.
    VALUE linter = rb_ivar_get(self, id_linter);

    if (!NIL_P(linter)) {
        return linter;
    }

    CLinterRuleArray rule_array = rdx_config_linter_rules(rdxi_config_from_object(self));
    VALUE rules = rb_hash_new_capa((long)rule_array.len);

    for (size_t i = 0; i < rule_array.len; i++) {
        CLinterRule rule = rule_array.items[i];
        VALUE rule_name = rb_str_freeze(rb_utf8_str_new(rule.name, (long)rule.name_length));
        VALUE argv[] = { rule_name , rule.enabled ? Qtrue : Qfalse};

        rb_hash_aset(rules, rule_name, rb_class_new_instance(2, argv, cRuleConfig));
    }

    rdx_config_linter_rules_free(rule_array);

    linter = rb_class_new_instance(1, &rules, cLinterConfig);
    rb_ivar_set(self, id_linter, linter);

    return linter;
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
    rb_define_method(cConfig, "linter", rdxr_config_linter, 0);
}
