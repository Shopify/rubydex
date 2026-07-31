#include "config.h"
#include "rustbindings.h"
#include "utils.h"

static VALUE mRubydex;

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

void rdxi_initialize_config(VALUE moduleRubydex) {
    mRubydex = moduleRubydex;

    VALUE cConfig = rb_define_class_under(mRubydex, "Config", rb_cObject);
    rb_undef_alloc_func(cConfig);

    // A configuration can only be obtained through `load`; `new` would create an object with no Rust data behind it.
    rb_undef_method(rb_singleton_class(cConfig), "new");

    rb_define_singleton_method(cConfig, "load", rdxr_config_load, 1);
    rb_define_method(cConfig, "workspace_path", rdxr_config_workspace_path, 0);
}
