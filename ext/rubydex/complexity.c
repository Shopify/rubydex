#include "complexity.h"
#include "rustbindings.h"
#include "utils.h"

/*
 * call-seq:
 *   Rubydex::Complexity.native_analyze(paths, format, top, methods_only, details, group) -> String
 *
 * Runs the complexity analysis pass over +paths+ (an array of path strings) and
 * returns the formatted report. +format+ is +"text"+ or +"json"+ (String or Symbol); +top+ is the
 * maximum number of entries in text output (0 prints all), ignored for JSON. +methods_only+
 * skips code outside methods; +details+ collects the per-construct breakdown;
 * +group+ groups text output by class with subtotals (ignored for JSON). Raises
 * ArgumentError on a fatal config or format error.
 */
static VALUE rdxr_complexity_analyze(VALUE self, VALUE paths, VALUE format, VALUE top, VALUE methods_only, VALUE details, VALUE group) {
    rdxi_check_array_of_strings(paths);

    long length = RARRAY_LEN(paths);
    char **paths_array = rdxi_str_array_to_char(paths, (size_t)length);

    struct CComplexityResult result = rdx_complexity_analyze(
        (const char *const *)paths_array,
        (size_t)length,
        rdxi_symbol_or_string_cstr(format, "text"),
        NUM2SIZET(top),
        RTEST(methods_only),
        RTEST(details),
        RTEST(group)
    );

    rdxi_free_str_array(paths_array, (size_t)length);

    if (result.error != NULL) {
        VALUE message = rb_utf8_str_new_cstr(result.error);
        free_c_string(result.error);
        rb_raise(rb_eArgError, "%s", StringValueCStr(message));
    }

    if (result.warnings != NULL) {
        VALUE stderr_io = rb_gv_get("$stderr");
        rb_io_write(stderr_io, rb_utf8_str_new_cstr(result.warnings));
        rb_io_write(stderr_io, rb_utf8_str_new_cstr("\n"));
        free_c_string(result.warnings);
    }

    return rdxi_owned_c_string_to_ruby(result.output);
}

/*
 * call-seq:
 *   Rubydex::Complexity.native_diff(baseline_json, current_json, format, top) -> String
 *
 * Diffs two complexity reports (JSON strings) and returns the formatted diff. +format+ is +"text"+
 * or +"json"+; +top+ caps each text section (0 prints all), ignored for JSON. Raises ArgumentError
 * if either JSON string is malformed or the format is unknown.
 */
static VALUE rdxr_complexity_diff(VALUE self, VALUE baseline_json, VALUE current_json, VALUE format, VALUE top) {
    Check_Type(baseline_json, T_STRING);
    Check_Type(current_json, T_STRING);

    struct CComplexityResult result = rdx_complexity_diff(
        StringValueCStr(baseline_json),
        StringValueCStr(current_json),
        rdxi_symbol_or_string_cstr(format, "text"),
        NUM2SIZET(top)
    );

    if (result.error != NULL) {
        VALUE message = rb_utf8_str_new_cstr(result.error);
        free_c_string(result.error);
        rb_raise(rb_eArgError, "%s", StringValueCStr(message));
    }

    return rdxi_owned_c_string_to_ruby(result.output);
}

void rdxi_initialize_complexity(VALUE mRubydex) {
    VALUE mComplexity = rb_define_module_under(mRubydex, "Complexity");
    // The `native_` prefix leaves the public kwargs API (in complexity.rb) free to own the
    // `analyze` / `diff` names without colliding with these module functions.
    rb_define_module_function(mComplexity, "native_analyze", rdxr_complexity_analyze, 6);
    rb_define_module_function(mComplexity, "native_diff", rdxr_complexity_diff, 4);
}
