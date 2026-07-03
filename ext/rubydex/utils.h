#ifndef RUBYDEX_UTILS_H
#define RUBYDEX_UTILS_H

#include "ruby.h"

// Convert a Ruby array of strings into a double char pointer so that we can pass that to Rust.
// This copies the data so it must be freed with rdxi_free_str_array
char **rdxi_str_array_to_char(VALUE array, size_t length);

// Free a char** array allocated by rdxi_str_array_to_char
void rdxi_free_str_array(char **array, size_t length);

// Verify that the Ruby object is an array of strings or raise `TypeError`
void rdxi_check_array_of_strings(VALUE array);

// Convert a Rust-owned C string to a Ruby string and release it with free_c_string.
// Returns nil when the Rust side returned NULL.
VALUE rdxi_owned_c_string_to_ruby(const char *string);

// Coerce an optional String, Symbol, or nil to a C string, returning `default_value` when the value
// is nil. A non-nil value must be a String or Symbol, otherwise a `TypeError` is raised. The
// returned pointer is owned by the Ruby VALUE and is only valid while it stays live.
const char *rdxi_symbol_or_string_cstr(VALUE value, const char *default_value);

// Yield body for iterating over declarations
VALUE rdxi_declarations_yield(VALUE args);

// Ensure function for iterating over declarations to always free the iterator
VALUE rdxi_declarations_ensure(VALUE args);

// Yield body for iterating over constant references
VALUE rdxi_constant_references_yield(VALUE args);

// Ensure function for iterating over constant references
VALUE rdxi_constant_references_ensure(VALUE args);

// Yield body for iterating over method references
VALUE rdxi_method_references_yield(VALUE args);

// Ensure function for iterating over method references
VALUE rdxi_method_references_ensure(VALUE args);

#endif // RUBYDEX_UTILS_H
