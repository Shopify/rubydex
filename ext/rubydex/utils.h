#ifndef SATURN_UTILS_H
#define SATURN_UTILS_H

#include "ruby.h"

// Convert a Ruby array of strings into a double char pointer so that we can pass that to Rust.
// This copies the data so it must be freed
char **str_array_to_char(VALUE array, size_t length);

// Verify that the Ruby object is an array of strings or raise `TypeError`
void check_array_of_strings(VALUE array);

#endif // SATURN_UTILS_H
