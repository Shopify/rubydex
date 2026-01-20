#include "utils.h"

// Convert a Ruby array of strings into a double char pointer so that we can pass that to Rust.
// This copies the data so it must be freed
char **rdxi_str_array_to_char(VALUE array, size_t length) {
    char **converted_array = malloc(length * sizeof(char *));

    for (size_t i = 0; i < length; i++) {
        VALUE item = rb_ary_entry(array, i);
        const char *string = StringValueCStr(item);

        converted_array[i] = malloc(strlen(string) + 1);
        strcpy(converted_array[i], string);
    }

    return converted_array;
}

// Verify that the Ruby object is an array of strings or raise `TypeError`
void rdxi_check_array_of_strings(VALUE array) {
    Check_Type(array, T_ARRAY);

    for (long i = 0; i < RARRAY_LEN(array); i++) {
        VALUE item = rb_ary_entry(array, i);
        Check_Type(item, T_STRING);
    }
}
