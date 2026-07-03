#include "query.h"
#include "graph.h"
#include "rustbindings.h"
#include "utils.h"

/*
 * call-seq:
 *   Rubydex::Query.schema(format = :table) -> String
 *
 * Returns a description of the queryable Cypher schema. +format+ may be +:table+ (default) or
 * +:json+. The schema is static, so it does not require a graph.
 */
static VALUE rdxr_cypher_schema(int argc, VALUE *argv, VALUE self) {
    VALUE format;
    rb_scan_args(argc, argv, "01", &format);

    const char *output = rdx_cypher_schema(rdxi_symbol_or_string_cstr(format, "table"));
    VALUE result = output == NULL ? rb_utf8_str_new_cstr("") : rb_utf8_str_new_cstr(output);
    if (output != NULL) {
        free_c_string(output);
    }

    return result;
}

// Free function for Rubydex::Query: releases the parsed query allocated by Rust.
static void query_free(void *ptr) {
    if (ptr) {
        rdx_cypher_query_free(ptr);
    }
}

static const rb_data_type_t query_type = {
    .wrap_struct_name = "Rubydex::Query",
    .function = {
        .dmark = NULL,
        .dfree = query_free,
        .dsize = NULL,
        .dcompact = NULL,
    },
    .parent = NULL,
    .data = NULL,
    .flags = RUBY_TYPED_FREE_IMMEDIATELY,
};

/*
 * call-seq:
 *   Rubydex::Query.parse(query) -> Rubydex::Query
 *
 * Parses a Cypher query into an opaque, reusable object without needing a graph. Raises
 * ArgumentError on a syntax error, so a query can be validated before building a graph.
 */
static VALUE rdxr_query_parse(VALUE klass, VALUE query) {
    Check_Type(query, T_STRING);

    struct CParseResult result = rdx_cypher_parse(StringValueCStr(query));
    if (result.error != NULL) {
        VALUE message = rb_utf8_str_new_cstr(result.error);
        free_c_string(result.error);
        rb_raise(rb_eArgError, "%s", StringValueCStr(message));
    }

    return TypedData_Wrap_Struct(klass, &query_type, result.query);
}

/*
 * call-seq:
 *   render(graph, format = :table) -> String
 *
 * Runs this parsed query against +graph+ and returns the formatted output. +format+ may be
 * +:table+ (default) or +:json+. Raises ArgumentError on an execution or format error.
 */
static VALUE rdxr_query_render(int argc, VALUE *argv, VALUE self) {
    VALUE graph_obj, format;
    rb_scan_args(argc, argv, "11", &graph_obj, &format);

    void *query;
    TypedData_Get_Struct(self, void *, &query_type, query);

    void *graph;
    TypedData_Get_Struct(graph_obj, void *, &graph_type, graph);

    struct CQueryResult result = rdx_query_run(query, graph, rdxi_symbol_or_string_cstr(format, "table"));

    if (result.error != NULL) {
        VALUE message = rb_utf8_str_new_cstr(result.error);
        free_c_string(result.error);
        rb_raise(rb_eArgError, "%s", StringValueCStr(message));
    }

    VALUE output = result.output == NULL ? rb_utf8_str_new_cstr("") : rb_utf8_str_new_cstr(result.output);
    if (result.output != NULL) {
        free_c_string(result.output);
    }

    return output;
}

void rdxi_initialize_query(VALUE mRubydex) {
    VALUE cQuery = rb_define_class_under(mRubydex, "Query", rb_cObject);
    rb_undef_alloc_func(cQuery);
    rb_define_singleton_method(cQuery, "parse", rdxr_query_parse, 1);
    rb_define_singleton_method(cQuery, "schema", rdxr_cypher_schema, -1);
    rb_define_method(cQuery, "render", rdxr_query_render, -1);
}
