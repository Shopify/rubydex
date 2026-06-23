#include "query.h"
#include "declaration.h"
#include "definition.h"
#include "document.h"
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

    void *graph = rdxi_graph_from_object(graph_obj);

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

// Converts a structured result cell into a Ruby value. Node cells become real graph handles
// (Declaration / Definition / Document) built against `graph_obj`; lists recurse.
static VALUE cypher_cell_to_value(VALUE graph_obj, const struct CCell *cell) {
    switch (cell->tag) {
    case CCellTag_Null:
        return Qnil;
    case CCellTag_Bool:
        return cell->payload.bool_val ? Qtrue : Qfalse;
    case CCellTag_Int:
        return LL2NUM(cell->payload.int_val);
    case CCellTag_Str:
        return cell->payload.str_val == NULL ? Qnil : rb_utf8_str_new_cstr(cell->payload.str_val);
    case CCellTag_List: {
        VALUE array = rb_ary_new_capa((long)cell->payload.list.len);
        for (size_t i = 0; i < cell->payload.list.len; i++) {
            rb_ary_push(array, cypher_cell_to_value(graph_obj, &cell->payload.list.items[i]));
        }
        return array;
    }
    case CCellTag_Map: {
        VALUE hash = rb_hash_new();
        for (size_t i = 0; i < cell->payload.map.len; i++) {
            const char *raw_key = cell->payload.map.keys[i];
            VALUE key = raw_key == NULL ? Qnil : rb_utf8_str_new_cstr(raw_key);
            rb_hash_aset(hash, key, cypher_cell_to_value(graph_obj, &cell->payload.map.values[i]));
        }
        return hash;
    }
    case CCellTag_Node: {
        VALUE argv[] = {graph_obj, ULL2NUM(cell->payload.node.id)};
        VALUE klass;
        switch (cell->payload.node.category) {
        case CNodeCategory_Declaration:
            klass = rdxi_declaration_class_for_kind((CDeclarationKind)cell->payload.node.kind);
            break;
        case CNodeCategory_Definition:
            klass = rdxi_definition_class_for_kind((DefinitionKind)cell->payload.node.kind);
            break;
        case CNodeCategory_Document:
        default:
            klass = cDocument;
            break;
        }
        return rb_class_new_instance(2, argv, klass);
    }
    default:
        return Qnil;
    }
}

/*
 * call-seq:
 *   run(graph) -> Array[Hash[String, Object]]
 *
 * Runs this parsed query against +graph+ and returns the rows as Ruby objects: each row is a Hash
 * keyed by RETURN column name. Scalar cells become String/Integer/true/false/nil, lists become
 * Arrays, and node cells become Declaration / Definition / Document handles. Raises ArgumentError
 * on an execution error.
 */
static VALUE rdxr_query_run(VALUE self, VALUE graph_obj) {
    void *query;
    TypedData_Get_Struct(self, void *, &query_type, query);

    void *graph = rdxi_graph_from_object(graph_obj);

    struct CRunRows run = rdx_query_run_rows(query, graph);

    if (run.error != NULL) {
        VALUE message = rb_utf8_str_new_cstr(run.error);
        free_c_string(run.error);
        rb_raise(rb_eArgError, "%s", StringValueCStr(message));
    }

    struct CRowsIter *iter = run.iter;
    size_t column_count = rdx_rows_iter_column_count(iter);
    const char *const *columns = rdx_rows_iter_columns(iter);
    VALUE rows = rb_ary_new_capa((long)rdx_rows_iter_len(iter));

    struct CResultRow row;
    while (rdx_rows_iter_next(iter, &row)) {
        VALUE hash = rb_hash_new();
        for (size_t c = 0; c < row.len && c < column_count; c++) {
            VALUE key = rb_utf8_str_new_cstr(columns[c]);
            rb_hash_aset(hash, key, cypher_cell_to_value(graph_obj, &row.cells[c]));
        }
        rb_ary_push(rows, hash);
    }

    rdx_rows_iter_free(iter);
    return rows;
}

void rdxi_initialize_query(VALUE mRubydex) {
    VALUE cQuery = rb_define_class_under(mRubydex, "Query", rb_cObject);
    rb_undef_alloc_func(cQuery);
    rb_define_singleton_method(cQuery, "parse", rdxr_query_parse, 1);
    rb_define_singleton_method(cQuery, "schema", rdxr_cypher_schema, -1);
    rb_define_method(cQuery, "render", rdxr_query_render, -1);
    rb_define_method(cQuery, "run", rdxr_query_run, 1);
}
