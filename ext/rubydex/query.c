#include "query.h"
#include "declaration.h"
#include "definition.h"
#include "document.h"
#include "graph.h"
#include "rustbindings.h"
#include "utils.h"

/*
 * RDoc parser workaround for https://github.com/ruby/rdoc/issues/1744:
 * mRubydex = rb_define_module("Rubydex")
 */

static VALUE mRubydex;
static VALUE cQueryResult;

// Raises the Ruby error that matches a Cypher failure reported by Rust and releases `message`.
// Syntax and execution failures get a Rubydex error; everything else is a Ruby argument error.
NORETURN(static void raise_query_error(const char *message, CQueryErrorKind kind));

static void raise_query_error(const char *message, CQueryErrorKind kind) {
    VALUE error_message = rb_utf8_str_new_cstr(message);
    free_c_string(message);

    VALUE error_class;
    switch (kind) {
    case CQueryErrorKind_Syntax:
        error_class = rb_const_get(mRubydex, rb_intern("QuerySyntaxError"));
        break;
    case CQueryErrorKind_Execution:
        error_class = rb_const_get(mRubydex, rb_intern("QueryExecutionError"));
        break;
    default:
        error_class = rb_eArgError;
        break;
    }

    rb_exc_raise(rb_exc_new_str(error_class, error_message));
}

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
 * Rubydex::QuerySyntaxError on a syntax error, so a query can be validated before building a graph.
 */
static VALUE rdxr_query_parse(VALUE klass, VALUE query) {
    Check_Type(query, T_STRING);

    struct CParseResult result = rdx_cypher_parse(StringValueCStr(query));
    if (result.error != NULL) {
        raise_query_error(result.error, result.error_kind);
    }

    return TypedData_Wrap_Struct(klass, &query_type, result.query);
}

// Backing data for Rubydex::Query::Result: the executed result set plus the graph it came from.
typedef struct {
    void *result_set; // Result set owned by Rust, released with rdx_result_set_free
    VALUE graph_obj;  // Ruby Graph object to keep it alive, since node cells build handles from it
    VALUE rows;       // Memoized array of row hashes, nil until `rows` builds it
} QueryResultData;

// Marks the references movable, so that a compaction can relocate them. `query_result_compact`
// then writes their new locations back into the struct.
static void query_result_mark(void *ptr) {
    if (ptr) {
        QueryResultData *data = (QueryResultData *)ptr;
        rb_gc_mark_movable(data->graph_obj);
        rb_gc_mark_movable(data->rows);
    }
}

static void query_result_compact(void *ptr) {
    if (ptr) {
        QueryResultData *data = (QueryResultData *)ptr;
        data->graph_obj = rb_gc_location(data->graph_obj);
        data->rows = rb_gc_location(data->rows);
    }
}

static void query_result_free(void *ptr) {
    if (ptr) {
        QueryResultData *data = (QueryResultData *)ptr;
        rdx_result_set_free(data->result_set);
        xfree(data);
    }
}

static const rb_data_type_t query_result_type = {
    .wrap_struct_name = "Rubydex::Query::Result",
    .function = {
        .dmark = query_result_mark,
        .dfree = query_result_free,
        .dsize = NULL,
        .dcompact = query_result_compact,
    },
    .parent = NULL,
    .data = NULL,
    .flags = RUBY_TYPED_FREE_IMMEDIATELY,
};

static inline QueryResultData *query_result_data(VALUE self) {
    QueryResultData *data;
    TypedData_Get_Struct(self, QueryResultData, &query_result_type, data);
    return data;
}

/*
 * call-seq:
 *   run(graph) -> Rubydex::Query::Result
 *
 * Runs this parsed query against +graph+ exactly once and returns the result set. Read it as Ruby
 * objects with Rubydex::Query::Result#rows, or format it with Rubydex::Query::Result#render. Raises
 * Rubydex::QueryExecutionError when the query fails against the graph.
 */
static VALUE rdxr_query_run(VALUE self, VALUE graph_obj) {
    void *query;
    TypedData_Get_Struct(self, void *, &query_type, query);

    // Wrap first, so the result set has an owner that frees it even if a later step raises.
    QueryResultData *data;
    VALUE result = TypedData_Make_Struct(cQueryResult, QueryResultData, &query_result_type, data);
    data->result_set = NULL;
    data->graph_obj = graph_obj;
    data->rows = Qnil;

    struct CExecuteResult executed = rdx_query_execute(query, rdxi_graph_from_object(graph_obj));
    if (executed.error != NULL) {
        raise_query_error(executed.error, executed.error_kind);
    }

    data->result_set = executed.result_set;

    return result;
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

// Builds the Hash keys of one walk: one frozen UTF-8 String per column. The keys are shared by
// every row of the walk, so a wide result does not allocate a key String per cell. `rb_hash_aset`
// stores a frozen String key as it is, instead of duplicating and freezing it.
static VALUE query_row_keys(struct CRowsIter *iter) {
    size_t count = rdx_rows_iter_column_count(iter);
    const char *const *columns = rdx_rows_iter_columns(iter);
    VALUE keys = rb_ary_new_capa((long)count);

    for (size_t i = 0; i < count; i++) {
        rb_ary_push(keys, rb_str_freeze(rb_utf8_str_new_cstr(columns[i])));
    }

    return keys;
}

// Converts one row of the cursor into a Hash keyed by the shared Strings in `keys`.
static VALUE query_row_to_hash(VALUE graph_obj, VALUE keys, const struct CResultRow *row) {
    long column_count = RARRAY_LEN(keys);
    VALUE hash = rb_hash_new_capa(column_count);

    for (size_t c = 0; c < row->len && (long)c < column_count; c++) {
        rb_hash_aset(hash, RARRAY_AREF(keys, (long)c), cypher_cell_to_value(graph_obj, &row->cells[c]));
    }

    return hash;
}

// Raises when the graph no longer holds a node that the query returned. Building a string in place
// of the missing handle would silently change the column's type, so the walk stops instead.
NORETURN(static void raise_stale_result(struct CRowsIter *iter));

static void raise_stale_result(struct CRowsIter *iter) {
    VALUE error_class = rb_const_get(mRubydex, rb_intern("StaleQueryResultError"));
    const char *node = rdx_rows_iter_error(iter);

    if (node == NULL) {
        rb_raise(error_class, "the graph no longer holds a node that this query returned");
    }

    rb_raise(error_class, "the graph no longer holds `%s`, a node that this query returned", node);
}

// Body function for rb_ensure in Rubydex::Query::Result#rows — walks the cursor and collects every
// row. May raise if a node is gone, or if cell conversion (e.g. handle construction) fails; the
// ensure function frees the cursor regardless.
static VALUE query_rows_collect(VALUE args) {
    VALUE graph_obj = rb_ary_entry(args, 0);
    struct CRowsIter *iter = (struct CRowsIter *)(uintptr_t)NUM2ULL(rb_ary_entry(args, 1));

    VALUE keys = query_row_keys(iter);
    VALUE rows = rb_ary_new_capa((long)rdx_rows_iter_len(iter));

    struct CResultRow row;
    for (;;) {
        switch (rdx_rows_iter_next(iter, &row)) {
        case CRowsNextStatus_Row:
            rb_ary_push(rows, query_row_to_hash(graph_obj, keys, &row));
            break;
        case CRowsNextStatus_MissingNode:
            raise_stale_result(iter);
        default:
            return rows;
        }
    }
}

// Body function for rb_ensure in Rubydex::Query::Result#each — walks the cursor and yields one row
// at a time, so only one row exists as Ruby objects at any moment. A `break` or an exception in the
// block leaves through rb_ensure, which frees the cursor.
static VALUE query_rows_stream(VALUE args) {
    VALUE graph_obj = rb_ary_entry(args, 0);
    struct CRowsIter *iter = (struct CRowsIter *)(uintptr_t)NUM2ULL(rb_ary_entry(args, 1));

    VALUE keys = query_row_keys(iter);

    struct CResultRow row;
    for (;;) {
        switch (rdx_rows_iter_next(iter, &row)) {
        case CRowsNextStatus_Row:
            rb_yield(query_row_to_hash(graph_obj, keys, &row));
            break;
        case CRowsNextStatus_MissingNode:
            raise_stale_result(iter);
        default:
            return Qnil;
        }
    }
}

// Ensure function for rb_ensure to always free the cursor.
static VALUE query_rows_ensure(VALUE args) {
    struct CRowsIter *iter = (struct CRowsIter *)(uintptr_t)NUM2ULL(rb_ary_entry(args, 1));
    rdx_rows_iter_free(iter);
    return Qnil;
}

// Opens a cursor over the result set's rows and runs `body` with it. The cursor is always freed.
static VALUE query_with_rows(VALUE self, VALUE (*body)(VALUE)) {
    QueryResultData *data = query_result_data(self);

    struct CRowsIter *iter = rdx_result_set_rows(data->result_set, rdxi_graph_from_object(data->graph_obj));
    if (iter == NULL) {
        rb_raise(rb_eRuntimeError, "failed to create iterator");
    }

    VALUE args = rb_ary_new_from_args(2, data->graph_obj, ULL2NUM((uintptr_t)iter));
    return rb_ensure(body, args, query_rows_ensure, args);
}

/*
 * call-seq:
 *   rows -> Array[Hash[String, Object]]
 *
 * Returns the rows as Ruby objects: a frozen Array in which each row is a Hash keyed by RETURN
 * column name. Scalar cells become String/Integer/true/false/nil, lists become Arrays, maps become
 * Hashes, and node cells become Declaration / Definition / Document handles. The array is built on
 * the first call and reused afterwards.
 */
static VALUE rdxr_query_result_rows(VALUE self) {
    if (!NIL_P(query_result_data(self)->rows)) {
        return query_result_data(self)->rows;
    }

    VALUE rows = rb_ary_freeze(query_with_rows(self, query_rows_collect));
    query_result_data(self)->rows = rows;

    return rows;
}

/*
 * call-seq:
 *   columns -> Array[String]
 *
 * Returns the RETURN column names, in order. The names are known even when the query matched no
 * rows.
 */
static VALUE rdxr_query_result_columns(VALUE self) {
    QueryResultData *data = query_result_data(self);

    size_t count = rdx_result_set_column_count(data->result_set);
    VALUE columns = rb_ary_new_capa((long)count);

    for (size_t i = 0; i < count; i++) {
        rb_ary_push(columns, rdxi_owned_c_string_to_ruby(rdx_result_set_column(data->result_set, i)));
    }

    return columns;
}

/*
 * call-seq:
 *   each { |row| ... } -> self
 *   each -> Enumerator
 *
 * Yields every row as a Hash keyed by RETURN column name. Rubydex::Query::Result is Enumerable, so
 * +map+, +select+, and the rest of Enumerable work on the rows.
 *
 * Unless #rows already built the whole array, +each+ converts one row at a time and discards it
 * after the block returns. A large result therefore needs memory for one row, not for all of them,
 * and +first+ or +find+ stops converting as soon as the block breaks.
 */
static VALUE rdxr_query_result_each(VALUE self) {
    RETURN_ENUMERATOR(self, 0, 0);

    VALUE rows = query_result_data(self)->rows;

    if (NIL_P(rows)) {
        query_with_rows(self, query_rows_stream);
        return self;
    }

    long length = RARRAY_LEN(rows);

    for (long i = 0; i < length; i++) {
        rb_yield(RARRAY_AREF(rows, i));
    }

    return self;
}

/*
 * call-seq:
 *   size -> Integer
 *   length -> Integer
 *
 * Returns the number of rows, without building the row objects.
 */
static VALUE rdxr_query_result_size(VALUE self) {
    return SIZET2NUM(rdx_result_set_row_count(query_result_data(self)->result_set));
}

/*
 * call-seq:
 *   empty? -> bool
 *
 * Returns +true+ when the query matched no rows.
 */
static VALUE rdxr_query_result_empty_p(VALUE self) {
    return rdx_result_set_row_count(query_result_data(self)->result_set) == 0 ? Qtrue : Qfalse;
}

/*
 * call-seq:
 *   render(format = :table) -> String
 *
 * Returns the result set as formatted output. +format+ may be +:table+ (default) or +:json+. The
 * query is not run again. Raises ArgumentError on an unknown format.
 */
static VALUE rdxr_query_result_render(int argc, VALUE *argv, VALUE self) {
    VALUE format;
    rb_scan_args(argc, argv, "01", &format);

    QueryResultData *data = query_result_data(self);
    struct CQueryResult result = rdx_result_set_format(data->result_set, rdxi_symbol_or_string_cstr(format, "table"));

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

void rdxi_initialize_query(VALUE moduleRubydex) {
    mRubydex = moduleRubydex;

    VALUE cQuery = rb_define_class_under(mRubydex, "Query", rb_cObject);
    rb_undef_alloc_func(cQuery);
    rb_define_singleton_method(cQuery, "parse", rdxr_query_parse, 1);
    rb_define_singleton_method(cQuery, "schema", rdxr_cypher_schema, -1);
    rb_define_method(cQuery, "run", rdxr_query_run, 1);

    /*
     * The result of running a Rubydex::Query against a graph: the columns and rows produced by one
     * execution. Enumerable over its rows.
     */
    cQueryResult = rb_define_class_under(cQuery, "Result", rb_cObject);
    rb_undef_alloc_func(cQueryResult);

    // A result can only be obtained from `Query#run`; `new` would create an object with no Rust
    // data behind it.
    rb_undef_method(rb_singleton_class(cQueryResult), "new");

    rb_include_module(cQueryResult, rb_mEnumerable);
    rb_define_method(cQueryResult, "columns", rdxr_query_result_columns, 0);
    rb_define_method(cQueryResult, "rows", rdxr_query_result_rows, 0);
    rb_define_method(cQueryResult, "each", rdxr_query_result_each, 0);
    rb_define_method(cQueryResult, "size", rdxr_query_result_size, 0);
    rb_define_alias(cQueryResult, "length", "size");
    rb_define_method(cQueryResult, "empty?", rdxr_query_result_empty_p, 0);
    rb_define_method(cQueryResult, "render", rdxr_query_result_render, -1);
}
