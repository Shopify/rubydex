#include "document.h"
#include "graph.h"
#include "handle.h"
#include "rustbindings.h"

VALUE cDocument;

// Document#uri -> String
static VALUE sr_document_uri(VALUE self) {
    HandleData *data;
    TypedData_Get_Struct(self, HandleData, &handle_type, data);

    void *graph;
    TypedData_Get_Struct(data->graph_obj, void *, &graph_type, graph);
    const char *uri = sat_document_uri(graph, data->id);

    if (uri == NULL) {
        return Qnil;
    }

    VALUE str = rb_utf8_str_new_cstr(uri);
    free_c_string(uri);

    return str;
}

void initialize_document(VALUE mSaturn) {
    cDocument = rb_define_class_under(mSaturn, "Document", rb_cObject);

    rb_define_alloc_func(cDocument, sr_handle_alloc);
    rb_define_method(cDocument, "initialize", sr_handle_initialize, 2);
    rb_define_method(cDocument, "uri", sr_document_uri, 0);

    rb_funcall(rb_singleton_class(cDocument), rb_intern("private"), 1, ID2SYM(rb_intern("new")));
}
