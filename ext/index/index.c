#include "ruby.h"
#include "rustbindings.h"
#include "repository.h"

VALUE mIndex;

void Init_index(void) {
    RB_EXT_RACTOR_SAFE(true);
    mIndex = rb_define_module("Index");
    initialize_repository(mIndex);
}
