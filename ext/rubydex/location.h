#ifndef RUBYDEX_LOCATION_H
#define RUBYDEX_LOCATION_H

#include "ruby.h"
#include "rustbindings.h"

extern VALUE cLocation;

void rdxi_initialize_location(VALUE mRubydex);

// Helper to build a Ruby Rubydex::Location from a C Location pointer.
// Does not take ownership; caller remains responsible for freeing the C Location on the Rust side.
VALUE rdxi_build_location_value(Location *loc);

#endif // RUBYDEX_LOCATION_H
