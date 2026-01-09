#ifndef SATURN_LOCATION_H
#define SATURN_LOCATION_H

#include "ruby.h"
#include "rustbindings.h"

extern VALUE cLocation;

void initialize_location(VALUE mSaturn);

// Helper to build a Ruby Saturn::Location from a C Location pointer.
// Does not take ownership; caller remains responsible for freeing the C Location on the Rust side.
VALUE build_location_value(Location *loc);

#endif // SATURN_LOCATION_H
