#ifndef RUBYDEX_CONFIG_H
#define RUBYDEX_CONFIG_H

#include "ruby.h"

extern const rb_data_type_t config_type;

static inline void *rdxi_config_from_object(VALUE config_obj) {
    void *config;
    TypedData_Get_Struct(config_obj, void *, &config_type, config);
    return config;
}

void rdxi_initialize_config(VALUE mRubydex);

#endif // RUBYDEX_CONFIG_H
