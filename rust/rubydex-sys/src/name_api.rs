use libc::c_char;
use rubydex::model::{graph::Graph, ids::NameId, name::Name};

use crate::utils;

#[repr(C)]
pub struct NameRefData {
    str: *const c_char,
    nesting: *mut NameRefData,
    parent_scope: *mut NameRefData,
}

/// Converts a `NameRefData` C struct into a `NameId` reference and registers all of the necessary data in the graph to
/// perform resolution
pub fn convert_name_data_to_ref(graph: &mut Graph, data: &NameRefData) -> Option<NameId> {
    let nesting = if data.nesting.is_null() {
        None
    } else {
        convert_name_data_to_ref(graph, unsafe { &*data.nesting })
    };

    let parent_scope = if data.parent_scope.is_null() {
        None
    } else {
        convert_name_data_to_ref(graph, unsafe { &*data.parent_scope })
    };

    let Ok(str) = (unsafe { utils::convert_char_ptr_to_string(data.str) }) else {
        return None;
    };

    let str_id = graph.intern_string(str);
    Some(graph.add_name(Name::new(str_id, parent_scope, nesting)))
}
