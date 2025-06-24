pub mod internal;

use rb_sys::*;
use std::ffi::CString;
use std::os::raw::{c_char, c_void};
use std::ptr;

use crate::internal::{Entry, Repository};

static mut REPOSITORY_DATA_TYPE: rb_data_type_t = rb_data_type_t {
    wrap_struct_name: b"Repository\0".as_ptr() as *const c_char,
    function: rb_data_type_struct__bindgen_ty_1 {
        dmark: None,
        dfree: Some(repository_free),
        dsize: None,
        dcompact: None,
        reserved: [ptr::null_mut(); 1],
    },
    parent: ptr::null(),
    data: ptr::null_mut(),
    flags: 0,
};

unsafe extern "C" fn repository_free(ptr: *mut c_void) {
    if !ptr.is_null() {
        let _ = Box::from_raw(ptr as *mut Repository);
    }
}

#[no_mangle]
pub extern "C" fn Init_index() {
    unsafe {
        let index_module = rb_define_module(b"Index\0".as_ptr() as *const c_char);

        let repository_class =
            rb_define_class_under(index_module, b"Repository\0".as_ptr() as *const c_char, rb_cObject);

        rb_define_alloc_func(repository_class, Some(repository_alloc));
        rb_define_method(
            repository_class,
            b"add_entry\0".as_ptr() as *const c_char,
            Some(std::mem::transmute::<
                unsafe extern "C" fn(VALUE, VALUE, VALUE) -> VALUE,
                unsafe extern "C" fn() -> VALUE,
            >(repository_add_entry)),
            2,
        );
        rb_define_method(
            repository_class,
            b"get_entry\0".as_ptr() as *const c_char,
            Some(std::mem::transmute::<
                unsafe extern "C" fn(VALUE, VALUE) -> VALUE,
                unsafe extern "C" fn() -> VALUE,
            >(repository_get_entry)),
            1,
        );

        let entry_class = rb_define_class_under(index_module, b"Entry\0".as_ptr() as *const c_char, rb_cObject);

        rb_define_method(
            entry_class,
            b"name\0".as_ptr() as *const c_char,
            Some(std::mem::transmute::<
                unsafe extern "C" fn(VALUE) -> VALUE,
                unsafe extern "C" fn() -> VALUE,
            >(entry_name)),
            0,
        );
        rb_define_method(
            entry_class,
            b"value\0".as_ptr() as *const c_char,
            Some(std::mem::transmute::<
                unsafe extern "C" fn(VALUE) -> VALUE,
                unsafe extern "C" fn() -> VALUE,
            >(entry_value)),
            0,
        );
    }
}

unsafe extern "C" fn repository_alloc(klass: VALUE) -> VALUE {
    let repo = Box::new(Repository::new());
    let repo_ptr = Box::into_raw(repo);
    rb_data_typed_object_wrap(
        klass,
        repo_ptr as *mut c_void,
        &raw const REPOSITORY_DATA_TYPE as *const rb_data_type_t,
    )
}

unsafe extern "C" fn repository_add_entry(self_: VALUE, name: VALUE, value: VALUE) -> VALUE {
    let repo_ptr = rb_check_typeddata(self_, &raw const REPOSITORY_DATA_TYPE as *const rb_data_type_t);
    let repo = &mut *(repo_ptr as *mut Repository);

    let mut name_copy = name;
    let name_cstr = rb_string_value_cstr(&mut name_copy as *mut VALUE);
    let name_str = std::ffi::CStr::from_ptr(name_cstr).to_str().unwrap();

    let mut value_copy = value;
    let value_cstr = rb_string_value_cstr(&mut value_copy as *mut VALUE);
    let value_str = std::ffi::CStr::from_ptr(value_cstr).to_str().unwrap();

    let entry = Entry::new(name_str.to_string(), value_str.to_string());
    repo.add_entry(entry);
    Qnil as VALUE
}

unsafe extern "C" fn repository_get_entry(self_: VALUE, name: VALUE) -> VALUE {
    let repo_ptr = rb_check_typeddata(self_, &raw const REPOSITORY_DATA_TYPE as *const rb_data_type_t);
    let repo = &*(repo_ptr as *mut Repository);

    let mut name_copy = name;
    let name_cstr = rb_string_value_cstr(&mut name_copy as *mut VALUE);
    let name_str = std::ffi::CStr::from_ptr(name_cstr).to_str().unwrap();

    match repo.get_entry(name_str) {
        Some(entry) => {
            let entry_class = rb_const_get(
                rb_const_get(rb_cObject, rb_intern(b"Index\0".as_ptr() as *const c_char)),
                rb_intern(b"Entry\0".as_ptr() as *const c_char),
            );
            let entry_obj = rb_obj_alloc(entry_class);

            let name_sym = rb_intern(b"@name\0".as_ptr() as *const c_char);
            let value_sym = rb_intern(b"@value\0".as_ptr() as *const c_char);

            let name_str = CString::new(entry.name.clone()).unwrap();
            let value_str = CString::new(entry.value.clone()).unwrap();

            rb_ivar_set(entry_obj, name_sym, rb_str_new_cstr(name_str.as_ptr()));
            rb_ivar_set(entry_obj, value_sym, rb_str_new_cstr(value_str.as_ptr()));

            entry_obj
        }
        None => Qnil as VALUE,
    }
}

unsafe extern "C" fn entry_name(self_: VALUE) -> VALUE {
    rb_ivar_get(self_, rb_intern(b"@name\0".as_ptr() as *const c_char))
}

unsafe extern "C" fn entry_value(self_: VALUE) -> VALUE {
    rb_ivar_get(self_, rb_intern(b"@value\0".as_ptr() as *const c_char))
}
