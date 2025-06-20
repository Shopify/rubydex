use std::ffi::{c_char, CString};

use crate::internal::{Entry, Repository};

pub type CRepository = std::ffi::c_void;

// C-compatible struct for direct field access
#[repr(C)]
pub struct CEntry {
    pub name: *mut c_char,
    pub value: *mut c_char,
}

impl CEntry {
    fn from_entry(entry: &Entry) -> Option<Self> {
        let name_cstring = CString::new(entry.name.clone()).ok()?;
        let value_cstring = CString::new(entry.value.clone()).ok()?;
        
        Some(Self {
            name: name_cstring.into_raw(),
            value: value_cstring.into_raw(),
        })
    }
    
    fn free(&mut self) {
        if !self.name.is_null() {
            unsafe {
                let _ = CString::from_raw(self.name);
            }
            self.name = std::ptr::null_mut();
        }
        if !self.value.is_null() {
            unsafe {
                let _ = CString::from_raw(self.value);
            }
            self.value = std::ptr::null_mut();
        }
    }
}

#[no_mangle]
pub extern "C" fn get_repository() -> *mut CRepository {
    let repository = Repository::new();
    Box::into_raw(Box::new(repository)) as *mut CRepository
}

#[no_mangle]
pub extern "C" fn repository_get_entry(repository: *const CRepository, name: *const c_char) -> *mut CEntry {
    if repository.is_null() || name.is_null() {
        return std::ptr::null_mut();
    }
    unsafe {
        let repository_ref = &*(repository as *const Repository);
        let c_str = std::ffi::CStr::from_ptr(name);
        let name_str = match c_str.to_str() {
            Ok(s) => s,
            Err(_) => return std::ptr::null_mut(),
        };
        
        match repository_ref.get_entry(name_str) {
            Some(entry) => {
                match CEntry::from_entry(entry) {
                    Some(c_entry) => Box::into_raw(Box::new(c_entry)),
                    None => std::ptr::null_mut(),
                }
            },
            None => std::ptr::null_mut(),
        }
    }
}

#[no_mangle]
pub extern "C" fn repository_add_entry(repository: *mut CRepository, name: *const c_char, value: *const c_char) {
    if repository.is_null() || name.is_null() || value.is_null() {
        return;
    }
    unsafe {
        let repository_ref = &mut *(repository as *mut Repository);
        let name_cstr = std::ffi::CStr::from_ptr(name);
        let value_cstr = std::ffi::CStr::from_ptr(value);
        
        if let (Ok(name_str), Ok(value_str)) = (name_cstr.to_str(), value_cstr.to_str()) {
            let entry = Entry::new(name_str.to_string(), value_str.to_string());
            repository_ref.add_entry(entry);
        }
    }
}

#[no_mangle]
pub extern "C" fn dealloc_entry(entry: *mut CEntry) {
    if !entry.is_null() {
        unsafe {
            let mut entry_box = Box::from_raw(entry);
            entry_box.free();
        }
    }
}

#[no_mangle]
pub extern "C" fn dealloc_repository(repository: *mut CRepository) {
    if !repository.is_null() {
        unsafe {
            let _ = Box::from_raw(repository as *mut Repository);
        }
    }
}
