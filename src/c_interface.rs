use std::ffi::{c_char, CString};

use crate::internal::{Entry, Repository, Point, Message};

pub type CRepository = std::ffi::c_void;
pub type CEntry = std::ffi::c_void;
pub type CPoint = std::ffi::c_void;
pub type CMessage = std::ffi::c_void;

#[unsafe(no_mangle)]
pub extern "C" fn get_repository() -> *mut CRepository {
    let repository = Repository::new();
    Box::into_raw(Box::new(repository)) as *mut CRepository
}

/// This will likely be removed
///
/// # Safety
///
/// This function is unsafe
#[unsafe(no_mangle)]
pub unsafe extern "C" fn repository_get_entry(repository: *const CRepository, name: *const c_char) -> *mut CEntry {
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
            Some(entry) => Box::into_raw(Box::new(entry.clone())) as *mut CEntry,
            None => std::ptr::null_mut(),
        }
    }
}

/// This will likely be removed
///
/// # Safety
///
/// This function is unsafe
#[unsafe(no_mangle)]
pub unsafe extern "C" fn repository_add_entry(repository: *mut CRepository, name: *const c_char, value: *const c_char) {
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

#[unsafe(no_mangle)]
pub extern "C" fn entry_get_name(entry: *const CEntry) -> *const c_char {
    if entry.is_null() {
        return std::ptr::null();
    }
    unsafe {
        let entry_ref = &*(entry as *const Entry);
        match CString::new(entry_ref.name.clone()) {
            Ok(c_string) => c_string.into_raw(),
            Err(_) => std::ptr::null(),
        }
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn entry_get_value(entry: *const CEntry) -> *const c_char {
    if entry.is_null() {
        return std::ptr::null();
    }
    unsafe {
        let entry_ref = &*(entry as *const Entry);
        match CString::new(entry_ref.value.clone()) {
            Ok(c_string) => c_string.into_raw(),
            Err(_) => std::ptr::null(),
        }
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn delloc_entry(entry: *mut CEntry) {
    if !entry.is_null() {
        unsafe {
            let _ = Box::from_raw(entry as *mut Entry);
        }
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn dealloc_repository(repository: *mut CRepository) {
    if !repository.is_null() {
        unsafe {
            let _ = Box::from_raw(repository as *mut Repository);
        }
    }
}

// Simple integer functions for boundary performance testing
#[unsafe(no_mangle)]
pub extern "C" fn get_constant_number() -> u32 {
    42
}

#[unsafe(no_mangle)]
pub extern "C" fn increment_number(input: u32) -> u32 {
    input + 1
}

// Point functions for object creation benchmarking
#[unsafe(no_mangle)]
pub extern "C" fn create_point(x: u32, y: u32) -> *mut CPoint {
    let point = Point::new(x, y);
    Box::into_raw(Box::new(point)) as *mut CPoint
}



#[unsafe(no_mangle)]
pub extern "C" fn dealloc_point(point: *mut CPoint) {
    if !point.is_null() {
        unsafe {
            let _ = Box::from_raw(point as *mut Point);
        }
    }
}

// Message functions for string object creation benchmarking
#[unsafe(no_mangle)]
pub unsafe extern "C" fn create_message(content: *const c_char) -> *mut CMessage {
    if content.is_null() {
        return std::ptr::null_mut();
    }
    unsafe {
        let c_str = std::ffi::CStr::from_ptr(content);
        match c_str.to_str() {
            Ok(s) => {
                let message = Message::new(s.to_string());
                Box::into_raw(Box::new(message)) as *mut CMessage
            }
            Err(_) => std::ptr::null_mut(),
        }
    }
}



#[unsafe(no_mangle)]
pub extern "C" fn dealloc_message(message: *mut CMessage) {
    if !message.is_null() {
        unsafe {
            let _ = Box::from_raw(message as *mut Message);
        }
    }
}


