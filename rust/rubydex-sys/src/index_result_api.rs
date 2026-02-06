use libc::c_void;
use rubydex::indexing::IndexResult;

pub type IndexResultPointer = *mut c_void;

#[unsafe(no_mangle)]
pub extern "C" fn rdx_index_result_free(pointer: IndexResultPointer) {
    if pointer.is_null() {
        return;
    }

    unsafe {
        let _ = Box::from_raw(pointer.cast::<IndexResult>());
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn rdx_index_result_definition_ids_len(pointer: IndexResultPointer) -> usize {
    if pointer.is_null() {
        return 0;
    }

    let result = unsafe { &*pointer.cast::<IndexResult>() };
    result.definition_ids.len()
}

#[unsafe(no_mangle)]
pub extern "C" fn rdx_index_result_reference_ids_len(pointer: IndexResultPointer) -> usize {
    if pointer.is_null() {
        return 0;
    }

    let result = unsafe { &*pointer.cast::<IndexResult>() };
    result.reference_ids.len()
}
