module std::core::sanitizer::asan @if(env::ADDRESS_SANITIZER) @private;

extern fn void __asan_poison_memory_region(void *addr, usz size) @extern("__asan_poison_memory_region");
extern fn void __asan_unpoison_memory_region(void *addr, usz size) @extern("__asan_unpoison_memory_region");
extern fn CInt __asan_address_is_poisoned(void *addr) @extern("__asan_address_is_poisoned");
extern fn void *__asan_region_is_poisoned(void *beg, usz size) @extern("__asan_region_is_poisoned");
extern fn void __asan_describe_address(void *addr) @extern("__asan_describe_address");
extern fn CInt __asan_report_present() @extern("__asan_report_present");
extern fn void *__asan_get_report_pc() @extern("__asan_get_report_pc");
extern fn void *__asan_get_report_bp() @extern("__asan_get_report_bp");
extern fn void *__asan_get_report_sp() @extern("__asan_get_report_sp");
extern fn void *__asan_get_report_address() @extern("__asan_get_report_address");
extern fn CInt __asan_get_report_access_type() @extern("__asan_get_report_access_type");
extern fn usz __asan_get_report_access_size() @extern("__asan_get_report_access_size");
extern fn ZString __asan_get_report_description() @extern("__asan_get_report_description");
extern fn ZString __asan_locate_address(void *addr, char *name, usz name_size,
																				void **region_address, usz *region_size) @extern("__asan_locate_address");
extern fn usz __asan_get_alloc_stack(void *addr, void **trace, usz size,
																		 CInt *thread_id) @extern("__asan_get_alloc_stack");
extern fn usz __asan_get_free_stack(void *addr, void **trace, usz size,
																		CInt *thread_id) @extern("__asan_get_free_stack");
extern fn void __asan_get_shadow_mapping(usz *shadow_scale, usz *shadow_offset) @extern("__asan_get_shadow_mapping");
extern fn void __asan_set_error_report_callback(ErrorCallback callback) @extern("__asan_set_error_report_callback");
extern fn void __asan_print_accumulated_stats() @extern("__asan_print_accumulated_stats");
extern fn void *__asan_get_current_fake_stack() @extern("__asan_get_current_fake_stack");
extern fn void *__asan_addr_is_in_fake_stack(void *fake_stack, void *addr, void **beg, void **end) @extern("__asan_addr_is_in_fake_stack");
extern fn void __asan_handle_no_return() @extern("__asan_handle_no_return");
extern fn int __asan_update_allocation_context(void *addr) @extern("__asan_update_allocation_context");

VoidFn error_callback;
fn void __asan_on_error() @if(env::ADDRESS_SANITIZER) @local @export("__asan_on_error")
{
	if (error_callback) error_callback();
}

module std::core::sanitizer::asan;

def ErrorCallback = fn void (ZString);

fn void set_error_callback(VoidFn callback)
{
	$if env::ADDRESS_SANITIZER:
		error_callback = callback;
	$endif
}

fn void set_error_report_callback(ErrorCallback callback)
{
	$if env::ADDRESS_SANITIZER:
		__asan_set_error_report_callback(callback);
	$endif
}

macro poison_memory_region(void *addr, usz size)
{
	$if env::ADDRESS_SANITIZER:
		__asan_poison_memory_region(addr, size);
	$endif
}

macro unpoison_memory_region(void *addr, usz size)
{
	$if env::ADDRESS_SANITIZER:
		__asan_unpoison_memory_region(addr, size);
	$endif
}

/**
 * @require $typeof(value).kindof == POINTER
 **/
macro poison_value(value) => poison_memory_region((void*)value, $sizeof(*value));

/**
 * @require $typeof(value).kindof == POINTER
 **/
macro unpoison_value(value) => unpoison_memory_region((void*)value, $sizeof(*value));

// Add this to your code to suppress leak detection or set other default options
// fn ZString __asan_default_options() @export("__asan_default_options") @if(env::ADDRESS_SANITIZER)
// {
// 	return "detect_leaks=0";
// }

// Add this to break on error
// asan::set_error_report_callback(fn (ZString err)
// {
//   breakpoint();
// });
