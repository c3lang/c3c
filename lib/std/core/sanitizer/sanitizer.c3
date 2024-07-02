module std::core::sanitizer @if (env::ADDRESS_SANITIZER || env::MEMORY_SANITIZER || env::THREAD_SANITIZER) @private;

// TODO ...

extern fn void __sanitizer_set_death_callback(VoidFn callback) @extern("__sanitizer_set_death_callback");

module std::core::sanitizer;

fn void set_death_callback(VoidFn callback)
{
	$if env::ADDRESS_SANITIZER:
		__sanitizer_set_death_callback(callback);
	$endif
}
