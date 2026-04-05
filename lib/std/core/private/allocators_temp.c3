module std::core::mem::allocators @private;

Allocator temp_base_allocator @private = base_allocator();
const LazyTempAllocator LAZY_TEMP @private = {};

sz temp_allocator_min_size = temp_allocator_default_min_size();
sz temp_allocator_reserve_size = temp_allocator_default_reserve_size();
sz temp_allocator_realloc_size = temp_allocator_default_min_size() * 4;

// Default temp allocator sizes
macro sz temp_allocator_size() @local
{
	$switch env::MEMORY_ENV:
		$case NORMAL: return 256 * 1024;
		$case SMALL: return 1024 * 32;
		$case TINY: return 1024 * 4;
		$case NONE: return 0;
	$endswitch
}

macro sz temp_allocator_default_min_size() @local
{
	$switch env::MEMORY_ENV:
		$case NORMAL: return 16 * 1024;
		$case SMALL: return 1024 * 2;
		$case TINY: return 256;
		$case NONE: return 256;
	$endswitch
}

macro sz temp_allocator_default_reserve_size() @local
{
	$switch env::MEMORY_ENV:
		$case NORMAL: return 1024;
		$case SMALL: return 128;
		$case TINY: return 64;
		$case NONE: return 64;
	$endswitch
}

<*
 @require !top_temp : "This should never be called when temp already exists"
*>
fn Allocator create_temp_allocator_on_demand() @private
{
	if (!auto_create_temp)
	{
	    auto_create_temp = true;
	    abort("Use '@pool_init()' to enable the temp allocator on a new thread. A temp allocator is only implicitly created on the main thread.");
	}
	return create_temp_allocator(temp_base_allocator, temp_allocator_size(), temp_allocator_reserve_size, temp_allocator_min_size, temp_allocator_realloc_size);
}

fn void allow_implicit_temp_allocator_on_load_thread() @init(1) @local @if(env::LIBC || env::FREESTANDING_WASM)
{
	auto_create_temp = true;
}

fn void destroy_temp_allocators_after_exit() @finalizer(65535) @local @if(env::LIBC)
{
	destroy_temp_allocators();
}