module std::core::mem::allocators @private;

tlocal Allocator thread_allocator = base_allocator();
macro Allocator base_allocator()
{
	$if env::LIBC:
		return &LIBC_ALLOCATOR;
	$else
		return &NULL_ALLOCATOR;
	$endif
}

