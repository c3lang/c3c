module std::core::mem::allocators;

const NullAllocator NULL_ALLOCATOR = {};
typedef NullAllocator (Allocator) = uptr;

fn void*? NullAllocator.acquire(&self, isz bytes, AllocInitType init_type, isz alignment) @dynamic
{
	return mem::OUT_OF_MEMORY~;
}

fn void*? NullAllocator.resize(&self, void* old_ptr, isz new_bytes, isz alignment) @dynamic
{
	return mem::OUT_OF_MEMORY~;
}

fn void NullAllocator.release(&self, void* old_ptr, bool aligned) @dynamic
{
}
