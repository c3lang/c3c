module std::core::mem::allocators;

const NullAllocator NULL_ALLOCATOR = {};
typedef NullAllocator (Allocator) = uptr;

fn void*? NullAllocator.acquire(&self, sz bytes, AllocInitType init_type, sz alignment) @dynamic
{
	return mem::OUT_OF_MEMORY~;
}

fn void*? NullAllocator.resize(&self, void* old_ptr, sz new_bytes, sz alignment) @dynamic
{
	return mem::OUT_OF_MEMORY~;
}

fn void NullAllocator.release(&self, void* old_ptr, bool aligned) @dynamic
{
}
