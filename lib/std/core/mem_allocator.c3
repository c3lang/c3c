module std::core::mem::allocator;

const DEFAULT_SIZE_PREFIX = usz.sizeof;
const DEFAULT_SIZE_PREFIX_ALIGNMENT = usz.alignof;

interface Allocator
{
	fn void reset(usz mark) @optional;
	fn usz mark() @optional;
	fn void*! acquire(usz size, bool clear = false, usz alignment = 0, usz offset = 0);
	fn void*! resize(void* ptr, usz new_size, usz alignment = 0, usz offset = 0);
	fn void release(void* ptr, bool aligned = false);
}

const LibcAllocator LIBC_ALLOCATOR = {};

def MemoryAllocFn = fn char[]!(usz);

// Allocator "functions"

macro void*! Allocator.alloc_checked(&self, usz size)
{
	$if env::TESTING:
		char* data = self.acquire(size, false, 0, 0)!;
		mem::set(data, 0xAA, size, mem::DEFAULT_MEM_ALIGNMENT);
		return data;
	$else
		return self.acquire(size, false, 0, 0);
	$endif
}

macro void*! Allocator.calloc_checked(&self, usz size) => self.acquire(size, true, 0, 0);
macro void*! Allocator.realloc_checked(&self, void* ptr, usz new_size) => self.resize(ptr, new_size, 0, 0);

macro Allocator.new_array(&self, $Type, usz size, usz end_padding = 0)
{
	return (($Type*)self.alloc_checked($Type.sizeof * size + end_padding))[:size]!!;
}

macro Allocator.new_array_checked(&self, $Type, usz size, usz end_padding = 0)
{
	return (($Type*)self.alloc_checked($Type.sizeof * size + end_padding))[:size];
}

macro Allocator.new_zero_array(&self, $Type, usz size, usz end_padding = 0)
{
	return (($Type*)self.calloc_checked($Type.sizeof * size + end_padding))[:size]!!;
}

macro Allocator.new_zero_array_checked(&self, $Type, usz size, usz end_padding = 0)
{
	return (($Type*)self.calloc_checked($Type.sizeof * size + end_padding))[:size];
}

macro Allocator.new(&self, $Type, usz end_padding = 0) @nodiscard
{
	return ($Type*)self.alloc_checked($Type.sizeof + end_padding)!!;
}

macro Allocator.new_checked(&self, $Type, usz end_padding = 0) @nodiscard
{
	return ($Type*)self.alloc_checked($Type.sizeof + end_padding);
}

macro Allocator.new_clear(&self, $Type, usz end_padding = 0) @nodiscard
{
	return ($Type*)self.calloc_checked($Type.sizeof + end_padding)!!;
}

macro Allocator.new_clear_checked(&self, $Type, usz end_padding = 0) @nodiscard
{
	return ($Type*)self.calloc_checked($Type.sizeof + end_padding);
}

macro Allocator.clone(&self, value)
{
	var x = self.alloc($typeof(value));
	*x = value;
	return x;
}

macro void* Allocator.alloc(&self, usz size) @nodiscard => self.alloc_checked(size)!!;
macro void* Allocator.calloc(&self, usz size) @nodiscard => self.acquire(size, true, 0, 0)!!;
macro void* Allocator.realloc(&self, void* ptr, usz new_size) @nodiscard => self.resize(ptr, new_size, 0, 0)!!;
macro void*! Allocator.alloc_aligned(&self, usz size, usz alignment, usz offset = 0)
{
	$if env::TESTING:
		char* data = self.acquire(size, false, alignment, offset)!;
		mem::set(data, 0xAA, size, mem::DEFAULT_MEM_ALIGNMENT);
		return data;
	$else
		return self.acquire(size, false, alignment, offset);
	$endif
}
macro void*! Allocator.calloc_aligned(&self, usz size, usz alignment, usz offset = 0) => self.acquire(size, true, alignment, offset);
macro void*! Allocator.realloc_aligned(&self, void* ptr, usz new_size, usz alignment = 0, usz offset = 0) => self.resize(ptr, new_size, alignment, offset);
macro void Allocator.free(&self, void* ptr)
{
	$if env::TESTING:
		if (ptr) ((char*)ptr)[0] = 0xBA;
	$endif
	self.release(ptr, false);
}
macro void Allocator.free_aligned(&self, void* ptr)
{
	$if env::TESTING:
		if (ptr) ((char*)ptr)[0] = 0xBA;
	$endif
	self.release(ptr, true);
}

fault AllocationFailure
{
	OUT_OF_MEMORY,
	CHUNK_TOO_LARGE,
}

fn usz alignment_for_allocation(usz alignment) @inline @private
{
	return alignment < mem::DEFAULT_MEM_ALIGNMENT ? alignment = mem::DEFAULT_MEM_ALIGNMENT : alignment;
}



