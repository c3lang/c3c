module std::core::mem::allocator;

const DEFAULT_SIZE_PREFIX = usz.sizeof;
const DEFAULT_SIZE_PREFIX_ALIGNMENT = usz.alignof;

struct TrackingEnv
{
	String file;
	String function;
	uint line;
}


interface Allocator
{
	fn void reset(usz mark) @optional;
	fn usz mark() @optional;
	fn void*! acquire(usz size, bool clear, usz alignment, usz offset, TrackingEnv* env);
	fn void*! resize(void* ptr, usz new_size, usz alignment, usz offset, TrackingEnv* env);
	fn void release(void* ptr, bool aligned, TrackingEnv* env);
}

struct AlignedBlock
{
	usz len;
	void* start;
}

/**
 * @require bytes > 0
 * @require alignment > 0
 **/
macro void*! @aligned_alloc(#alloc_fn, usz bytes, usz alignment, usz offset)
{
	usz header = mem::aligned_offset(AlignedBlock.sizeof + offset, alignment) - offset;
	$if @typekind(#alloc_fn(bytes)) == OPTIONAL:
		void* data = #alloc_fn(header + bytes)!;
	$else
		void* data = #alloc_fn(header + bytes);
	$endif
	void* mem = mem::aligned_pointer(data + header + offset, alignment) - offset;
	assert(mem > data);
	AlignedBlock* desc = (AlignedBlock*)mem - 1;
	*desc = { bytes, data };
	return mem;
}

/**
 * @require bytes > 0
 * @require alignment > 0
 **/
macro void*! @aligned_calloc(#calloc_fn, usz bytes, usz alignment, usz offset)
{
	usz header = mem::aligned_offset(AlignedBlock.sizeof + offset, alignment) - offset;
	$if @typekind(#calloc_fn(bytes)) == OPTIONAL:
		void* data = #calloc_fn(header + bytes)!;
	$else
		void* data = #calloc_fn(header + bytes);
	$endif
	void* mem = mem::aligned_pointer(data + header + offset, alignment) - offset;
	AlignedBlock* desc = (AlignedBlock*)mem - 1;
	assert(mem > data);
	*desc = { bytes, data };
	return mem;
}

/**
 * @require bytes > 0
 * @require alignment > 0
 **/
macro void*! @aligned_realloc(#calloc_fn, #free_fn, void* old_pointer, usz bytes, usz alignment, usz offset)
{
	AlignedBlock* desc = (AlignedBlock*)old_pointer - 1;
	void* data_start = desc.start;
	void* new_data = @aligned_calloc(#calloc_fn, bytes, alignment, offset)!;
	mem::copy(new_data, old_pointer, desc.len < bytes ? desc.len : bytes, mem::DEFAULT_MEM_ALIGNMENT, mem::DEFAULT_MEM_ALIGNMENT);
	$if @typekind(#free_fn(data_start)) == OPTIONAL:
	#free_fn(data_start)!;
	$else
	#free_fn(data_start);
	$endif
	return new_data;
}

macro void! @aligned_free(#free_fn, void* old_pointer)
{
	AlignedBlock* desc = (AlignedBlock*)old_pointer - 1;
	$if @typekind(#free_fn(desc.start)) == OPTIONAL:
	#free_fn(desc.start)!;
	$else
	#free_fn(desc.start);
	$endif
}

def MemoryAllocFn = fn char[]!(usz);

fault AllocationFailure
{
	OUT_OF_MEMORY,
	CHUNK_TOO_LARGE,
}

fn usz alignment_for_allocation(usz alignment) @inline @private
{
	return alignment < mem::DEFAULT_MEM_ALIGNMENT ? alignment = mem::DEFAULT_MEM_ALIGNMENT : alignment;
}

// Allocator "functions"

macro void*! Allocator.alloc_checked(&self, usz size, TrackingEnv* env = mem::get_tracking_env())
{
	$if env::TESTING:
		char* data = self.acquire(size, false, 0, 0, env)!;
		mem::set(data, 0xAA, size, mem::DEFAULT_MEM_ALIGNMENT);
		return data;
	$else
		return self.acquire(size, false, 0, 0, env);
	$endif
}

macro void*! Allocator.calloc_checked(&self, usz size, TrackingEnv* env = mem::get_tracking_env())
{
	return self.acquire(size, true, 0, 0, env);
}
macro void*! Allocator.realloc_checked(&self, void* ptr, usz new_size, TrackingEnv* env = mem::get_tracking_env())
{
	return self.resize(ptr, new_size, 0, 0, env);
}

macro Allocator.new_array(&self, $Type, usz size, usz end_padding = 0, TrackingEnv* env = mem::get_tracking_env())
{
	return (($Type*)self.alloc_checked($Type.sizeof * size + end_padding, env))[:size]!!;
}

macro Allocator.new_array_checked(&self, $Type, usz size, usz end_padding = 0, TrackingEnv* env = mem::get_tracking_env())
{
	return (($Type*)self.alloc_checked($Type.sizeof * size + end_padding, env))[:size];
}

macro Allocator.new_zero_array(&self, $Type, usz size, usz end_padding = 0, TrackingEnv* env = mem::get_tracking_env())
{
	return (($Type*)self.calloc_checked($Type.sizeof * size + end_padding, env))[:size]!!;
}

macro Allocator.new_zero_array_checked(&self, $Type, usz size, usz end_padding = 0, TrackingEnv* env = mem::get_tracking_env())
{
	return (($Type*)self.calloc_checked($Type.sizeof * size + end_padding, env))[:size];
}

macro Allocator.new(&self, $Type, usz end_padding = 0, TrackingEnv* env = mem::get_tracking_env()) @nodiscard
{
	return ($Type*)self.alloc_checked($Type.sizeof + end_padding, env)!!;
}

macro Allocator.new_checked(&self, $Type, usz end_padding = 0, TrackingEnv* env = mem::get_tracking_env()) @nodiscard
{
	return ($Type*)self.alloc_checked($Type.sizeof + end_padding, env);
}

macro Allocator.new_clear(&self, $Type, usz end_padding = 0, TrackingEnv* env = mem::get_tracking_env()) @nodiscard
{
	return ($Type*)self.calloc_checked($Type.sizeof + end_padding, env)!!;
}

macro Allocator.new_clear_checked(&self, $Type, usz end_padding = 0, TrackingEnv* env = mem::get_tracking_env()) @nodiscard
{
	return ($Type*)self.calloc_checked($Type.sizeof + end_padding, env);
}

macro Allocator.clone(&self, value, TrackingEnv* env = mem::get_tracking_env())
{
	var x = self.alloc($typeof(value), env);
	*x = value;
	return x;
}

macro void* Allocator.alloc(&self, usz size, TrackingEnv* env = mem::get_tracking_env()) @nodiscard
{
	return self.alloc_checked(size, env)!!;
}
macro void* Allocator.calloc(&self, usz size, TrackingEnv* env = mem::get_tracking_env()) @nodiscard
{
	return self.acquire(size, true, 0, 0, env)!!;
}
macro void* Allocator.realloc(&self, void* ptr, usz new_size, TrackingEnv* env = mem::get_tracking_env()) @nodiscard
{
	return self.resize(ptr, new_size, 0, 0, env)!!;
}

macro void*! Allocator.alloc_aligned(&self, usz size, usz alignment, usz offset = 0, TrackingEnv* env = mem::get_tracking_env())
{
	$if env::TESTING:
		char* data = self.acquire(size, false, alignment, offset, env)!;
		mem::set(data, 0xAA, size, mem::DEFAULT_MEM_ALIGNMENT);
		return data;
	$else
		return self.acquire(size, false, alignment, offset, env);
	$endif
}
macro void*! Allocator.calloc_aligned(&self, usz size, usz alignment, usz offset = 0, TrackingEnv* env = mem::get_tracking_env())
{
	return self.acquire(size, true, alignment, offset, env);
}
macro void*! Allocator.realloc_aligned(&self, void* ptr, usz new_size, usz alignment = 0, usz offset = 0, TrackingEnv* env = mem::get_tracking_env())
{
	return self.resize(ptr, new_size, alignment, offset, env);
}

macro void Allocator.free(&self, void* ptr, TrackingEnv* env = mem::get_tracking_env())
{
	$if env::TESTING:
		if (ptr) ((char*)ptr)[0] = 0xBA;
	$endif
	self.release(ptr, false, env);
}
macro void Allocator.free_aligned(&self, void* ptr, TrackingEnv* env = mem::get_tracking_env())
{
	$if env::TESTING:
		if (ptr) ((char*)ptr)[0] = 0xBA;
	$endif
	self.release(ptr, true, env);
}


