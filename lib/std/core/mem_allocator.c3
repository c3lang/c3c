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
	fn void*! acquire(usz size, bool clear, usz alignment, usz offset);
	fn void*! resize(void* ptr, usz new_size, usz alignment, usz offset);
	fn void release(void* ptr, bool aligned);
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

macro void* malloc(Allocator* allocator, usz size) @nodiscard
{
	return malloc_try(allocator, size)!!;
}

macro void*! malloc_try(Allocator* allocator, usz size) @nodiscard
{
	$if env::TESTING:
		char* data = allocator.acquire(size, false, 0, 0)!;
		mem::set(data, 0xAA, size, mem::DEFAULT_MEM_ALIGNMENT);
		return data;
	$else
		return allocator.acquire(size, false, 0, 0);
	$endif
}

macro void* calloc(Allocator* allocator, usz size) @nodiscard
{
	return calloc_try(allocator, size)!!;
}

macro void*! calloc_try(Allocator* allocator, usz size) @nodiscard
{
	return allocator.acquire(size, true, 0, 0);
}

macro void* realloc(Allocator* allocator, void* ptr, usz new_size) @nodiscard
{
	return realloc_try(allocator, ptr, new_size)!!;
}
macro void*! realloc_try(Allocator* allocator, void* ptr, usz new_size) @nodiscard
{
	return allocator.resize(ptr, new_size, 0, 0);
}

macro void free(Allocator* allocator, void* ptr)
{
	$if env::TESTING:
		if (ptr) ((char*)ptr)[0] = 0xBA;
	$endif
	allocator.release(ptr, false);
}

macro void*! malloc_aligned(Allocator* allocator, usz size, usz alignment, usz offset = 0) @nodiscard
{
	$if env::TESTING:
		char* data = allocator.acquire(size, false, alignment, offset)!;
		mem::set(data, 0xAA, size, mem::DEFAULT_MEM_ALIGNMENT);
		return data;
	$else
		return allocator.acquire(size, false, alignment, offset);
	$endif
}

macro void*! calloc_aligned(Allocator* allocator, usz size, usz alignment, usz offset = 0) @nodiscard
{
	return allocator.acquire(size, true, alignment, offset);
}

macro void*! realloc_aligned(Allocator* allocator, void* ptr, usz new_size, usz alignment, usz offset = 0) @nodiscard
{
	return allocator.resize(ptr, new_size, alignment, offset);
}

macro void free_aligned(Allocator* allocator, void* ptr)
{
	$if env::TESTING:
		if (ptr) ((char*)ptr)[0] = 0xBA;
	$endif
	allocator.release(ptr, true);
}

/**
 * @require $vacount < 2 : "Too many arguments."
 * @require $or($vacount == 1, $assignable($vaexpr(0), $Type)) : "The second argument must be an initializer for the type"
 **/
macro new(Allocator* allocator, $Type, ...) @nodiscard
{
	$if $vacount == 0:
		return ($Type*)calloc(allocator, $Type.sizeof);
	$else
		$Type* val = malloc(allocator, $Type.sizeof);
		*val = $vaexpr(0);
		return val;
	$endif
}

/**
 * @require $vacount < 2 : "Too many arguments."
 * @require $or($vacount == 0, $assignable($vaexpr(0), $Type)) : "The second argument must be an initializer for the type"
 **/
macro new_try(Allocator* allocator, $Type, ...) @nodiscard
{
	$if $vacount == 0:
		return ($Type*)calloc_try(allocator, $Type.sizeof);
	$else
		$Type* val = malloc_try(allocator, $Type.sizeof)!;
		*val = $vaexpr(0);
		return val;
	$endif
}

macro new_with_padding(Allocator* allocator, $Type, usz padding) @nodiscard
{
	return ($Type*)calloc_try(allocator, $Type.sizeof + padding);
}

macro alloc(Allocator* allocator, $Type) @nodiscard
{
	return ($Type*)malloc(allocator, $Type.sizeof);
}

macro alloc_try(Allocator* allocator, $Type) @nodiscard
{
	return ($Type*)malloc_try(allocator, $Type.sizeof);
}

macro alloc_with_padding(Allocator* allocator, $Type, usz padding) @nodiscard
{
	return ($Type*)malloc_try(allocator, $Type.sizeof + padding);
}

macro new_array(Allocator* allocator, $Type, usz elements) @nodiscard
{
	return new_array_try(allocator, $Type, elements)!!;
}

macro new_array_try(Allocator* allocator, $Type, usz elements) @nodiscard
{
	return (($Type*)calloc_try(allocator, $Type.sizeof * elements))[:elements];
}

macro alloc_array(Allocator* allocator, $Type, usz elements) @nodiscard
{
	return alloc_array_try(allocator, $Type, elements)!!;
}

macro alloc_array_try(Allocator* allocator, $Type, usz elements) @nodiscard
{
	return (($Type*)malloc_try(allocator, $Type.sizeof * elements))[:elements];
}

macro clone(Allocator* allocator, value) @nodiscard
{
	return new($typeof(value), value);
}


// Allocator "functions"

macro void*! Allocator.alloc_checked(&self, usz size) @deprecated("Use allocator::malloc_try")
{
	$if env::TESTING:
		char* data = self.acquire(size, false, 0, 0)!;
		mem::set(data, 0xAA, size, mem::DEFAULT_MEM_ALIGNMENT);
		return data;
	$else
		return self.acquire(size, false, 0, 0);
	$endif
}

macro void*! Allocator.calloc_checked(&self, usz size) @deprecated("Use allocator::calloc_try")
{
	return self.acquire(size, true, 0, 0);
}

macro void*! Allocator.realloc_checked(&self, void* ptr, usz new_size) @deprecated("Use allocator::realloc_try")
{
	return self.resize(ptr, new_size, 0, 0);
}

macro Allocator.new_array(&self, $Type, usz size, usz end_padding = 0) @deprecated("Use allocator::alloc_array")
{
	return (($Type*)self.alloc_checked($Type.sizeof * size + end_padding))[:size]!!;
}

macro Allocator.new_array_checked(&self, $Type, usz size, usz end_padding = 0) @deprecated("Use allocator::alloc_array_try")
{
	return (($Type*)self.alloc_checked($Type.sizeof * size + end_padding))[:size];
}

macro Allocator.new_zero_array(&self, $Type, usz size, usz end_padding = 0) @deprecated("Use allocator::new_array")
{
	return (($Type*)self.calloc_checked($Type.sizeof * size + end_padding))[:size]!!;
}

macro Allocator.new_zero_array_checked(&self, $Type, usz size, usz end_padding = 0) @deprecated("Use allocator::new_array_try")
{
	return (($Type*)self.calloc_checked($Type.sizeof * size + end_padding))[:size];
}

macro Allocator.new(&self, $Type, usz end_padding = 0) @nodiscard @deprecated("Use allocator::alloc")
{
	return ($Type*)self.alloc_checked($Type.sizeof + end_padding)!!;
}

macro Allocator.new_checked(&self, $Type, usz end_padding = 0) @nodiscard @deprecated("Use allocator::alloc_try")
{
	return ($Type*)self.alloc_checked($Type.sizeof + end_padding);
}

macro Allocator.new_clear(&self, $Type, usz end_padding = 0) @nodiscard @deprecated("Use allocator::new")
{
	return ($Type*)self.calloc_checked($Type.sizeof + end_padding)!!;
}

macro Allocator.new_clear_checked(&self, $Type, usz end_padding = 0) @nodiscard  @deprecated("Use allocator::new_try")
{
	return ($Type*)self.calloc_checked($Type.sizeof + end_padding);
}
macro Allocator.clone(&self, value) @deprecated("Use allocator::clone")
{
	var x = self.alloc($typeof(value));
	*x = value;
	return x;
}

macro void* Allocator.alloc(&self, usz size) @nodiscard @deprecated("Use allocator::malloc")
{
	return self.alloc_checked(size)!!;
}
macro void* Allocator.calloc(&self, usz size) @nodiscard @deprecated("Use allocator::calloc")
{
	return self.acquire(size, true, 0, 0)!!;
}
macro void* Allocator.realloc(&self, void* ptr, usz new_size) @nodiscard @deprecated("Use allocator::realloc")
{
	return self.resize(ptr, new_size, 0, 0)!!;
}

macro void*! Allocator.alloc_aligned(&self, usz size, usz alignment, usz offset = 0) @deprecated("Use allocator::alloc_aligned")
{
	$if env::TESTING:
		char* data = self.acquire(size, false, alignment, offset)!;
		mem::set(data, 0xAA, size, mem::DEFAULT_MEM_ALIGNMENT);
		return data;
	$else
		return self.acquire(size, false, alignment, offset);
	$endif
}
macro void*! Allocator.calloc_aligned(&self, usz size, usz alignment, usz offset = 0) @deprecated("Use allocator::calloc_aligned")
{
	return self.acquire(size, true, alignment, offset);
}
macro void*! Allocator.realloc_aligned(&self, void* ptr, usz new_size, usz alignment = 0, usz offset = 0)  @deprecated("Use allocator::realloc_aligned")
{
	return self.resize(ptr, new_size, alignment, offset);
}

macro void Allocator.free(&self, void* ptr) @deprecated("Use allocator::free")
{
	$if env::TESTING:
		if (ptr) ((char*)ptr)[0] = 0xBA;
	$endif
	self.release(ptr, false);
}
macro void Allocator.free_aligned(&self, void* ptr) @deprecated("Use allocator::free_aligned")
{
	$if env::TESTING:
		if (ptr) ((char*)ptr)[0] = 0xBA;
	$endif
	self.release(ptr, true);
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

struct AlignedBlock
{
	usz len;
	void* start;
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


// All allocators
tlocal Allocator* thread_allocator @private = &allocator::LIBC_ALLOCATOR;
tlocal TempAllocator* thread_temp_allocator @private = null;
tlocal TempAllocator*[2] temp_allocator_pair @private;
Allocator* temp_base_allocator @private = &allocator::LIBC_ALLOCATOR;

macro TempAllocator* create_default_sized_temp_allocator(Allocator* allocator) @local
{
	$switch (env::MEMORY_ENV)
	$case NORMAL:
		return new_temp_allocator(1024 * 256, allocator)!!;
	$case SMALL:
		return new_temp_allocator(1024 * 16, allocator)!!;
	$case TINY:
		return new_temp_allocator(1024 * 2, allocator)!!;
	$case NONE:
		unreachable("Temp allocator must explicitly created when memory-env is set to 'none'.");
	$endswitch
}

macro Allocator* heap() => thread_allocator;

macro TempAllocator* temp()
{
	if (!thread_temp_allocator)
	{
		init_default_temp_allocators();
	}
	return thread_temp_allocator;
}

fn void init_default_temp_allocators() @private
{
	temp_allocator_pair[0] = create_default_sized_temp_allocator(temp_base_allocator);
	temp_allocator_pair[1] = create_default_sized_temp_allocator(temp_base_allocator);
	thread_temp_allocator = temp_allocator_pair[0];
}

fn TempAllocator *temp_allocator_next() @private
{
	if (!thread_temp_allocator)
	{
		init_default_temp_allocators();
		return thread_temp_allocator;
	}
	usz index = thread_temp_allocator == temp_allocator_pair[0] ? 1 : 0;
	return thread_temp_allocator = temp_allocator_pair[index];
}