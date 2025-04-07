module std::core::mem::allocator;
import std::math;

const DEFAULT_SIZE_PREFIX = usz.sizeof;
const DEFAULT_SIZE_PREFIX_ALIGNMENT = usz.alignof;

struct TrackingEnv
{
	String file;
	String function;
	uint line;
}

enum AllocInitType
{
	NO_ZERO,
	ZERO
}

interface Allocator
{
	<*
	 @require !alignment || math::is_power_of_2(alignment)
	 @require alignment <= mem::MAX_MEMORY_ALIGNMENT : `alignment too big`
	 @require size > 0
	 @return? mem::INVALID_ALLOC_SIZE, mem::OUT_OF_MEMORY
	*>
	fn void*? acquire(usz size, AllocInitType init_type, usz alignment = 0);
	<*
	 @require !alignment || math::is_power_of_2(alignment)
	 @require alignment <= mem::MAX_MEMORY_ALIGNMENT : `alignment too big`
	 @require ptr != null
	 @require new_size > 0
	 @return? mem::INVALID_ALLOC_SIZE, mem::OUT_OF_MEMORY
	*>
	fn void*? resize(void* ptr, usz new_size, usz alignment = 0);
	<*
	 @require ptr != null
	*>
	fn void release(void* ptr, bool aligned);
}

alias MemoryAllocFn = fn char[]?(usz);



fn usz alignment_for_allocation(usz alignment) @inline @private
{
	return alignment < mem::DEFAULT_MEM_ALIGNMENT ? mem::DEFAULT_MEM_ALIGNMENT : alignment;
}

macro void* malloc(Allocator allocator, usz size) @nodiscard
{
	return malloc_try(allocator, size)!!;
}

macro void*? malloc_try(Allocator allocator, usz size) @nodiscard
{
	if (!size) return null;
	$if env::TESTING:
		char* data = allocator.acquire(size, NO_ZERO)!;
		mem::set(data, 0xAA, size, mem::DEFAULT_MEM_ALIGNMENT);
		return data;
	$else
		return allocator.acquire(size, NO_ZERO);
	$endif
}

macro void* calloc(Allocator allocator, usz size) @nodiscard
{
	return calloc_try(allocator, size)!!;
}

macro void*? calloc_try(Allocator allocator, usz size) @nodiscard
{
	if (!size) return null;
	return allocator.acquire(size, ZERO);
}

macro void* realloc(Allocator allocator, void* ptr, usz new_size) @nodiscard
{
	return realloc_try(allocator, ptr, new_size)!!;
}

macro void*? realloc_try(Allocator allocator, void* ptr, usz new_size) @nodiscard
{
	if (!new_size)
	{
		free(allocator, ptr);
		return null;
	}
	if (!ptr) return allocator.acquire(new_size, NO_ZERO);
	return allocator.resize(ptr, new_size);
}

macro void free(Allocator allocator, void* ptr)
{
	if (!ptr) return;
	$if env::TESTING:
		((char*)ptr)[0] = 0xBA;
	$endif
	allocator.release(ptr, false);
}

macro void*? malloc_aligned(Allocator allocator, usz size, usz alignment) @nodiscard
{
	if (!size) return null;
	$if env::TESTING:
		char* data = allocator.acquire(size, NO_ZERO, alignment)!;
		mem::set(data, 0xAA, size, mem::DEFAULT_MEM_ALIGNMENT);
		return data;
	$else
		return allocator.acquire(size, NO_ZERO, alignment);
	$endif
}

macro void*? calloc_aligned(Allocator allocator, usz size, usz alignment) @nodiscard
{
	if (!size) return null;
	return allocator.acquire(size, ZERO, alignment);
}

macro void*? realloc_aligned(Allocator allocator, void* ptr, usz new_size, usz alignment) @nodiscard
{
	if (!new_size)
	{
		free_aligned(allocator, ptr);
		return null;
	}
	if (!ptr)
	{
		return malloc_aligned(allocator, new_size, alignment);
	}
	return allocator.resize(ptr, new_size, alignment);
}

macro void free_aligned(Allocator allocator, void* ptr)
{
	if (!ptr) return;
	$if env::TESTING:
	((char*)ptr)[0] = 0xBA;
	$endif
	allocator.release(ptr, aligned: true);
}

<*
 @require $Type.alignof <= mem::DEFAULT_MEM_ALIGNMENT : "Types with alignment exceeding the default must use 'new_aligned' instead"
 @require $vacount < 2 : "Too many arguments."
 @require $vacount == 0 ||| $assignable($vaexpr[0], $Type) : "The second argument must be an initializer for the type"
*>
macro new(Allocator allocator, $Type, ...) @nodiscard
{
	$if $vacount == 0:
		return ($Type*)calloc(allocator, $Type.sizeof);
	$else
		$Type* val = malloc(allocator, $Type.sizeof);
		*val = $vaexpr[0];
		return val;
	$endif
}

<*
 @require $Type.alignof <= mem::DEFAULT_MEM_ALIGNMENT : "Types with alignment exceeding the default must use 'new_aligned' instead"
 @require $vacount < 2 : "Too many arguments."
 @require $vacount == 0 ||| $assignable($vaexpr[0], $Type) : "The second argument must be an initializer for the type"
*>
macro new_try(Allocator allocator, $Type, ...) @nodiscard
{
	$if $vacount == 0:
		return ($Type*)calloc_try(allocator, $Type.sizeof);
	$else
		$Type* val = malloc_try(allocator, $Type.sizeof)!;
		*val = $vaexpr[0];
		return val;
	$endif
}

<*
 Allocate using an aligned allocation. This is necessary for types with a default memory alignment
 exceeding DEFAULT_MEM_ALIGNMENT. IMPORTANT! It must be freed using free_aligned.
 @require $vacount < 2 : "Too many arguments."
 @require $vacount == 0 ||| $assignable($vaexpr[0], $Type) : "The second argument must be an initializer for the type"
*>
macro new_aligned(Allocator allocator, $Type, ...) @nodiscard
{
	$if $vacount == 0:
		return ($Type*)calloc_aligned(allocator, $Type.sizeof, $Type.alignof);
	$else
		$Type* val = malloc_aligned(allocator, $Type.sizeof, $Type.alignof)!;
		*val = $vaexpr[0];
		return val;
	$endif
}

<*
 @require $Type.alignof <= mem::DEFAULT_MEM_ALIGNMENT
*>
macro new_with_padding(Allocator allocator, $Type, usz padding) @nodiscard
{
	return ($Type*)calloc_try(allocator, $Type.sizeof + padding);
}

<*
 @require $Type.alignof <= mem::DEFAULT_MEM_ALIGNMENT : "Types with alignment exceeding the default must use 'alloc_aligned' instead"
*>
macro alloc(Allocator allocator, $Type) @nodiscard
{
	return ($Type*)malloc(allocator, $Type.sizeof);
}

<*
 @require $Type.alignof <= mem::DEFAULT_MEM_ALIGNMENT : "Types with alignment exceeding the default must use 'alloc_aligned' instead"
*>
macro alloc_try(Allocator allocator, $Type) @nodiscard
{
	return ($Type*)malloc_try(allocator, $Type.sizeof);
}

<*
 Allocate using an aligned allocation. This is necessary for types with a default memory alignment
 exceeding DEFAULT_MEM_ALIGNMENT. IMPORTANT! It must be freed using free_aligned.
*>
macro alloc_aligned(Allocator allocator, $Type) @nodiscard
{
	return ($Type*)malloc_aligned(allocator, $Type.sizeof, $Type.alignof);
}

<*
 @require $Type.alignof <= mem::DEFAULT_MEM_ALIGNMENT
*>
macro alloc_with_padding(Allocator allocator, $Type, usz padding) @nodiscard
{
	return ($Type*)malloc_try(allocator, $Type.sizeof + padding);
}

<*
 @require $Type.alignof <= mem::DEFAULT_MEM_ALIGNMENT : "Types with alignment exceeding the default must use 'new_array_aligned' instead"
*>
macro new_array(Allocator allocator, $Type, usz elements) @nodiscard
{
	return new_array_try(allocator, $Type, elements)!!;
}

<*
 @require $Type.alignof <= mem::DEFAULT_MEM_ALIGNMENT : "Types with alignment exceeding the default must use 'new_array_aligned' instead"
*>
macro new_array_try(Allocator allocator, $Type, usz elements) @nodiscard
{
	return (($Type*)calloc_try(allocator, $Type.sizeof * elements))[:elements];
}

<*
 Allocate using an aligned allocation. This is necessary for types with a default memory alignment
 exceeding DEFAULT_MEM_ALIGNMENT. IMPORTANT! It must be freed using free_aligned.
*>
macro new_array_aligned(Allocator allocator, $Type, usz elements) @nodiscard
{
	return (($Type*)calloc_aligned(allocator, $Type.sizeof * elements, $Type.alignof))[:elements]!!;
}

<*
 @require $Type.alignof <= mem::DEFAULT_MEM_ALIGNMENT : "Types with alignment exceeding the default must use 'alloc_array_aligned' instead"
*>
macro alloc_array(Allocator allocator, $Type, usz elements) @nodiscard
{
	return alloc_array_try(allocator, $Type, elements)!!;
}

<*
 Allocate using an aligned allocation. This is necessary for types with a default memory alignment
 exceeding DEFAULT_MEM_ALIGNMENT. IMPORTANT! It must be freed using free_aligned.
*>
macro alloc_array_aligned(Allocator allocator, $Type, usz elements) @nodiscard
{
	return (($Type*)malloc_aligned(allocator, $Type.sizeof * elements, $Type.alignof))[:elements]!!;
}

<*
 @require $Type.alignof <= mem::DEFAULT_MEM_ALIGNMENT : "Types with alignment exceeding the default must use 'alloc_array_aligned' instead"
*>
macro alloc_array_try(Allocator allocator, $Type, usz elements) @nodiscard
{
	return (($Type*)malloc_try(allocator, $Type.sizeof * elements))[:elements];
}

macro clone(Allocator allocator, value) @nodiscard
{
	return new(allocator, $typeof(value), value);
}

fn any clone_any(Allocator allocator, any value) @nodiscard
{
	usz size = value.type.sizeof;
	void* data = malloc(allocator, size);
	mem::copy(data, value.ptr, size);
	return any_make(data, value.type);
}


<*
 @require bytes > 0
 @require alignment > 0
 @require bytes <= isz.max
*>
macro void*? @aligned_alloc(#alloc_fn, usz bytes, usz alignment)
{
	if (alignment < void*.alignof) alignment = void*.alignof;
	usz header = AlignedBlock.sizeof + alignment;
	usz alignsize = bytes + header;
	$if @typekind(#alloc_fn(bytes)) == OPTIONAL:
		void* data = #alloc_fn(alignsize)!;
	$else
		void* data = #alloc_fn(alignsize);
	$endif
	void* mem = mem::aligned_pointer(data + AlignedBlock.sizeof, alignment);
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

macro void? @aligned_free(#free_fn, void* old_pointer)
{
	AlignedBlock* desc = (AlignedBlock*)old_pointer - 1;
	$if @typekind(#free_fn(desc.start)) == OPTIONAL:
	#free_fn(desc.start)!;
	$else
	#free_fn(desc.start);
	$endif
}

<*
 @require bytes > 0
 @require alignment > 0
*>
macro void*? @aligned_realloc(#calloc_fn, #free_fn, void* old_pointer, usz bytes, usz alignment)
{
	AlignedBlock* desc = (AlignedBlock*)old_pointer - 1;
	void* data_start = desc.start;
	void* new_data = @aligned_alloc(#calloc_fn, bytes, alignment)!;
	mem::copy(new_data, old_pointer, desc.len < bytes ? desc.len : bytes, 1, 1);
	$if @typekind(#free_fn(data_start)) == OPTIONAL:
	#free_fn(data_start)!;
	$else
	#free_fn(data_start);
	$endif
	return new_data;
}


// All allocators
alias mem @builtin = thread_allocator ;
tlocal Allocator thread_allocator @private = base_allocator();
Allocator temp_base_allocator @private = base_allocator();

typedef PoolState = TempAllocator*;

const LazyTempAllocator LAZY_TEMP @private = {};
tlocal Allocator current_temp = &LAZY_TEMP;
tlocal TempAllocator* top_temp;
tlocal bool auto_create_temp = false;

usz temp_allocator_min_size = temp_allocator_default_min_size();
usz temp_allocator_buffer_size = temp_allocator_default_buffer_size();
usz temp_allocator_new_mult = 4;

fn PoolState push_pool()
{
	Allocator old = top_temp ? current_temp : create_temp_allocator_on_demand();
	current_temp = ((TempAllocator*)old).derive_allocator(temp_allocator_min_size, temp_allocator_buffer_size, temp_allocator_new_mult)!!;
	return (PoolState)old.ptr;
}

fn void pop_pool(PoolState old)
{
	TempAllocator* temp = (TempAllocator*)old;
	current_temp = temp;
	temp.reset();
}

macro Allocator base_allocator() @private
{
	$if env::LIBC:
		return &allocator::LIBC_ALLOCATOR;
	$else
		return &allocator::NULL_ALLOCATOR;
	$endif
}

macro usz temp_allocator_size() @local
{
	$switch env::MEMORY_ENV:
		$case NORMAL: return 256 * 1024;
		$case SMALL: return 1024 * 32;
		$case TINY: return 1024 * 4;
		$case NONE: return 0;
	$endswitch
}

macro usz temp_allocator_default_min_size() @local
{
	$switch env::MEMORY_ENV:
		$case NORMAL: return 16 * 1024;
		$case SMALL: return 1024 * 2;
		$case TINY: return 256;
		$case NONE: return 256;
	$endswitch
}

macro usz temp_allocator_default_buffer_size() @local
{
	$switch env::MEMORY_ENV:
		$case NORMAL: return 1024;
		$case SMALL: return 128;
		$case TINY: return 64;
		$case NONE: return 64;
	$endswitch
}

macro Allocator heap() => thread_allocator;

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
	return create_temp_allocator(temp_base_allocator, temp_allocator_size());
}
<*
 @require !top_temp : "This should never be called when temp already exists"
*>
fn Allocator create_temp_allocator(Allocator allocator, usz size, usz buffer = temp_allocator_default_buffer_size()) @private
{
	return current_temp = top_temp = allocator::new_temp_allocator(allocator, size)!!;
}

macro Allocator temp()
{
	return current_temp;
}

alias tmem @builtin = current_temp;

fn void allow_implicit_temp_allocator_on_load_thread() @init(1) @local @if(env::LIBC || env::WASM_NOLIBC)
{
	auto_create_temp = true;
}

fn void destroy_temp_allocators_after_exit() @finalizer(65535) @local @if(env::LIBC)
{
	destroy_temp_allocators();
}

<*
 Call this to destroy any memory used by the temp allocators. This will invalidate all temp memory.
*>
fn void destroy_temp_allocators()
{
	if (!top_temp) return;
	top_temp.free();
	top_temp = null;
	current_temp = &LAZY_TEMP;
}

import libc;
typedef LazyTempAllocator (Allocator) @private = uptr;

fn void*? LazyTempAllocator.acquire(&self, usz bytes, AllocInitType init_type, usz alignment) @dynamic
{
	if (!top_temp) create_temp_allocator_on_demand();
	return top_temp.acquire(bytes, init_type, alignment);
}

fn void*? LazyTempAllocator.resize(&self, void* old_ptr, usz new_bytes, usz alignment) @dynamic
{
	if (!top_temp) create_temp_allocator_on_demand();
	return top_temp.resize(old_ptr, new_bytes, alignment);
}

fn void LazyTempAllocator.release(&self, void* old_ptr, bool aligned) @dynamic
{
}

const NullAllocator NULL_ALLOCATOR = {};
typedef NullAllocator (Allocator) = uptr;

fn void*? NullAllocator.acquire(&self, usz bytes, AllocInitType init_type, usz alignment) @dynamic
{
	return mem::OUT_OF_MEMORY?;
}

fn void*? NullAllocator.resize(&self, void* old_ptr, usz new_bytes, usz alignment) @dynamic
{
	return mem::OUT_OF_MEMORY?;
}

fn void NullAllocator.release(&self, void* old_ptr, bool aligned) @dynamic
{
}

