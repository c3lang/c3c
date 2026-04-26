module std::core::mem::alloc;
import std::math;

struct TrackingEnv
{
	String file;
	String function;
	int line;
}

enum AllocInitType
{
	NO_ZERO,
	ZERO
}

interface Allocator
{
	<*
	 Acquire memory from the allocator, with the given alignment and initialization type.

	 @require !alignment || math::is_power_of_2(alignment)
	 @require alignment <= mem::MAX_MEMORY_ALIGNMENT : `alignment too big`
	 @require size > 0 : "The size must be 1 or more"
	 @return? mem::INVALID_ALLOC_SIZE, mem::OUT_OF_MEMORY
	*>
	fn void*? acquire(sz size, AllocInitType init_type, sz alignment = 0);

	<*
	 Resize acquired memory from the allocator, with the given new size and alignment.

	 @require !alignment || math::is_power_of_2(alignment)
	 @require alignment <= mem::MAX_MEMORY_ALIGNMENT : `alignment too big`
	 @require ptr != null
	 @require new_size > 0
	 @return? mem::INVALID_ALLOC_SIZE, mem::OUT_OF_MEMORY
	*>
	fn void*? resize(void* ptr, sz new_size, sz alignment = 0);

	<*
	 Release memory acquired using `acquire` or `resize`.

	 @require ptr != null : "Empty pointers should never be released"
	*>
	fn void release(void* ptr, bool aligned);
}

alias MemoryAllocFn = fn char[]?(sz);


<*
 @require size >= 0 : "Invalid memory size"
*>
macro void* malloc(Allocator allocator, sz size) @nodiscard
{
	return malloc_try(allocator, size)!!;
}

<*
 @require size >= 0 : "Invalid memory size"
*>
macro void*? malloc_try(Allocator allocator, sz size) @nodiscard
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

<*
 @require size >= 0 : "Invalid memory size"
*>
macro void* calloc(Allocator allocator, sz size) @nodiscard
{
	return calloc_try(allocator, size)!!;
}

<*
 @require size >= 0 : "Invalid memory size"
*>
macro void*? calloc_try(Allocator allocator, sz size) @nodiscard
{
	if (!size) return null;
	return allocator.acquire(size, ZERO);
}

<*
 @require new_size >= 0 : "Invalid memory size"
*>
macro void* realloc(Allocator allocator, void* ptr, sz new_size) @nodiscard
{
	return realloc_try(allocator, ptr, new_size)!!;
}

<*
 @require new_size >= 0 : "Invalid memory size"
*>
macro void*? realloc_try(Allocator allocator, void* ptr, sz new_size) @nodiscard
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

<*
 @require size >= 0 : "Invalid memory size"
 @require alignment > 0 : "Invalid alignment"
*>
macro void*? malloc_aligned(Allocator allocator, sz size, sz alignment) @nodiscard
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

<*
 @require size >= 0 : "Invalid memory size"
 @require alignment > 0 : "Invalid alignment"
*>
macro void*? calloc_aligned(Allocator allocator, sz size, sz alignment) @nodiscard
{
	if (!size) return null;
	return allocator.acquire(size, ZERO, alignment);
}

<*
 @require new_size >= 0 : "Invalid memory size"
 @require alignment > 0 : "Invalid alignment"
*>
macro void*? realloc_aligned(Allocator allocator, void* ptr, sz new_size, sz alignment) @nodiscard
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
 @require $Type::alignment <= mem::DEFAULT_MEM_ALIGNMENT : "Types with alignment exceeding the default must use 'new_aligned' instead"
 @require !$defined(#init) ||| $defined($Type a = #init) : "#init must be an initializer for the type"
*>
macro new(Allocator allocator, $Type, #init = ...) @nodiscard @safemacro
{
	$if $defined(#init):
		$Type* val = malloc(allocator, $Type::size);
		*val = #init;
		return val;
	$else
		return ($Type*)calloc(allocator, $Type::size);
	$endif
}

<*
 @require $Type::alignment <= mem::DEFAULT_MEM_ALIGNMENT : "Types with alignment exceeding the default must use 'new_aligned' instead"
 @require !$defined(#init) ||| $defined($Type a = #init) : "#init must be an initializer for the type"
*>
macro new_try(Allocator allocator, $Type, #init = ...) @nodiscard @safemacro
{
	$if $defined(#init):
		$Type* val = malloc_try(allocator, $Type::size)!;
		*val = #init;
		return val;
	$else
		return ($Type*)calloc_try(allocator, $Type::size);
	$endif
}

<*
 Allocate using an aligned allocation. This is necessary for types with a default memory alignment
 exceeding DEFAULT_MEM_ALIGNMENT. IMPORTANT! It must be freed using free_aligned.

 @require !$defined(#init) ||| $defined($Type a = #init) : "#init must be an initializer for the type"
*>
macro new_aligned(Allocator allocator, $Type, #init = ...) @nodiscard @safemacro
{
	$if $defined(#init):
		$Type* val = malloc_aligned(allocator, $Type::size, $Type::alignment)!;
		*val = #init;
		return val;
	$else
		return ($Type*)calloc_aligned(allocator, $Type::size, $Type::alignment);
	$endif
}

<*
 @require $Type::alignment <= mem::DEFAULT_MEM_ALIGNMENT
 @require padding >= 0 : "Invalid padding size"
*>
macro new_with_padding(Allocator allocator, $Type, sz padding) @nodiscard
{
	return ($Type*)calloc_try(allocator, $Type::size + padding);
}

<*
 @require $Type::alignment <= mem::DEFAULT_MEM_ALIGNMENT : "Types with alignment exceeding the default must use 'alloc_aligned' instead"
*>
macro alloc(Allocator allocator, $Type) @nodiscard
{
	return ($Type*)malloc(allocator, $Type::size);
}

<*
 @require $Type::alignment <= mem::DEFAULT_MEM_ALIGNMENT : "Types with alignment exceeding the default must use 'alloc_aligned' instead"
*>
macro alloc_try(Allocator allocator, $Type) @nodiscard
{
	return ($Type*)malloc_try(allocator, $Type::size);
}

<*
 Allocate using an aligned allocation. This is necessary for types with a default memory alignment
 exceeding DEFAULT_MEM_ALIGNMENT. IMPORTANT! It must be freed using free_aligned.
*>
macro alloc_aligned(Allocator allocator, $Type) @nodiscard
{
	return ($Type*)malloc_aligned(allocator, $Type::size, $Type::alignment);
}

<*
 @require $Type::alignment <= mem::DEFAULT_MEM_ALIGNMENT
 @require padding >= 0 : "Invalid padding size"
*>
macro alloc_with_padding(Allocator allocator, $Type, sz padding) @nodiscard
{
	return ($Type*)malloc_try(allocator, $Type::size + padding);
}

<*
 @require $Type::alignment <= mem::DEFAULT_MEM_ALIGNMENT : "Types with alignment exceeding the default must use 'new_array_aligned' instead"
 @require elements >= 0 : "Elements may not be negative"
 @require !@catch($Type::size.overflow_mul(elements)) : "Resulting array size would overflow sz::max"
*>
macro new_array(Allocator allocator, $Type, sz elements) @nodiscard
{
	return new_array_try(allocator, $Type, elements)!!;
}

<*
 @require $Type::alignment <= mem::DEFAULT_MEM_ALIGNMENT : "Types with alignment exceeding the default must use 'new_array_aligned' instead"
 @require elements >= 0 : "Elements may not be negative"
 @require !@catch($Type::size.overflow_mul(elements)) : "Resulting array size would overflow sz::max"
*>
macro new_array_try(Allocator allocator, $Type, sz elements) @nodiscard
{
	return (($Type*)calloc_try(allocator, $Type::size * elements))[:elements];
}

<*
 Allocate using an aligned allocation. This is necessary for types with a default memory alignment
 exceeding DEFAULT_MEM_ALIGNMENT. IMPORTANT! It must be freed using free_aligned.

 @require elements >= 0 : "Elements may not be negative"
 @require !@catch($Type::size.overflow_mul(elements)) : "Resulting array size would overflow sz::max"
*>
macro new_array_aligned(Allocator allocator, $Type, sz elements) @nodiscard
{
	return (($Type*)calloc_aligned(allocator, $Type::size * elements, $Type::alignment))[:elements]!!;
}

<*
 @require $Type::alignment <= mem::DEFAULT_MEM_ALIGNMENT : "Types with alignment exceeding the default must use 'alloc_array_aligned' instead"
 @require elements >= 0 : "Elements may not be negative"
 @require !@catch($Type::size.overflow_mul(elements)) : "Resulting array size would overflow sz::max"
*>
macro alloc_array(Allocator allocator, $Type, sz elements) @nodiscard
{
	return alloc_array_try(allocator, $Type, elements)!!;
}

<*
 Allocate using an aligned allocation. This is necessary for types with a default memory alignment
 exceeding DEFAULT_MEM_ALIGNMENT. IMPORTANT! It must be freed using free_aligned.

 @require elements >= 0 : "Elements may not be negative"
 @require !@catch($Type::size.overflow_mul(elements)) : "Resulting array size would overflow sz::max"
*>
macro alloc_array_aligned(Allocator allocator, $Type, sz elements) @nodiscard
{
	return (($Type*)malloc_aligned(allocator, $Type::size * elements, $Type::alignment))[:elements]!!;
}

<*
 @require $Type::alignment <= mem::DEFAULT_MEM_ALIGNMENT : "Types with alignment exceeding the default must use 'alloc_array_aligned' instead"
 @require elements >= 0 : "Elements may not be negative"
 @require !@catch($Type::size.overflow_mul(elements)) : "Resulting array size would overflow sz::max"
*>
macro alloc_array_try(Allocator allocator, $Type, sz elements) @nodiscard
{
	return (($Type*)malloc_try(allocator, $Type::size * elements))[:elements];
}

<*
 @require $Type::alignment <= mem::DEFAULT_MEM_ALIGNMENT : "Types with alignment exceeding the default must use 'alloc_array_aligned' instead"
 @require elements >= 0 : "Elements may not be negative"
 @require !@catch($Type::size.overflow_mul(elements)) : "Resulting array size would overflow sz::max"
*>
macro realloc_array(Allocator allocator, void* ptr, $Type, sz elements) @nodiscard
{
	return realloc_array_try(allocator, ptr, $Type, elements)!!;
}

<*
 Allocate using an aligned allocation. This is necessary for types with a default memory alignment
 exceeding DEFAULT_MEM_ALIGNMENT. IMPORTANT! It must be freed using free_aligned.

 @require elements >= 0 : "Elements may not be negative"
 @require !@catch($Type::size.overflow_mul(elements)) : "Resulting array size would overflow sz::max"
*>
macro realloc_array_aligned(Allocator allocator, void* ptr, $Type, sz elements) @nodiscard
{
	return (($Type*)realloc_aligned(allocator, ptr, $Type::size * elements, $Type::alignment))[:elements]!!;
}

<*
 @require $Type::alignment <= mem::DEFAULT_MEM_ALIGNMENT : "Types with alignment exceeding the default must use 'alloc_array_aligned' instead"
 @require elements >= 0 : "Elements may not be negative"
 @require !@catch($Type::size.overflow_mul(elements)) : "Resulting array size would overflow sz::max"
*>
macro realloc_array_try(Allocator allocator, void* ptr, $Type, sz elements) @nodiscard
{
	return (($Type*)realloc_try(allocator, ptr, $Type::size * elements))[:elements];
}

<*
 Clone a value.

 @param [&inout] allocator : "The allocator to use to clone"
 @param value : "The value to clone"
 @return "A pointer to the cloned value"
 @require @alignof(value) <= mem::DEFAULT_MEM_ALIGNMENT : "Types with alignment exceeding the default must use 'clone_aligned' instead"
*>
macro clone(Allocator allocator, value) @nodiscard
{
	return new(allocator, $typeof(value), value);
}

<*
 @param [&inout] allocator : "The allocator used to clone"
 @param slice : "The slice to clone"
 @return "A pointer to the cloned slice"

 @require @kindof(slice) == SLICE || @kindof(slice) == ARRAY
*>
macro clone_slice(Allocator allocator, slice) @nodiscard
{
	if (!lengthof(slice)) return {};

	var $Type = $typeof(slice[0]);

	$Type[] new_arr = new_array(allocator, $Type, slice.len);
	mem::copy(new_arr.ptr, &slice[0], slice.len * $Type::size);

	return new_arr;
}

<*
 Clone overaligned values. Must be released using free_aligned.

 @param [&inout] allocator : "The allocator to use to clone"
 @param value : "The value to clone"
 @return "A pointer to the cloned value"
*>
macro clone_aligned(Allocator allocator, value) @nodiscard
{
	return new_aligned(allocator, $typeof(value), value)!!;
}

fn any clone_any(Allocator allocator, any value) @nodiscard
{
	sz size = value.type.size;
	void* data = malloc(allocator, size);
	mem::copy(data, value.ptr, size);
	return any_make(data, value.type);
}






