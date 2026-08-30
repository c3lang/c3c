module std::core::mem::alloc;

<*
 Source-location information recorded for a tracked allocation.
*>
struct TrackingEnv
{
	String file;
	String function;
	int line;
}

<*
 Whether newly acquired memory should be zero-initialized.
*>
enum AllocInitType
{
	NO_ZERO,
	ZERO
}

interface Allocator
{
	<*
	 Acquire memory from the allocator, with the given alignment and initialization type.

	 @param size : "The number of bytes to allocate"
	 @param init_type : "Whether the memory should be zero-initialized"
	 @param alignment : "The required alignment, or 0 to use the default alignment"
	 @return? mem::INVALID_ALLOC_SIZE, mem::OUT_OF_MEMORY

     @require [alignment] !alignment || mem::is_valid_alignment(alignment) : "Alignment must be a valid power of 2"
	 @require [size] size > 0 : "The size must be 1 or more"
	*>
	fn void*? acquire(sz size, AllocInitType init_type, sz alignment = 0);

	<*
	 Resize acquired memory from the allocator, with the given new size and alignment.

	 @param [&drop] ptr : "The pointer to resize"
	 @param new_size : "The new size in bytes"
	 @param alignment : "The required alignment, or 0 to use the default alignment"
	 @return? mem::INVALID_ALLOC_SIZE, mem::OUT_OF_MEMORY

     @require [alignment] !alignment || mem::is_valid_alignment(alignment) : "Alignment must be a valid power of 2"
	 @require [new_size] new_size > 0
	*>
	fn void*? resize(void* ptr, sz new_size, sz alignment = 0);

	<*
	 Release memory acquired using `acquire` or `resize`.

	 @param [&drop] ptr : "The pointer to release"
	 @param aligned : "Whether the pointer was acquired with an alignment requirement"
	*>
	fn void release(void* ptr, bool aligned);
}

<*
 Function signature for a simple byte-buffer allocator callback.
*>
alias MemoryAllocFn = fn char[]?(sz);


<*
 Allocate memory of the given size using the allocator, panicking on failure.

 @param [&inout] allocator : "The allocator to use"
 @param size : "The number of bytes to allocate"
 @return "A pointer to uninitialized memory of the requested size (0xAA-filled in test mode), or null if size is 0"

 @require [size] size >= 0 : "Invalid memory size"
*>
macro void* malloc(Allocator allocator, sz size) @nodiscard
{
	return malloc_try(allocator, size)!!;
}

<*
 Allocate memory of the given size using the allocator.

 @param [&inout] allocator : "The allocator to use"
 @param size : "The number of bytes to allocate"
 @return "A pointer to uninitialized memory of the requested size (0xAA-filled in test mode), or null if size is 0"
 @return? mem::INVALID_ALLOC_SIZE, mem::OUT_OF_MEMORY

 @require [size] size >= 0 : "Invalid memory size"
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
 Allocate zero-initialized memory of the given size using the allocator, panicking on failure.

 @param [&inout] allocator : "The allocator to use"
 @param size : "The number of bytes to allocate"
 @return "A pointer to zero-initialized memory of the requested size, or null if size is 0"

 @require [size] size >= 0 : "Invalid memory size"
*>
macro void* calloc(Allocator allocator, sz size) @nodiscard
{
	return calloc_try(allocator, size)!!;
}

<*
 Allocate zero-initialized memory of the given size using the allocator.

 @param [&inout] allocator : "The allocator to use"
 @param size : "The number of bytes to allocate"
 @return "A pointer to zero-initialized memory of the requested size, or null if size is 0"
 @return? mem::INVALID_ALLOC_SIZE, mem::OUT_OF_MEMORY

 @require [size] size >= 0 : "Invalid memory size"
*>
macro void*? calloc_try(Allocator allocator, sz size) @nodiscard
{
	if (!size) return null;
	return allocator.acquire(size, ZERO);
}

<*
 Resize a previously allocated block to a new size, panicking on failure.

 @param [&inout] allocator : "The allocator that acquired `ptr`"
 @param [drop] ptr : "The pointer to resize, may be null to allocate new memory"
 @param new_size : "The new size in bytes"
 @return "A pointer to memory of the new size, or null if new_size is 0 (which also frees `ptr`)"

 @require [new_size] new_size >= 0 : "Invalid memory size"
*>
macro void* realloc(Allocator allocator, void* ptr, sz new_size) @nodiscard
{
	return realloc_try(allocator, ptr, new_size)!!;
}

<*
 Resize a previously allocated block to a new size.

 @param [&inout] allocator : "The allocator that acquired `ptr`"
 @param [drop] ptr : "The pointer to resize, may be null to allocate new memory"
 @param new_size : "The new size in bytes"
 @return "A pointer to memory of the new size, or null if new_size is 0 (which also frees `ptr`)"
 @return? mem::INVALID_ALLOC_SIZE, mem::OUT_OF_MEMORY

 @require [new_size] new_size >= 0 : "Invalid memory size"
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

<*
 Release memory acquired using `malloc`, `calloc` or `realloc`. Passing a null pointer is a no-op.

 @param [&inout] allocator : "The allocator that acquired `ptr`"
 @param [drop] ptr : "The pointer to release, may be null"
*>
macro void free(Allocator allocator, void* ptr)
{
	if (!ptr) return;
	$if env::TESTING:
		((char*)ptr)[0] = 0xBA;
	$endif
	allocator.release(ptr, false);
}

<*
 Allocate over-aligned memory of the given size. Must be released using `free_aligned`.

 @param [&inout] allocator : "The allocator to use"
 @param size : "The number of bytes to allocate"
 @param alignment : "The required alignment, must be a power of 2"
 @return "A pointer to uninitialized memory of the requested size and alignment (0xAA-filled in test mode), or null if size is 0"
 @return? mem::INVALID_ALLOC_SIZE, mem::OUT_OF_MEMORY

 @require [size] size >= 0 : "Invalid memory size"
 @require [alignment] mem::is_valid_alignment(alignment) : "The alignment must be a valid power of two"
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
 Allocate zero-initialized, over-aligned memory of the given size. Must be released using
 `free_aligned`.

 @param [&inout] allocator : "The allocator to use"
 @param size : "The number of bytes to allocate"
 @param alignment : "The required alignment, must be a power of 2"
 @return "A pointer to zero-initialized memory of the requested size and alignment, or null if size is 0"
 @return? mem::INVALID_ALLOC_SIZE, mem::OUT_OF_MEMORY

 @require [size] size >= 0 : "Invalid memory size"
 @require [alignment] mem::is_valid_alignment(alignment) : "The alignment must be a valid power of two"
*>
macro void*? calloc_aligned(Allocator allocator, sz size, sz alignment) @nodiscard
{
	if (!size) return null;
	return allocator.acquire(size, ZERO, alignment);
}

<*
 Resize a previously allocated over-aligned block to a new size. Must be released using
 `free_aligned`.

 @param [&inout] allocator : "The allocator that acquired `ptr`"
 @param [drop] ptr : "The pointer to resize, may be null to allocate new memory"
 @param new_size : "The new size in bytes"
 @param alignment : "The required alignment, must be a power of 2"
 @return "A pointer to memory of the new size and alignment, or null if new_size is 0 (which also frees `ptr`)"
 @return? mem::INVALID_ALLOC_SIZE, mem::OUT_OF_MEMORY

 @require [new_size] new_size >= 0 : "Invalid memory size"
 @require [alignment] mem::is_valid_alignment(alignment) : "The alignment must be a valid power of two"
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

<*
 Release memory acquired using `malloc_aligned`, `calloc_aligned` or `realloc_aligned`. Passing
 a null pointer is a no-op.

 @param [&inout] allocator : "The allocator that acquired `ptr`"
 @param [drop] ptr : "The pointer to release, may be null"
*>
macro void free_aligned(Allocator allocator, void* ptr)
{
	if (!ptr) return;
	$if env::TESTING:
	((char*)ptr)[0] = 0xBA;
	$endif
	allocator.release(ptr, aligned: true);
}

<*
 Allocate a new value of the given type, panicking on failure. Zero-initialized unless `#init`
 is given, in which case the value is initialized to `#init` instead.

 @param [&inout] allocator : "The allocator to use"
 @param $Type : "The type to allocate"
 @param #init : "An optional initializer for the allocated value"
 @return "A pointer to the newly allocated value"

 @require [$Type] $Type::alignment <= mem::DEFAULT_MEM_ALIGNMENT : "Types with alignment exceeding the default must use 'new_aligned' instead"
 @require [#init] !$defined(#init) ||| $defined($Type a = #init) : "#init must be an initializer for the type"
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
 Allocate a new value of the given type. Zero-initialized unless `#init` is given, in which
 case the value is initialized to `#init` instead.

 @param [&inout] allocator : "The allocator to use"
 @param $Type : "The type to allocate"
 @param #init : "An optional initializer for the allocated value"
 @return "A pointer to the newly allocated value"
 @return? mem::INVALID_ALLOC_SIZE, mem::OUT_OF_MEMORY

 @require [$Type] $Type::alignment <= mem::DEFAULT_MEM_ALIGNMENT : "Types with alignment exceeding the default must use 'new_aligned' instead"
 @require [#init] !$defined(#init) ||| $defined($Type a = #init) : "#init must be an initializer for the type"
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
 Allocate a new, over-aligned value of the given type. Zero-initialized unless `#init` is
 given, in which case the value is initialized to `#init` instead. This is necessary for types
 with a default memory alignment exceeding DEFAULT_MEM_ALIGNMENT. IMPORTANT! It must be freed
 using `free_aligned`.

 @param [&inout] allocator : "The allocator to use"
 @param $Type : "The type to allocate"
 @param #init : "An optional initializer for the allocated value"
 @return "A pointer to the newly allocated value"

 @require [#init] !$defined(#init) ||| $defined($Type a = #init) : "#init must be an initializer for the type"
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
 Allocate a new, zero-initialized value of the given type with `padding` extra bytes appended
 after it, e.g. for holding a trailing flexible array member.

 @param [&inout] allocator : "The allocator to use"
 @param $Type : "The type to allocate"
 @param padding : "The number of extra bytes to allocate after the value"
 @return "A pointer to the newly allocated value"
 @return? mem::INVALID_ALLOC_SIZE, mem::OUT_OF_MEMORY

 @require [$Type] $Type::alignment <= mem::DEFAULT_MEM_ALIGNMENT : "The type was overaligned"
 @require [padding] padding >= 0 : "Invalid padding size"
*>
macro new_with_padding(Allocator allocator, $Type, sz padding) @nodiscard
{
	return ($Type*)calloc_try(allocator, $Type::size + padding);
}

<*
 Allocate uninitialized memory for a new value of the given type, panicking on failure.

 @param [&inout] allocator : "The allocator to use"
 @param $Type : "The type to allocate"
 @return "A pointer to the newly allocated, uninitialized value"

 @require [$Type] $Type::alignment <= mem::DEFAULT_MEM_ALIGNMENT : "Types with alignment exceeding the default must use 'alloc_aligned' instead"
*>
macro alloc(Allocator allocator, $Type) @nodiscard
{
	return ($Type*)malloc(allocator, $Type::size);
}

<*
 Allocate uninitialized memory for a new value of the given type.

 @param [&inout] allocator : "The allocator to use"
 @param $Type : "The type to allocate"
 @return "A pointer to the newly allocated, uninitialized value"
 @return? mem::INVALID_ALLOC_SIZE, mem::OUT_OF_MEMORY

 @require [$Type] $Type::alignment <= mem::DEFAULT_MEM_ALIGNMENT : "Types with alignment exceeding the default must use 'alloc_aligned' instead"
*>
macro alloc_try(Allocator allocator, $Type) @nodiscard
{
	return ($Type*)malloc_try(allocator, $Type::size);
}

<*
 Allocate uninitialized, over-aligned memory for a new value of the given type. This is
 necessary for types with a default memory alignment exceeding DEFAULT_MEM_ALIGNMENT.
 IMPORTANT! It must be freed using `free_aligned`.

 @param [&inout] allocator : "The allocator to use"
 @param $Type : "The type to allocate"
 @return "A pointer to the newly allocated, uninitialized value"
*>
macro alloc_aligned(Allocator allocator, $Type) @nodiscard
{
	return ($Type*)malloc_aligned(allocator, $Type::size, $Type::alignment);
}

<*
 Allocate uninitialized memory for a new value of the given type with `padding` extra bytes
 appended after it, e.g. for holding a trailing flexible array member.

 @param [&inout] allocator : "The allocator to use"
 @param $Type : "The type to allocate"
 @param padding : "The number of extra bytes to allocate after the value"
 @return "A pointer to the newly allocated, uninitialized value"
 @return? mem::INVALID_ALLOC_SIZE, mem::OUT_OF_MEMORY

 @require [$Type] $Type::alignment <= mem::DEFAULT_MEM_ALIGNMENT : "The type was overaligned"
 @require [padding] padding >= 0 : "Invalid padding size"
*>
macro alloc_with_padding(Allocator allocator, $Type, sz padding) @nodiscard
{
	return ($Type*)malloc_try(allocator, $Type::size + padding);
}

<*
 Allocate a new, zero-initialized array of `elements` values of the given type, panicking on
 failure.

 @param [&inout] allocator : "The allocator to use"
 @param $Type : "The element type"
 @param elements : "The number of elements to allocate"
 @return "A slice covering the newly allocated array"

 @require [$Type] $Type::alignment <= mem::DEFAULT_MEM_ALIGNMENT : "Types with alignment exceeding the default must use 'new_array_aligned' instead"
 @require [elements] elements >= 0 : "Elements may not be negative"
 @require [elements] !@catch($Type::size.overflow_mul(elements)) : "Resulting array size would overflow sz::max"
*>
macro new_array(Allocator allocator, $Type, sz elements) @nodiscard
{
	return new_array_try(allocator, $Type, elements)!!;
}

<*
 Allocate a new, zero-initialized array of `elements` values of the given type.

 @param [&inout] allocator : "The allocator to use"
 @param $Type : "The element type"
 @param elements : "The number of elements to allocate"
 @return "A slice covering the newly allocated array"
 @return? mem::INVALID_ALLOC_SIZE, mem::OUT_OF_MEMORY

 @require [$Type] $Type::alignment <= mem::DEFAULT_MEM_ALIGNMENT : "Types with alignment exceeding the default must use 'new_array_aligned' instead"
 @require [elements] elements >= 0 : "Elements may not be negative"
 @require [elements] !@catch($Type::size.overflow_mul(elements)) : "Resulting array size would overflow sz::max"
*>
macro new_array_try(Allocator allocator, $Type, sz elements) @nodiscard
{
	return (($Type*)calloc_try(allocator, $Type::size * elements))[:elements];
}

<*
 Allocate a new, zero-initialized, over-aligned array of `elements` values of the given type.
 This is necessary for types with a default memory alignment exceeding DEFAULT_MEM_ALIGNMENT.
 IMPORTANT! It must be freed using `free_aligned`.

 @param [&inout] allocator : "The allocator to use"
 @param $Type : "The element type"
 @param elements : "The number of elements to allocate"
 @return "A slice covering the newly allocated array"

 @require [elements] elements >= 0 : "Elements may not be negative"
 @require [elements] !@catch($Type::size.overflow_mul(elements)) : "Resulting array size would overflow sz::max"
*>
macro new_array_aligned(Allocator allocator, $Type, sz elements) @nodiscard
{
	return (($Type*)calloc_aligned(allocator, $Type::size * elements, $Type::alignment))[:elements]!!;
}

<*
 Allocate a new, uninitialized array of `elements` values of the given type, panicking on
 failure.

 @param [&inout] allocator : "The allocator to use"
 @param $Type : "The element type"
 @param elements : "The number of elements to allocate"
 @return "A slice covering the newly allocated array"

 @require [$Type] $Type::alignment <= mem::DEFAULT_MEM_ALIGNMENT : "Types with alignment exceeding the default must use 'alloc_array_aligned' instead"
 @require [elements] elements >= 0 : "Elements may not be negative"
 @require [elements] !@catch($Type::size.overflow_mul(elements)) : "Resulting array size would overflow sz::max"
*>
macro alloc_array(Allocator allocator, $Type, sz elements) @nodiscard
{
	return alloc_array_try(allocator, $Type, elements)!!;
}

<*
 Allocate a new, uninitialized, over-aligned array of `elements` values of the given type. This
 is necessary for types with a default memory alignment exceeding DEFAULT_MEM_ALIGNMENT.
 IMPORTANT! It must be freed using `free_aligned`.

 @param [&inout] allocator : "The allocator to use"
 @param $Type : "The element type"
 @param elements : "The number of elements to allocate"
 @return "A slice covering the newly allocated array"

 @require [elements] elements >= 0 : "Elements may not be negative"
 @require [elements] !@catch($Type::size.overflow_mul(elements)) : "Resulting array size would overflow sz::max"
*>
macro alloc_array_aligned(Allocator allocator, $Type, sz elements) @nodiscard
{
	return (($Type*)malloc_aligned(allocator, $Type::size * elements, $Type::alignment))[:elements]!!;
}

<*
 Allocate a new, uninitialized array of `elements` values of the given type.

 @param [&inout] allocator : "The allocator to use"
 @param $Type : "The element type"
 @param elements : "The number of elements to allocate"
 @return "A slice covering the newly allocated array"
 @return? mem::INVALID_ALLOC_SIZE, mem::OUT_OF_MEMORY

 @require [$Type] $Type::alignment <= mem::DEFAULT_MEM_ALIGNMENT : "Types with alignment exceeding the default must use 'alloc_array_aligned' instead"
 @require [elements] elements >= 0 : "Elements may not be negative"
 @require [elements] !@catch($Type::size.overflow_mul(elements)) : "Resulting array size would overflow sz::max"
*>
macro alloc_array_try(Allocator allocator, $Type, sz elements) @nodiscard
{
	return (($Type*)malloc_try(allocator, $Type::size * elements))[:elements];
}

<*
 Resize a previously allocated array to a new number of elements, panicking on failure.

 @param [&inout] allocator : "The allocator that acquired `ptr`"
 @param [drop] ptr : "The array pointer to resize, may be null to allocate a new array"
 @param $Type : "The element type"
 @param elements : "The new number of elements"
 @return "A slice covering the resized array, or an empty slice if elements is 0"

 @require [$Type] $Type::alignment <= mem::DEFAULT_MEM_ALIGNMENT : "Types with alignment exceeding the default must use 'alloc_array_aligned' instead"
 @require [elements] elements >= 0 : "Elements may not be negative"
 @require [elements] !@catch($Type::size.overflow_mul(elements)) : "Resulting array size would overflow sz::max"
*>
macro realloc_array(Allocator allocator, void* ptr, $Type, sz elements) @nodiscard
{
	return realloc_array_try(allocator, ptr, $Type, elements)!!;
}

<*
 Resize a previously allocated over-aligned array to a new number of elements. Must be released
 using `free_aligned`.

 @param [&inout] allocator : "The allocator that acquired `ptr`"
 @param [drop] ptr : "The array pointer to resize, may be null to allocate a new array"
 @param $Type : "The element type"
 @param elements : "The new number of elements"
 @return "A slice covering the resized array, or an empty slice if elements is 0"

 @require [elements] elements >= 0 : "Elements may not be negative"
 @require [elements] !@catch($Type::size.overflow_mul(elements)) : "Resulting array size would overflow sz::max"
*>
macro realloc_array_aligned(Allocator allocator, void* ptr, $Type, sz elements) @nodiscard
{
	return (($Type*)realloc_aligned(allocator, ptr, $Type::size * elements, $Type::alignment))[:elements]!!;
}

<*
 Resize a previously allocated array to a new number of elements.

 @param [&inout] allocator : "The allocator that acquired `ptr`"
 @param [drop] ptr : "The array pointer to resize, may be null to allocate a new array"
 @param $Type : "The element type"
 @param elements : "The new number of elements"
 @return "A slice covering the resized array, or an empty slice if elements is 0"
 @return? mem::INVALID_ALLOC_SIZE, mem::OUT_OF_MEMORY

 @require [$Type] $Type::alignment <= mem::DEFAULT_MEM_ALIGNMENT : "Types with alignment exceeding the default must use 'alloc_array_aligned' instead"
 @require [elements] elements >= 0 : "Elements may not be negative"
 @require [elements] !@catch($Type::size.overflow_mul(elements)) : "Resulting array size would overflow sz::max"
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
 @require [value] @alignof(value) <= mem::DEFAULT_MEM_ALIGNMENT : "Types with alignment exceeding the default must use 'clone_aligned' instead"
*>
macro clone(Allocator allocator, value) @nodiscard
{
	return new(allocator, $Typeof(value), value);
}

<*
 Clone a slice or array, allocating a new, separate backing array for the copy.

 @param [&inout] allocator : "The allocator used to clone"
 @param slice : "The slice to clone"
 @return "A copy of the slice, backed by newly allocated memory"

 @require [slice] @kindof(slice) == SLICE || @kindof(slice) == ARRAY : "Only arrays and slices are allowed"
*>
macro clone_slice(Allocator allocator, slice) @nodiscard
{
	if (!lengthof(slice)) return {};

	var $Type = $Typeof(slice[0]);

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
	return new_aligned(allocator, $Typeof(value), value)!!;
}

<*
 Clone the value referenced by an `any`, allocating a new copy of its underlying data.

 @param [&inout] allocator : "The allocator to use to clone"
 @param value : "The any whose underlying value should be cloned"
 @return "A new any referencing the newly allocated copy, with the same type as `value`"
*>
fn any clone_any(Allocator allocator, any value) @nodiscard
{
	sz size = value.type.size;
	void* data = malloc(allocator, size);
	mem::copy(data, value.ptr, size);
	return any_make(data, value.type);
}
