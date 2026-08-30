module std::core::mem::allocators;

// C3 has several different allocators available:
//
// Name                     Arena  Uses buffer  OOM Fallback?     Mark?   Reset?
// ArenaAllocator             Yes          Yes             No       Yes      Yes
// BackedArenaAllocator       Yes           No            Yes       Yes      Yes
// DynamicArenaAllocator      Yes           No            Yes        No      Yes
// HeapAllocator               No           No             No        No       No   *Note: Not for normal use
// LibcAllocator               No           No             No        No       No   *Note: Wraps malloc
// OnStackAllocator           Yes          Yes            Yes        No       No   *Note: Used by @stack_mem
// TempAllocator              Yes           No            Yes        No*      No*  *Note: Mark/reset using @pool
// TrackingAllocator           No           No            N/A        No       No   *Note: Wraps other heap allocator
// Vmem                       Yes           No             No       Yes      Yes   *Note: Can be set to huge sizes

// Heap allocator global data

<* The default heap allocator for the current thread. *>
alias mem @builtin = thread_allocator ;

// Temp allocator global data

<* The current thread's active temp allocator, see `push_pool`/`pop_pool` and `@pool`. *>
alias tmem @builtin = current_temp;
<* Opaque handle to a previous temp allocator state, returned by `push_pool` and consumed by `pop_pool`. *>
typedef PoolState = TempAllocator*;
tlocal Allocator current_temp = &LAZY_TEMP;
tlocal TempAllocator* top_temp;
bool auto_create_temp @feat(EMSCRIPTEN) = false;
tlocal bool auto_create_temp @feat(!EMSCRIPTEN) = false;

<*
 Push the current pool manually, must be partnered with a `pop_pool(state)`.

 @param reserve : "A hint on how much memory should be reserved on the old temp allocator for out of order allocations"
 @return "The stored state, to pass into the 'pop_pool' call"

 @require [reserve] reserve >= 0 : "Reserve may not be negative"
*>
fn PoolState push_pool(sz reserve = 0)
{
	Allocator old = top_temp ? current_temp : create_temp_allocator_on_demand();
	current_temp = ((TempAllocator*)old).derive_allocator(reserve)!!;
	return (PoolState)old.ptr;
}

<*
 Pop the current pool manually after a `push_pool`.

 @param [&inout] old : "The old state to restore to, from the push_pool call"
*>
fn void pop_pool(PoolState old)
{
	TempAllocator* temp = (TempAllocator*)old;
	current_temp = temp;
	temp.reset();
}

<*
 Create the top-level temp allocator for the current thread.

 @param [&inout] allocator : "The backing allocator used to acquire the temp allocator's buffer"
 @param size : "The size in bytes of the temp allocator's initial buffer"
 @param reserve : "A hint on how much memory should be reserved for out of order allocations"
 @param min_size : "The minimum size a derived (pushed) pool may have"
 @param realloc_size : "The size used when the temp allocator falls back to heap allocations"
 @return "The newly created top-level temp allocator"

 @require !top_temp : "This should never be called when temp already exists"
*>
fn Allocator create_temp_allocator(Allocator allocator, sz size, sz reserve, sz min_size, sz realloc_size) @private
{
	return current_temp = top_temp = allocators::new_temp_allocator(allocator, size, reserve, min_size, realloc_size)!!;
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

// Helper functionality for building allocators:

<* Default prefix for holding a size in a malloc block *>
const DEFAULT_SIZE_PREFIX = sz::size;
<* Default alignment required *>
const DEFAULT_SIZE_PREFIX_ALIGNMENT = sz::alignment;

<*
 Round an alignment up to at least the default memory alignment.

 @param alignment : "The requested alignment, 0 meaning 'no particular alignment'"
 @return "`alignment` if it's at least DEFAULT_MEM_ALIGNMENT, otherwise DEFAULT_MEM_ALIGNMENT"
*>
fn sz alignment_for_allocation(sz alignment) @inline
{
	return alignment < mem::DEFAULT_MEM_ALIGNMENT ? mem::DEFAULT_MEM_ALIGNMENT : alignment;
}

<*
 Header stored just before the memory returned by `@aligned_alloc`/`@aligned_alloc_fn`, so it
 can later be located and freed/resized via `@aligned_free`/`@aligned_realloc` and friends.
*>
struct AlignedBlock
{
	sz len;
	void* start;
}

<*
 Support aligned free for something allocated using `@aligned_alloc`.

 @param #free_fn : "The underlying (unaligned) free function to release the real allocation with"
 @param [&drop] old_pointer : "The aligned pointer previously returned by `@aligned_alloc`"
*>
macro void? @aligned_free(#free_fn, void* old_pointer)
{
	AlignedBlock* desc = (AlignedBlock*)old_pointer - 1;
	$if @kindof(#free_fn(desc.start)) == OPTIONAL:
	#free_fn(desc.start)!;
	$else
	#free_fn(desc.start);
	$endif
}

<*
 Support aligned free for something allocated using `@aligned_alloc_fn`.

 @param context : "The context value to forward to `#free_fn`"
 @param #free_fn : "The underlying (unaligned) free function to release the real allocation with"
 @param [&drop] old_pointer : "The aligned pointer previously returned by `@aligned_alloc_fn`"
*>
macro void? @aligned_free_fn(context, #free_fn, void* old_pointer)
{
	AlignedBlock* desc = (AlignedBlock*)old_pointer - 1;
	$if @kindof(#free_fn(context, desc.start)) == OPTIONAL:
	#free_fn(context, desc.start)!;
	$else
	#free_fn(context, desc.start);
	$endif
}

<*
 Support aligned realloc on top of a standard calloc/free pair, by relocating the header added
 by `@aligned_alloc`.

 @param #calloc_fn : "The underlying (unaligned) allocation function used to acquire the new, larger block"
 @param #free_fn : "The underlying (unaligned) free function used to release the old block"
 @param [&drop] old_pointer : "The aligned pointer previously returned by `@aligned_alloc`"
 @param bytes : "The new size in bytes"
 @param alignment : "The required alignment, must be a power of 2"
 @return "A new aligned pointer of the requested size, with the old data preserved"

 @require [bytes] bytes > 0 : "A non-zero amount bytes were expected."
 @require [bytes] bytes < sz::max - (AlignedBlock::size + alignment) : "Too big block allocated"
 @require [alignment] alignment > 0 : "A non-zero alignment was expected"
*>
macro void*? @aligned_realloc(#calloc_fn, #free_fn, void* old_pointer, sz bytes, sz alignment)
{
	AlignedBlock* desc = (AlignedBlock*)old_pointer - 1;
	void* data_start = desc.start;
	void* new_data = @aligned_alloc(#calloc_fn, bytes, alignment)!;
	mem::copy(new_data, old_pointer, desc.len < bytes ? desc.len : bytes, 1, 1);
	$if @kindof(#free_fn(data_start)) == OPTIONAL:
	#free_fn(data_start)!;
	$else
	#free_fn(data_start);
	$endif
	return new_data;
}
<*
 Support aligned alloc on top of a standard alloc function, by prefixing the allocation with an
 `AlignedBlock` header.

 @param #alloc_fn : "The underlying (unaligned) allocation function to acquire the real allocation with"
 @param bytes : "The number of bytes to allocate"
 @param alignment : "The required alignment, must be a power of 2"
 @return "A pointer aligned to `alignment`, holding `bytes` usable bytes. Must be released using `@aligned_free`"

 @require [bytes] bytes > 0 : "A non-zero amount bytes were expected."
 @require [bytes] bytes < sz::max - (AlignedBlock::size + alignment) : "Too big block allocated"
 @require [alignment] alignment > 0 : "A non-zero alignment was expected"
*>
macro void*? @aligned_alloc(#alloc_fn, sz bytes, sz alignment)
{
	if (alignment < void*::alignment) alignment = void*::alignment;
	sz header = AlignedBlock::size + alignment;
	sz alignsize = bytes + header;
	$if @kindof(#alloc_fn(bytes)) == OPTIONAL:
		void* data = #alloc_fn(alignsize)!;
	$else
		void* data = #alloc_fn(alignsize);
	$endif
	void* mem = mem::aligned_pointer(data + AlignedBlock::size, alignment);
	AlignedBlock* desc = (AlignedBlock*)mem - 1;
	assert(mem > data);
	*desc = { bytes, data };
	return mem;
}

<*
 Support aligned alloc on top of a standard alloc function taking a context, by prefixing the
 allocation with an `AlignedBlock` header.

 @param context : "The context value to forward to `#alloc_fn`"
 @param #alloc_fn : "The underlying (unaligned) allocation function to acquire the real allocation with"
 @param bytes : "The number of bytes to allocate"
 @param alignment : "The required alignment, must be a power of 2"
 @return "A pointer aligned to `alignment`, holding `bytes` usable bytes. Must be released using `@aligned_free_fn`"

 @require [bytes] bytes > 0 : "A non-zero amount bytes were expected."
 @require [bytes] bytes < sz::max - (AlignedBlock::size + alignment) : "Too big block allocated"
 @require [alignment] alignment > 0 : "A non-zero alignment was expected"
*>
macro void*? @aligned_alloc_fn(context, #alloc_fn, sz bytes, sz alignment)
{
	if (alignment < void*::alignment) alignment = void*::alignment;
	sz header = AlignedBlock::size + alignment;
	sz alignsize = bytes + header;
	$if @kindof(#alloc_fn(context, bytes)) == OPTIONAL:
		void* data = #alloc_fn(context, alignsize)!;
	$else
		void* data = #alloc_fn(context, alignsize);
	$endif
	void* mem = mem::aligned_pointer(data + AlignedBlock::size, alignment);
	AlignedBlock* desc = (AlignedBlock*)mem - 1;
	assert(mem > data);
	*desc = { bytes, data };
	return mem;
}



<*
 Support aligned realloc on top of a standard calloc/free pair taking a context, by relocating
 the header added by `@aligned_alloc_fn`.

 @param context : "The context value to forward to `#calloc_fn`/`#free_fn`"
 @param #calloc_fn : "The underlying (unaligned) allocation function used to acquire the new, larger block"
 @param #free_fn : "The underlying (unaligned) free function used to release the old block"
 @param [&drop] old_pointer : "The aligned pointer previously returned by `@aligned_alloc_fn`"
 @param bytes : "The new size in bytes"
 @param alignment : "The required alignment, must be a power of 2"
 @return "A new aligned pointer of the requested size, with the old data preserved"

 @require [bytes] bytes > 0 : "A non-zero amount bytes were expected."
 @require [bytes] bytes < sz::max - (AlignedBlock::size + alignment) : "Too big block allocated"
 @require [alignment] alignment > 0 : "A non-zero alignment was expected"
*>
macro void*? @aligned_realloc_fn(context, #calloc_fn, #free_fn, void* old_pointer, sz bytes, sz alignment)
{
	AlignedBlock* desc = (AlignedBlock*)old_pointer - 1;
	void* data_start = desc.start;
	void* new_data = @aligned_alloc_fn(context, #calloc_fn, bytes, alignment)!;
	mem::copy(new_data, old_pointer, desc.len < bytes ? desc.len : bytes, 1, 1);
	$if @kindof(#free_fn(context, data_start)) == OPTIONAL:
	#free_fn(context, data_start)!;
	$else
	#free_fn(context, data_start);
	$endif
	return new_data;
}
