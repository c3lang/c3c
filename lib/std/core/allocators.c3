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

alias mem @builtin = thread_allocator ;

// Temp allocator global data

alias tmem @builtin = current_temp;
typedef PoolState = TempAllocator*;
tlocal Allocator current_temp = &LAZY_TEMP;
tlocal TempAllocator* top_temp;
bool auto_create_temp @if(env::EMSCRIPTEN) = false;
tlocal bool auto_create_temp @if(!env::EMSCRIPTEN) = false;

<*
 Push the current pool manually, must be partnered with a `pop_pool(state)`

 @param reserve : "A hint on how much memory should be reserved on the old temp allocator for out of order allocations"
 @require reserve >= 0 : "Reserve may not be negative"
 @returns "The stored state, to pass into the 'pop_pool' call"
*>
fn PoolState push_pool(sz reserve = 0)
{
	Allocator old = top_temp ? current_temp : create_temp_allocator_on_demand();
	current_temp = ((TempAllocator*)old).derive_allocator(reserve)!!;
	return (PoolState)old.ptr;
}

<*
 Pop the current pool manually after a `push_pool`

 @param old : "The old state to restore to, from the push_pool call"
*>
fn void pop_pool(PoolState old)
{
	TempAllocator* temp = (TempAllocator*)old;
	current_temp = temp;
	temp.reset();
}

<*
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

fn sz alignment_for_allocation(sz alignment) @inline
{
	return alignment < mem::DEFAULT_MEM_ALIGNMENT ? mem::DEFAULT_MEM_ALIGNMENT : alignment;
}

struct AlignedBlock
{
	sz len;
	void* start;
}

macro void? @aligned_free(#free_fn, void* old_pointer)
{
	AlignedBlock* desc = (AlignedBlock*)old_pointer - 1;
	$if @kindof(#free_fn(desc.start)) == OPTIONAL:
	#free_fn(desc.start)!;
	$else
	#free_fn(desc.start);
	$endif
}

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
 @require bytes > 0
 @require alignment > 0
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
 @require bytes > 0
 @require alignment > 0
 @require bytes <= sz::max
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
 @require bytes > 0
 @require alignment > 0
 @require bytes <= sz::max
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
 @require bytes > 0
 @require alignment > 0
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
