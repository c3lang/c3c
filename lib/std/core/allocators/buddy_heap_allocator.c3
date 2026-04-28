module std::core::mem::allocators;
import std::io;
import std::math;

const DEFAULT_CAPACITY          @local = 4096;
const DEFAULT_SPLIT_BOUNDARY    @local = 512;
const DEFAULT_COALESCE_BOUNDARY @local = 1024;
const MIN_BLOCK_SIZE            @local = 16;

<*
 Buddy allocator is an allocator that splits the backing buffer into multiple regions called buddy blocks.
 If the allocator cannot find a sufficiently large block, it will try to coalesce blocks to make space — use the 'coalesce_boundary' parameter to tune the upper boundary for coalescing (this can affect how often the allocator performs split <-> coalesce cycles).
 Otherwise, the block will be split to reduce memory usage — use the 'split_boundary' parameter to control this during initialization.
 It may use slightly more memory than other allocators because it can only store allocations sized as powers of two.

 This allocator is intended to be used in environments where there isn't any native libc malloc,
 and it has to be emulated from a memory region, or wrapping linear memory as is the case for plain WASM.
*>
struct BuddyHeapAllocator (Allocator, Printable)
{
	BuddyBlock* head;
	BuddyBlock* tail;
	sz split_boundary;
	sz coalesce_boundary;
	sz init_padding;
	FallbackAlloc* fallback_head;
	MemoryAllocFn alloc_fn;
	MemoryFreeFn free_fn;
}

bitstruct BuddyBlockInner : sz @local
{
	// Embedding 'is_free' inside other state for efficient memory usage.
	bool is_free : 0..0;
	sz size : 1..(bitsizeof(sz) / 2 - 1);
	sz prev_size : (bitsizeof(sz) / 2)..(bitsizeof(sz) - 1);
}

struct BuddyBlock @local
{
	inline BuddyBlockInner _inner_state;
	char[*] memory;
}

bitstruct FallbackAllocState : sz @local
{
	sz padding : 0..9;
	sz size : 10..(bitsizeof(sz) - 1);
}

struct FallbackAlloc @local
{
	// sz size;
	inline FallbackAllocState _state;
	FallbackAlloc* next;
	char[*] memory;
}

<*
 Initializes the allocator structure.
 
 @require coalesce_boundary <= init_capacity && split_boundary <= init_capacity : "Can't exceed the capacity"
 @param split_boundary : "Value in bytes down to which the allocator will split its blocks, recommended value - estimated minimal allocation size"
 @param coalesce_boundary : "Value in bytes up to which the allocator will try to coalesce its blocks, recommended value - estimated most common allocation size"
*>
fn void BuddyHeapAllocator.init(
	&self,
	MemoryAllocFn alloc_fn,
	MemoryFreeFn free_fn = dummy_free_fn,
	sz init_capacity = DEFAULT_CAPACITY,
	sz coalesce_boundary = DEFAULT_COALESCE_BOUNDARY,
	sz split_boundary = DEFAULT_SPLIT_BOUNDARY,
)
{
	sz cap = math::next_power_of_2(init_capacity);
	self.coalesce_boundary = max(math::next_power_of_2(coalesce_boundary), MIN_BLOCK_SIZE);
	self.split_boundary = max(math::next_power_of_2(split_boundary), MIN_BLOCK_SIZE);
	self.head = (BuddyBlock*)(char*)alloc_fn(BuddyBlock::size + cap + mem::DEFAULT_MEM_ALIGNMENT)!!;
	BuddyBlock* aligned_head = mem::aligned_pointer(self.head + 1, mem::DEFAULT_MEM_ALIGNMENT);
	aligned_head--;
	self.init_padding = (char*)aligned_head - (char*)self.head;
	self.head = aligned_head;
	self.head.size = cap;
	self.head.prev_size = 0;
	self.head.is_free = true;
	self.tail = next_block(self.head);
	self.tail.size = 0;
	self.tail.prev_size = cap;
	self.tail.is_free = false;
	self.alloc_fn = alloc_fn;
	self.free_fn = free_fn;
	split_block(self.head, self.split_boundary);
}

<*
 Frees resources of the allocator.
*>
fn void BuddyHeapAllocator.free(&self)
{
	if (@likely(self.head != null))
	{
		self.free_fn((char*)self.head - self.init_padding);
	}
	if (self.fallback_head != null)
	{
		fallback_free_all(self);
	}
	*self = { };
}

<*
 Resets the state of the allocator making it available for reuse.
*>
fn void BuddyHeapAllocator.reset(&self)
{
	sz cap = capacity(self);
	self.head.size = cap;
	self.head.prev_size = 0;
	self.head.is_free = true;
	split_block(self.head, self.split_boundary);
}

<*
 Implements the Allocator interface method.

 @require !alignment || math::is_power_of_2(alignment)
 @require alignment <= mem::MAX_MEMORY_ALIGNMENT : `alignment too big`
 @require size > 0
*>
fn void*? BuddyHeapAllocator.acquire(&self, sz size, AllocInitType init_type, sz alignment) @dynamic
{
	sz req_size = calc_required_alloc_size(size, alignment);
	return _acquire(self, req_size, init_type, alignment);
}

<*
 Called in 'acquire' and 'resize', for not re-calculating allocation size with 'calc_required_alloc_size',
 because it's expensive and reduntant.

 @require math::is_power_of_2(size) : "Should be a power of 2"
*>
fn void*? _acquire(BuddyHeapAllocator* self, sz size, AllocInitType init_type, sz alignment) @local
{
	BuddyBlock* block = find_first_best_fit_block(self, size);

	if (@unlikely(!block))
	{
		block = coalesce_top_bottom(self, size);
		if (!block.is_free)
		{
			return fallback_alloc(self, size, init_type, alignment);
		}
	}

	block.is_free = false;

	if (init_type == ZERO) {
		mem::clear(&block.memory, block.size - BuddyBlock::size);
	}

	return &block.memory;
}

<*
 Implements the Allocator interface method.

 @require !alignment || math::is_power_of_2(alignment)
 @require alignment <= mem::MAX_MEMORY_ALIGNMENT : `alignment too big`
 @require old_pointer != null
 @require size > 0
 @return ? mem::INVALID_ALLOC_SIZE, mem::OUT_OF_MEMORY
*>
fn void*? BuddyHeapAllocator.resize(&self, void* old_pointer, sz size, sz alignment) @dynamic
{
	if (@unlikely(!owns_pointer_in_main_buff(self, old_pointer)))
	{
		return fallback_realloc(self, old_pointer, size, alignment);
	}

	BuddyBlock* old_block = (BuddyBlock*)old_pointer - 1;
	sz old_size = old_block.size;
	sz new_size = calc_required_alloc_size(size, alignment);

	if (@unlikely(old_size == new_size)) return old_pointer;

	void* new_pointer = _acquire(self, new_size, NO_ZERO, alignment)!;
	mem::copy(new_pointer, old_pointer, min(old_size, new_size) - BuddyBlock::size);
	self.release(old_pointer, false);
	
	return new_pointer;
}

<*
 Implements the Allocator interface method.

 @require ptr != null
*>
fn void BuddyHeapAllocator.release(&self, void* ptr, bool) @dynamic
{
	if (@unlikely(!owns_pointer_in_main_buff(self, ptr)))
	{
		return fallback_free(self, ptr);
	}
	BuddyBlock* block = (BuddyBlock*)ptr - 1;
	// Prevent double free.
	if (@unlikely(block.is_free)) return;

	block.is_free = true;
	if (block.size < self.coalesce_boundary)
	{
		coalesce_bottom_top_free_block(self, block, self.coalesce_boundary);
	}
}

// ------ Private API ------

<*
 Tries to find the smallest block that can fit the size of requested memory region, and
 splits the block according to the allocation size. If no block can be found,
 the contiguous free blocks are coalesced and sufficient block is returned to the user.

 @require math::is_power_of_2(req_size) : "Should be a power of 2"
*>
fn BuddyBlock* find_first_best_fit_block(BuddyHeapAllocator* self, sz req_size) @local
{
	BuddyBlock* best_block = null;
	BuddyBlock* block = self.head;
	BuddyBlock* buddy = next_block(block);

	while (@likely(block < self.tail) && @likely(buddy < self.tail))
	{
		if (block.size < req_size && can_coalesce(block, buddy))
		{
			block = coalesce_bottom_top(self, block, buddy, req_size);
			if (test_best_block_priority(block, &best_block, req_size)) return block;

			block = next_block(buddy);
			if (block < self.tail)
			{
				buddy = next_block(block);
			}
			continue;
		}
	
		if (test_best_block_priority(buddy, &best_block, req_size)) return buddy;
		if (test_best_block(block, &best_block, req_size)) return block;

		// Check if buddy was split into smaller blocks.
		if (@likely(buddy.size < block.size))
		{
			block = buddy;
			buddy = next_block(block);
		}
		else
		{
			block = next_block(buddy);
			if (block < self.tail)
			{
				buddy = next_block(block);
			}
		}
	}

	if (best_block != null && best_block.size > req_size)
	{
		return split_block(best_block, req_size);
	}

	return best_block;
}

<*
 Performs exhaustive coalescing top-bottom on the blocks up to 'req_size'.
 This function is heavier than 'coalesce_bottom_top', and only called when no sufficient block is found.

 @ensure return != null : "Must return non-null block"
*>
fn BuddyBlock* coalesce_top_bottom(BuddyHeapAllocator* self, sz req_size)
{
	BuddyBlock* block = self.head;
	BuddyBlock* buddy = next_block(block);

	while (block < self.tail && buddy < self.tail)
	{
		if (can_coalesce(block, buddy))
		{
			block = coalesce_bottom_top(self, block, buddy, req_size);
			if (block.size >= req_size) return block;
		}
	
		// Check if buddy was split into smaller blocks.
		if (buddy.size < block.size)
		{
			BuddyBlock* curr_block = block;
			BuddyBlock* curr_buddy = buddy;

			while (curr_buddy.size < curr_block.size)
			{
				curr_block = curr_buddy;
				curr_buddy = next_block(curr_block);
			}

			BuddyBlock* res = coalesce_bottom_top(self, curr_block, curr_buddy, req_size);
			if (res && res.size >= req_size)
			{
				return res;
			}
		}

		// Step forward.
		block = next_block(buddy);
		if (block < self.tail)
		{
			buddy = next_block(block);
		}
	}

	return block;
}

<*
 Called only after 'release' and forwards to 'coalesce_bottom_top'.

 @require free_block.is_free : "Should be free block"
 @require req_size <= capacity(self) : "Shoudn't exceed capacity"
*>
fn void coalesce_bottom_top_free_block(BuddyHeapAllocator* self, BuddyBlock* free_block, sz req_size) @local
{
	BuddyBlock* block = null;
	BuddyBlock* buddy = null;
	
	// Initially we don't know if provided block is buddy or not.
	if (@likely(is_buddy(free_block)))
	{
		buddy = free_block;
		block = prev_block(buddy);
	}
	else
	{
		block = free_block;
		buddy = next_block(block);
	}

	coalesce_bottom_top(self, block, buddy, req_size);
}

<*
 Performs coalescing bottom-top on the blocks up to 'req_size'.
 This function is way faster than 'coalesce_top_bottom', because it knows where to start from.

 @require block != null && buddy != null : "Shouldn't be null"
 @require math::is_power_of_2(req_size) : "Must be a power of 2"
 @require req_size <= capacity(self) : "Shoudn't exceed capacity"
*>
fn BuddyBlock* coalesce_bottom_top(BuddyHeapAllocator* self, BuddyBlock* block, BuddyBlock* buddy, sz req_size) @local
{
	while (block >= self.head && can_coalesce(block, buddy))
	{
		block.size <<= 1;
		if (block.size == req_size)
		{
			return block;
		}
		
		buddy = block;
		block = prev_block(block);
	}

	return block;
}

<*
 Checks if blocks can be coalesced.

 @require block != null && buddy != null : "Shouldn't be null"
*>
fn bool can_coalesce(BuddyBlock* block, BuddyBlock* buddy) @inline @local
{
	return block.is_free && buddy.is_free && block.size == buddy.size;
}

<*
 Splits the block down to multiple blocks with size == 'req_size'.

 @require math::is_power_of_2(req_size) : "Required allocation size must be a power of 2"
 @require block != null && block.is_free : "Block should exist and should be free"
 @require req_size <= block.size : "Required size should be smaller or equal to initial block size"
*>
fn BuddyBlock* split_block(BuddyBlock* block, sz req_size)
{
	sz curr_size = block.size;

	while (curr_size > req_size)
	{
		curr_size >>= 1;
		block.size = curr_size;
		// Move to buddy.
		block = next_block(block);
		block.size = curr_size;
		block.prev_size = curr_size;
		block.is_free = true;
	}

	return block;
}

<*
 Goes to the next block in memory.
*>
fn BuddyBlock* next_block(BuddyBlock* block) @inline @local
{
	return (BuddyBlock*)((char*)block + block.size);
}

<*
 Goes to the previous block in memory.
*>
fn BuddyBlock* prev_block(BuddyBlock* block) @inline @local
{
	return (BuddyBlock*)((char*)block - block.prev_size);
}

<*
 Tells if provided block is 'buddy' (the right one).
*>
fn bool is_buddy(BuddyBlock* block) @inline @local
{
	return block.prev_size == block.size;
}

<*
 Bumps 'req_size' value so it can fit allocation header and be aligned to closest power of 2.
 Input isn't required to be a power of 2.

 @ensure math::is_power_of_2(return) : "Result should be a power of 2"
*>
fn sz calc_required_alloc_size(sz req_size, sz alignment) @local
{
	alignment = allocators::alignment_for_allocation(alignment);
	req_size = mem::aligned_offset(req_size + BuddyBlock::size, alignment);
	return max(math::next_power_of_2(req_size), MIN_BLOCK_SIZE);
}

<*
 Checks if the provided block is better fits requested size than current 'best_block' if it exists.

 @return "True if size of the provided block is equal to 'req_size', otherwise false"
*>
fn bool test_best_block(BuddyBlock* block, BuddyBlock** best_block, sz req_size) @local
{
	if (block.is_free && block.size >= req_size && (*best_block == null || block.size < (*best_block).size))
	{
		*best_block = block;
		return block.size == req_size;
	}
	return false;
}

<*
 Checks if the provided block is better fits requested size than current 'best_block' if it exists.
 Takes priority over current 'best_block' even if sizes are equal.

 @return "True if size of the provided block is equal to 'req_size', otherwise false"
*>
fn bool test_best_block_priority(BuddyBlock* block, BuddyBlock** best_block, sz req_size) @local
{
	if (block.is_free && block.size >= req_size && (*best_block == null || block.size <= (*best_block).size))
	{
		*best_block = block;
		return block.size == req_size;
	}
	return false;
}

<*
 Checks if pointer is owned by the allocator.
*>
fn bool owns_pointer_in_main_buff(BuddyHeapAllocator* self, void* ptr) @inline @local
{
	return ptr >= self.head && ptr < self.tail;
}

<*
 Calculates capacity from 'head' and 'tail' pointers.
*>
fn sz capacity(BuddyHeapAllocator* self) @inline @local
{
	return (char*)self.tail - (char*)self.head;
}

// ---- Fallback allocations ----

fn void*? fallback_alloc(BuddyHeapAllocator* self, sz size, AllocInitType init_type, sz alignment) @local
{
	if (alignment > 0)
	{
		size = mem::aligned_offset(size + FallbackAlloc::size + alignment, mem::DEFAULT_MEM_ALIGNMENT);
		char* alloc = (char*)self.alloc_fn(size)!;
		FallbackAlloc* aligned_alloc = (FallbackAlloc*)mem::aligned_pointer(alloc + FallbackAlloc::size, alignment);
		aligned_alloc--;
		assert((char*)aligned_alloc >= alloc, "Alignment failed: %p < %p", aligned_alloc, alloc);
		ushort padding = (ushort)((char*)aligned_alloc - alloc);
		aligned_alloc.size = size - padding - FallbackAlloc::size;
		aligned_alloc.padding = padding;
		aligned_alloc.next = self.fallback_head;
		if (init_type == ZERO)
		{
			mem::clear(&aligned_alloc.memory, aligned_alloc.size);
		}
		self.fallback_head = aligned_alloc;
		return &aligned_alloc.memory;
	}
	else
	{
		size = mem::aligned_offset(size + FallbackAlloc::size, mem::DEFAULT_MEM_ALIGNMENT);
		FallbackAlloc* alloc = (FallbackAlloc*)(char*)self.alloc_fn(size)!;
		alloc.size = size - FallbackAlloc::size;
		alloc.padding = 0;
		alloc.next = self.fallback_head;
		if (init_type == ZERO)
		{
			mem::clear(&alloc.memory, alloc.size);
		}
		self.fallback_head = alloc;
		return &alloc.memory;
	}
}

<*
 Reallocates allocation made as a fallback.

 @require old_ptr != null : "Shouldn't be null"
 @require new_size > 0 : "Should be bigger than 0"
*>
fn void*? fallback_realloc(BuddyHeapAllocator* self, void* old_ptr, sz new_size, sz alignment) @local
{
	FallbackAlloc* block = (FallbackAlloc*)old_ptr - 1;
	if (block.size >= new_size) return old_ptr;
	void* new_alloc = fallback_alloc(self, new_size, NO_ZERO, alignment)!;
	sz max_to_copy = math::min(block.size, new_size);
	mem::copy(new_alloc, old_ptr, max_to_copy);
	fallback_free(self, old_ptr);
	return new_alloc;
}

<*
 Frees allocation made as a fallback.
*>
fn void fallback_free(BuddyHeapAllocator* self, void* ptr) @local
{
	if (!self.fallback_head || !ptr)
	{
		return;
	}

	FallbackAlloc* curr = self.fallback_head;
	FallbackAlloc* prev = curr;

	while (curr)
	{
		if (&curr.memory == ptr)
		{
			break;
		}
		prev = curr;
		curr = curr.next;
	}

	assert(curr != null, "Pointer isn't owned by this allocator");

	// It was the root.
	if (prev == curr)
	{
		self.fallback_head = self.fallback_head.next;
		curr = (FallbackAlloc*)((char*)curr - curr.padding);
		self.free_fn(curr);
		return;
	}
	else
	{
		FallbackAlloc* next = curr.next;
		curr = (FallbackAlloc*)((char*)curr - curr.padding);
		self.free_fn(curr);
		prev.next = next;
	}
}

<*
 Frees all allocations made as a fallback.
*>
fn void fallback_free_all(BuddyHeapAllocator* self) @local
{
	FallbackAlloc* curr = self.fallback_head;
	while (curr)
	{
		FallbackAlloc* next = curr.next;
		curr = (FallbackAlloc*)((char*)curr - curr.padding);
		self.free_fn(curr);
		curr = next;
	}
	self.fallback_head = null;
}

<*
 Calculates all memory that were allocated as a fallback.
*>
fn sz calc_used_fallback_mem(BuddyHeapAllocator* self) @local
{
	sz res = 0;
	FallbackAlloc* curr = self.fallback_head;
	while (curr)
	{
		res += curr.size;
		curr = curr.next;
	}
	return res;
}

<*
 Implements 'Printable' interface.
*>
fn sz? BuddyHeapAllocator.to_format(&self, Formatter* fmt) @dynamic
{
	BuddyBlock* block = self.head;
	BuddyBlock* buddy = next_block(block);
	sz total = fmt.print("Buddy Allocator state:")!;
	sz used = 0;
	sz fallback_used = calc_used_fallback_mem(self); 
	bool block_is_prev_buddy = false;

	if (!block.is_free) used += block.size;
	if (!buddy.is_free) used += buddy.size;

	while (block < self.tail && buddy < self.tail)
	{
		String block_free_state = block.is_free ? "[free]" : "[in use]";
		String buddy_free_state = buddy.is_free ? "[free]" : "[in use]";
		total += fmt.printf("\n\tblock: %s %s -- buddy: %s %s", block.size, block_free_state, buddy.size, buddy_free_state)!;
		// Check if buddy was split into smaller blocks
		if (buddy.size < block.size)
		{
			total += fmt.print("\n\t! block is prev buddy !")!;
			block = buddy;
			buddy = next_block(block);
			block_is_prev_buddy = true;
		}
		else
		{
			block = next_block(buddy);
			if (block < self.tail)
			{
				buddy = next_block(block);
			}
			block_is_prev_buddy = false;
		}

		if (block_is_prev_buddy)
		{
			if (!buddy.is_free) used += buddy.size;
		}
		else
		{
			if (!block.is_free) used += block.size;
			if (!buddy.is_free) used += buddy.size;
		}
	}

	total += fmt.printf("\n\tcapacity: %s -- used: %s -- extra allocated: %s", capacity(self), used, fallback_used)!;
	return total;
}
