module buddy_alloc;
import std::io;
import std::math;

const uint DEFAULT_CAPACITY = 4096;
const DEFAULT_SPLIT_BOUNDARY = 512;
const DEFAULT_COALESCE_BOUNDARY = 1024;
const MIN_BLOCK_SIZE = 16;

<*
 Buddy allocator is a type of allocator that splits the backing buffer into multiple regions called buddy blocks.
 If the allocator cannot find a sufficiently large block, it will try to coalesce blocks to make space — use the 'coalesce_boundary' parameter to tune the upper boundary for coalescing (this can affect how often the allocator performs split <-> coalesce cycles).
 Otherwise, the block will be split to reduce memory usage — use the 'split_boundary' parameter to control this during initialization.
 It may use slightly more memory than other allocators because it can only store allocations sized as powers of two.
 When the allocator exhausts its memory, it falls back to its backing allocator. Fallback allocations are automatically freed when the allocator is destroyed; the user must not free them manually. Fallback allocations can also be resized.
*>
struct BuddyAlloc (Allocator, Printable)
{
	BuddyBlock* head;
	BuddyBlock* tail;
	uint split_boundary;
	uint coalesce_boundary;
	sz init_padding;
	FallbackAlloc* fallback_head;
	Allocator backing_alloc;
}

bitstruct BuddyBlockInner : ulong @local
{
	// Embedding 'is_free' inside other state for efficient memory usage.
	bool is_free : 0..0;
	uint size : 1..31;
	uint prev_size : 32..63;
}

struct BuddyBlock @local
{
	inline BuddyBlockInner _inner_state;
	char[*] memory;
}

struct FallbackAlloc @local
{
	FallbackAlloc* next;
	uint size;
	char[*] memory;
}

<*
 Initializes the allocator structure.
 
 @require coalesce_boundary <= init_capacity && split_boundary <= init_capacity : "Can't exceed the capacity"
 @param split_boundary : "Value in bytes down to which the allocator will split its blocks, recommended value - estimated minimal allocation size"
 @param coalesce_boundary : "Value in bytes up to which the allocator will try to coalesce its blocks, recommended value - estimated most common allocation size"
*>
fn void BuddyAlloc.init(
	&self,
	Allocator backing_alloc = mem,
	uint init_capacity = DEFAULT_CAPACITY,
	uint coalesce_boundary = DEFAULT_COALESCE_BOUNDARY,
	uint split_boundary = DEFAULT_SPLIT_BOUNDARY,
)
{
	uint cap = math::next_power_of_2(init_capacity);
	self.coalesce_boundary = max(math::next_power_of_2(coalesce_boundary), MIN_BLOCK_SIZE);
	self.split_boundary = max(math::next_power_of_2(split_boundary), MIN_BLOCK_SIZE);
	self.head = alloc::alloc_with_padding(backing_alloc, BuddyBlock, cap + mem::DEFAULT_MEM_ALIGNMENT)!!;
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
	self.backing_alloc = backing_alloc;
	split_block(self.head, self.split_boundary);
}

<*
 Frees resources of the allocator.
*>
fn void BuddyAlloc.free(&self)
{
	if (@likely(self.head != null))
	{
		alloc::free(self.backing_alloc, (char*)self.head - self.init_padding);
	}
	if (self.fallback_head)
	{
		free_fallback_allocations(self);
	}
	*self = { };
}

<*
 Resets the state of the allocator making it available for reuse.
*>
fn void BuddyAlloc.reset(&self)
{
	uint cap = capacity(self);
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
fn void*? BuddyAlloc.acquire(&self, sz size, AllocInitType init_type, sz alignment) @dynamic
{
	uint req_size = calc_required_alloc_size((uint)size, alignment);
	return _acquire(self, req_size, init_type);
}

<*
 Called in 'acquire' and 'resize', for not re-calculating allocation size with 'calc_required_alloc_size',
 because it's expensive and reduntant.

 @require math::is_power_of_2(size) : "Should be a power of 2"
*>
fn void*? _acquire(BuddyAlloc* self, uint size, AllocInitType init_type) @local
{
	BuddyBlock* block = find_first_best_fit_block(self, size);

	if (@unlikely(!block))
	{
		block = coalesce_top_bottom(self, size);
		if (!block.is_free)
		{
			return make_fallback_allocation(self, size, init_type);
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
fn void*? BuddyAlloc.resize(&self, void* old_pointer, sz size, sz alignment) @dynamic
{
	if (@unlikely(!owns_pointer(self, old_pointer)))
	{
		return resize_fallback_allocation(self, old_pointer, (uint)size);
	}

	BuddyBlock* old_block = (BuddyBlock*)old_pointer - 1;
	uint old_size = old_block.size;
	uint new_size = calc_required_alloc_size((uint)size, alignment);

	if (@unlikely(old_size == new_size)) return old_pointer;

	void* new_pointer = _acquire(self, new_size, NO_ZERO)!;
	mem::copy(new_pointer, old_pointer, min(old_size, new_size) - BuddyBlock::size);
	self.release(old_pointer, false);
	
	return new_pointer;
}

<*
 Implements the Allocator interface method.

 @require ptr != null
*>
fn void BuddyAlloc.release(&self, void* ptr, bool) @dynamic
{
	if (@unlikely(!owns_pointer(self, ptr))) return;
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
fn BuddyBlock* find_first_best_fit_block(BuddyAlloc* self, uint req_size) @local
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
fn BuddyBlock* coalesce_top_bottom(BuddyAlloc* self, uint req_size)
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
fn void coalesce_bottom_top_free_block(BuddyAlloc* self, BuddyBlock* free_block, uint req_size) @local
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
fn BuddyBlock* coalesce_bottom_top(BuddyAlloc* self, BuddyBlock* block, BuddyBlock* buddy, uint req_size) @local
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
fn BuddyBlock* split_block(BuddyBlock* block, uint req_size)
{
	uint curr_size = block.size;

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
fn uint calc_required_alloc_size(uint req_size, sz alignment) @local
{
	alignment = allocators::alignment_for_allocation(alignment);
	req_size = (uint)mem::aligned_offset(req_size + BuddyBlock::size, alignment);
	return max(math::next_power_of_2(req_size), MIN_BLOCK_SIZE);
}

<*
 Checks if the provided block is better fits requested size than current 'best_block' if it exists.

 @return "True if size of the provided block is equal to 'req_size', otherwise false"
*>
fn bool test_best_block(BuddyBlock* block, BuddyBlock** best_block, uint req_size) @local
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
fn bool test_best_block_priority(BuddyBlock* block, BuddyBlock** best_block, uint req_size) @local
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
fn bool owns_pointer(BuddyAlloc* self, void* ptr) @inline @local
{
	return ptr >= self.head && ptr < self.tail;
}

<*
 Calculates capacity from 'head' and 'tail' pointers.
*>
fn uint capacity(BuddyAlloc* self) @inline @local
{
	return (uint)((char*)self.tail - (char*)self.head);
}

// ----- Fallback allocations -----

<*
 Allocates to the separate linked list in case there's no free blocks in the Buddy allocator.

 @require size > 0
*>
fn void*? make_fallback_allocation(BuddyAlloc* self, uint size, AllocInitType init_type) @local
{
	FallbackAlloc* curr = self.fallback_head;
	while (curr)
	{
		curr = curr.next;
	}

	curr = alloc::alloc_with_padding(self.backing_alloc, FallbackAlloc, size)!;
	curr.size = size;
	curr.next = null;

	if (!self.fallback_head)
	{
		self.fallback_head = curr;
	}

	if (init_type == ZERO)
	{
		mem::clear(&curr.memory, size);
	}
	
	return &curr.memory;
}

<*
 Tries to find and resize allocation made as a fallback.

 @require old_ptr != null
 @require new_size > 0
 @return ? mem::INVALID_ALLOC_SIZE, mem::OUT_OF_MEMORY
*>
fn void*? resize_fallback_allocation(BuddyAlloc* self, void* old_ptr, uint new_size) @local
{
	FallbackAlloc* curr = self.fallback_head;
	FallbackAlloc* alloc = null;
	
	while (curr)
	{
		if (curr == old_ptr)
		{
			alloc = curr;
			break;
		}
		curr = curr.next;
	}

	assert(alloc != null, "Pointer originates from a different allocator: %p, not in %p - %p", old_ptr, self.head, (char*)self.head + capacity(self));

	alloc = alloc::realloc_try(self.backing_alloc, alloc, FallbackAlloc::size + new_size)!;
	alloc.size = new_size;
	
	return &alloc.memory;
}

<*
 Cleanup all fallback allocations.
*>
fn void free_fallback_allocations(BuddyAlloc* self) @local
{
	FallbackAlloc* curr = self.fallback_head;
	while (curr)
	{
		FallbackAlloc* next = curr.next; 
		alloc::free(self.backing_alloc, curr);
		curr = next;
	}
	self.fallback_head = null;
}

<*
 Calculates all memory that were allocated as a fallback.
*>
fn uint calc_used_fallback_mem(BuddyAlloc* self) @local
{
	uint res = 0;
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
fn sz? BuddyAlloc.to_format(&self, Formatter* fmt) @dynamic
{
	BuddyBlock* block = self.head;
	BuddyBlock* buddy = next_block(block);
	sz total = fmt.print("Buddy Allocator state:")!;
	uint used = 0;
	uint fallback_used = calc_used_fallback_mem(self); 
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
