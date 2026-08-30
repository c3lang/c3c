module std::core::mem::mempool;
import std::core::mem, std::core::mem::allocator, std::math;
import std::core::sanitizer::asan;

<* Sentinel `capacity` value meaning "pick a capacity automatically from the OS page size". *>
const INITIAL_CAPACITY = 0;

<* One page of blocks backing a `FixedBlockPool`, chained together as the pool grows. *>
struct FixedBlockPoolNode
{
	void* buffer;
	FixedBlockPoolNode *next;
	sz capacity;
}

<*
 Overlay written into a deallocated block to thread it onto the pool's freelist. Only valid for
 blocks currently on the freelist — this is why `FixedBlockPool.init` always ensures
 `block_size >= FixedBlockPoolEntry::size`.
*>
struct FixedBlockPoolEntry
{
	void *previous;
}

<*
 Fixed blocks pool pre-allocating blocks backed by an Allocator which are then reserved for the user,
 blocks deallocated by the user are later re-used by future blocks allocations

 `grow_capacity` can be changed in order to affect how many blocks will be allocated by next pool allocation,
 it has to be greater than 0
 `allocated` number of allocated blocks
 `used` number of used blocks by the user
*>
struct FixedBlockPool
{
	Allocator allocator;
	FixedBlockPoolNode head;
	FixedBlockPoolNode *tail;
	void *next_free;
	void *freelist;
	sz block_size;
	sz grow_capacity;
	sz allocated;
	sz alignment;
	sz used;
	bool initialized;
}

<*
 Initialize an block pool

 @param [in] allocator : "The allocator to use"
 @param block_size : "The block size to use"
 @param capacity : "The amount of blocks to be pre-allocated"
 @param alignment : "The alignment each returned block must have beyond what it naturally has, or 0 for none"
 @return "self"

 @require [alignment] !alignment || math::is_power_of_2(alignment)
 @require [alignment] alignment <= mem::MAX_MEMORY_ALIGNMENT : `alignment too big`
 @require !self.initialized : "The block pool must not be initialized"
 @require [block_size] block_size > 0 : "Block size must be non zero"
*>
fn FixedBlockPool* FixedBlockPool.init(&self, Allocator allocator, sz block_size, sz capacity = INITIAL_CAPACITY, sz alignment = 0)
{
	self.allocator = allocator;
	self.tail = &self.head;
	self.head.next = null;
	self.alignment = allocators::alignment_for_allocation(alignment);
	if (block_size < FixedBlockPoolEntry::size) block_size = FixedBlockPoolEntry::size;
	if (alignment) block_size = mem::aligned_offset(block_size, alignment);
	self.block_size = block_size;
	capacity = calculate_actual_capacity(capacity, self.block_size);
    assert(capacity * self.block_size >= self.block_size, "Total memory would overflow %d %d", block_size, capacity);
	self.grow_capacity = capacity;
	fixedblockpool_allocate_page(self, &self.head);
	self.next_free = self.head.buffer;
	self.freelist = null;
	self.initialized = true;
	self.allocated = capacity;
	self.used = 0;
	return self;
}

<*
 Initialize an block pool

 @param [in] allocator : "The allocator to use"
 @param $Type : "The type used for setting the block size"
 @param capacity : "The amount of blocks to be pre-allocated"
 @return "self"
 @require !self.initialized : "The block pool must not be initialized"
*>
macro FixedBlockPool* FixedBlockPool.init_for_type(&self, Allocator allocator, $Type, sz capacity = INITIAL_CAPACITY)
{
	return self.init(allocator, $Type::size, capacity, $Type::alignment);
}

<*
 Initialize an block pool using Temporary allocator

 @param $Type : "The type used for setting the block size"
 @param capacity : "The amount of blocks to be pre-allocated"
 @return "self"
 @require !self.initialized : "The block pool must not be initialized"
*>
macro FixedBlockPool* FixedBlockPool.tinit_for_type(&self, $Type, sz capacity = INITIAL_CAPACITY) => self.init_for_type(tmem, $Type, capacity);

<*
 Initialize an block pool using Temporary allocator

 @param block_size : "The block size to use"
 @param capacity : "The amount of blocks to be pre-allocated"
 @return "self"
 @require !self.initialized : "The block pool must not be initialized"
*>
macro FixedBlockPool* FixedBlockPool.tinit(&self, sz block_size, sz capacity = INITIAL_CAPACITY) => self.init(tmem, block_size, capacity);

<*
 Free up the entire block pool

 @require self.initialized : "The block pool must be initialized"
*>
fn void FixedBlockPool.free(&self)
{
	$if env::COMPILER_SAFE_MODE && $feat(ASAN):
		asan::unpoison_memory_region(self.head.buffer, self.head.capacity * self.block_size);
	$endif
	fixedblockpool_free_page(self, self.head.buffer);
	FixedBlockPoolNode* iter = self.head.next;

	while (iter)
	{
		$if env::COMPILER_SAFE_MODE && $feat(ASAN):
			asan::unpoison_memory_region(iter.buffer, iter.capacity * self.block_size);
		$endif
		fixedblockpool_free_page(self, iter.buffer);
		FixedBlockPoolNode* current = iter;
		iter = iter.next;
		alloc::free(self.allocator, current);
	}
	self.initialized = false;
	self.allocated = 0;
	self.used = 0;
}

<*
 Allocate an block on the block pool, re-uses previously deallocated blocks

 @return "A pointer to a zero-initialized block of `self.block_size` bytes"
 @require self.initialized : "The block pool must be initialized"
*>
fn void* FixedBlockPool.alloc(&self)
{
	defer self.used++;

	if (self.freelist)
	{
		FixedBlockPoolEntry* entry = self.freelist;
		$if env::COMPILER_SAFE_MODE && $feat(ASAN):
			asan::unpoison_memory_region(entry, self.block_size);
		$endif
		self.freelist = entry.previous;
		mem::clear(entry, self.block_size);
		return entry;
	}

	void* end = self.tail.buffer + (self.tail.capacity * self.block_size);
	if (self.next_free >= end) fixedblockpool_new_node(self);
	void* ptr = self.next_free;
	self.next_free += self.block_size;
	$if env::COMPILER_SAFE_MODE && $feat(ASAN):
		asan::unpoison_memory_region(ptr, self.block_size);
	$endif

	return ptr;
}

<*
 Deallocate a block from the block pool

 @param [&drop] ptr : "The block to deallocate, previously returned by `alloc`"
 @require self.initialized : "The block pool must be initialized"
 @require [ptr] fixedblockpool_check_ptr(self, ptr) : "The pointer should be part of the pool"
*>
fn void FixedBlockPool.dealloc(&self, void* ptr)
{
	$if env::COMPILER_SAFE_MODE && !$feat(ASAN):
		mem::set(ptr, 0xAA, self.block_size);
	$endif

	FixedBlockPoolEntry* entry = ptr;
	entry.previous = self.freelist;
	self.freelist = entry;
	self.used--;

	$if env::COMPILER_SAFE_MODE && $feat(ASAN):
		asan::poison_memory_region(ptr, self.block_size);
	$endif
}

<*
 @param ptr : "The pointer to validate"
 @return "true if ptr could have come from `alloc`"
 @require self.initialized : "The block pool must be initialized"
*>
fn bool fixedblockpool_check_ptr(FixedBlockPool* self, void *ptr) @local
{
	FixedBlockPoolNode* iter = &self.head;

	while (iter)
	{
		void* end = iter.buffer + (iter.capacity * self.block_size);
		// We found the block, return true if it's aligned properly
		if (ptr >= iter.buffer && ptr < end) return !((ptr - iter.buffer) % self.block_size);
		iter = iter.next;
	}

	return false;
}

<*
 Grow the pool by allocating and linking in one more page of `self.grow_capacity` blocks.

 @require self.grow_capacity > 0 : "How many blocks will it store"
*>
fn void fixedblockpool_new_node(FixedBlockPool* self) @local
{
	FixedBlockPoolNode* node = alloc::new(self.allocator, FixedBlockPoolNode);
	fixedblockpool_allocate_page(self, node);
	self.tail.next = node;
	self.tail = node;
	self.next_free = node.buffer;
	self.allocated += node.capacity;
}

<*
 Allocate one page, aligned to `self.alignment` if it exceeds the
 default memory alignment.

 @param [&in] self
 @param [&out] node : "The node to allocate for"
*>
macro void fixedblockpool_allocate_page(FixedBlockPool* self, FixedBlockPoolNode *node) @private
{
	sz page_size = self.block_size * (node.capacity = self.grow_capacity);
	void *mem = node.buffer = self.alignment > mem::DEFAULT_MEM_ALIGNMENT
	       ? alloc::calloc_aligned(self.allocator, page_size, self.alignment)!!
           : alloc::calloc(self.allocator, page_size);
	$if env::COMPILER_SAFE_MODE && $feat(ASAN):
		asan::poison_memory_region(mem, page_size);
	$endif
}

<*
 Release one page previously returned by `fixedblockpool_allocate_page`.

 @param [&drop] page : "The page to release"
*>
macro void fixedblockpool_free_page(FixedBlockPool* self, void* page) @private
{
	if (self.alignment > mem::DEFAULT_MEM_ALIGNMENT)
	{
		alloc::free_aligned(self.allocator, page);
	}
	else
	{
		alloc::free(self.allocator, page);
	}
}


<*
 @param capacity : "The requested capacity, or 0 to derive one from the OS page size"
 @param block_size : "The (already bumped) block size"
 @return "The actual number of blocks to allocate"
*>
macro sz calculate_actual_capacity(sz capacity, sz block_size) @private
{
	// Assume some overhead
	if (capacity) return capacity;
	capacity = (mem::os_pagesize() - 128) / block_size;
	return capacity ?: 1;
}
