module std::core::mem::mempool;
import std::core::mem, std::core::mem::allocator, std::math;
import std::core::sanitizer::asan;

const INITIAL_CAPACITY = 0;

struct FixedBlockPoolNode
{
	void* buffer;
	FixedBlockPoolNode *next;
	usz capacity;
}

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
	usz block_size;
	usz grow_capacity;
	usz allocated;
	usz page_size;
	usz alignment;
	usz used;
	bool initialized;
}

<*
 Initialize an block pool

 @param [in] allocator : "The allocator to use"
 @param block_size : "The block size to use"
 @param capacity : "The amount of blocks to be pre-allocated"
 @param alignment : "The alignment of the buffer"
 @require !alignment || math::is_power_of_2(alignment)
 @require !self.initialized : "The block pool must not be initialized"
 @require block_size > 0 : "Block size must be non zero"
 @require calculate_actual_capacity(capacity, block_size) * block_size >= block_size
    : "Total memory would overflow"
*>
macro FixedBlockPool* FixedBlockPool.init(&self, Allocator allocator, usz block_size, usz capacity = INITIAL_CAPACITY, usz alignment = 0)
{
	self.allocator = allocator;
	self.tail = &self.head;
	self.head.next = null;
	self.block_size = math::max(block_size, FixedBlockPoolEntry.sizeof);
	capacity = calculate_actual_capacity(capacity, self.block_size);
	self.alignment = allocator::alignment_for_allocation(alignment);
	self.page_size = capacity * self.block_size;
    assert(self.page_size >= self.block_size, "Total memory would overflow %d %d", block_size, capacity);
	self.head.buffer = self.allocate_page();
	$if env::COMPILER_SAFE_MODE && env::ADDRESS_SANITIZER:
		asan::poison_memory_region(self.head.buffer, self.page_size);
	$endif
	self.head.capacity = capacity;
	self.next_free = self.head.buffer;
	self.freelist = null;
	self.grow_capacity = capacity;
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
 @require !self.initialized : "The block pool must not be initialized"
*>
macro FixedBlockPool* FixedBlockPool.init_for_type(&self, Allocator allocator, $Type, usz capacity = INITIAL_CAPACITY)
{
	return self.init(allocator, $Type.sizeof, capacity, $Type.alignof);
}

<*
 Initialize an block pool using Temporary allocator

 @param $Type : "The type used for setting the block size"
 @param capacity : "The amount of blocks to be pre-allocated"
 @require !self.initialized : "The block pool must not be initialized"
*>
macro FixedBlockPool* FixedBlockPool.tinit_for_type(&self, $Type, usz capacity = INITIAL_CAPACITY) => self.init_for_type(tmem, $Type, capacity);

<*
 Initialize an block pool using Temporary allocator

 @param block_size : "The block size to use"
 @param capacity : "The amount of blocks to be pre-allocated"
 @require !self.initialized : "The block pool must not be initialized"
*>
macro FixedBlockPool* FixedBlockPool.tinit(&self, usz block_size, usz capacity = INITIAL_CAPACITY) => self.init(tmem, block_size, capacity);

<*
 Free up the entire block pool

 @require self.initialized : "The block pool must be initialized"
*>
fn void FixedBlockPool.free(&self)
{
	$if env::COMPILER_SAFE_MODE && env::ADDRESS_SANITIZER:
		asan::unpoison_memory_region(self.head.buffer, self.page_size);
	$endif
	self.free_page(self.head.buffer);
	FixedBlockPoolNode* iter = self.head.next;

	while (iter)
	{
		$if env::COMPILER_SAFE_MODE && env::ADDRESS_SANITIZER:
			asan::unpoison_memory_region(iter.buffer, self.page_size);
		$endif
		self.free_page(iter.buffer);
		FixedBlockPoolNode* current = iter;
		iter = iter.next;
		allocator::free(self.allocator, current);
	}
	self.initialized = false;
	self.allocated = 0;
	self.used = 0;
}

<*
 Allocate an block on the block pool, re-uses previously deallocated blocks

 @require self.initialized : "The block pool must be initialized"
*>
fn void* FixedBlockPool.alloc(&self)
{
	defer self.used++;

	if (self.freelist)
	{
		FixedBlockPoolEntry* entry = self.freelist;
		$if env::COMPILER_SAFE_MODE && env::ADDRESS_SANITIZER:
			asan::unpoison_memory_region(entry, self.block_size);
		$endif
		self.freelist = entry.previous;
		mem::clear(entry, self.block_size);
		return entry;
	}

	void* end = self.tail.buffer + (self.tail.capacity * self.block_size);
	if (self.next_free >= end) self.new_node();
	void* ptr = self.next_free;
	self.next_free += self.block_size;
	$if env::COMPILER_SAFE_MODE && env::ADDRESS_SANITIZER:
		asan::unpoison_memory_region(ptr, self.block_size);
	$endif

	return ptr;
}

<*
 Deallocate a block from the block pool

 @require self.initialized : "The block pool must be initialized"
 @require self.check_ptr(ptr) : "The pointer should be part of the pool"
*>
fn void FixedBlockPool.dealloc(&self, void* ptr)
{
	$if env::COMPILER_SAFE_MODE && !env::ADDRESS_SANITIZER:
		mem::set(ptr, 0xAA, self.block_size);
	$endif

	FixedBlockPoolEntry* entry = ptr;
	entry.previous = self.freelist;
	self.freelist = entry;
	self.used--;

	$if env::COMPILER_SAFE_MODE && env::ADDRESS_SANITIZER:
		asan::poison_memory_region(ptr, self.block_size);
	$endif
}

<*
 @require self.initialized : "The block pool must be initialized"
*>
fn bool FixedBlockPool.check_ptr(&self, void *ptr) @local
{
	FixedBlockPoolNode* iter = &self.head;

	while (iter)
	{
		void* end = iter.buffer + (iter.capacity * self.block_size);
		if (ptr >= iter.buffer && ptr < end) return true;
		iter = iter.next;
	}

	return false;
}

<*
 @require self.grow_capacity > 0 : "How many blocks will it store"
*>
fn void FixedBlockPool.new_node(&self) @local
{
	FixedBlockPoolNode* node = allocator::new(self.allocator, FixedBlockPoolNode);
	node.buffer = self.allocate_page();
	$if env::COMPILER_SAFE_MODE && env::ADDRESS_SANITIZER:
		asan::poison_memory_region(node.buffer, self.page_size);
	$endif
	node.capacity = self.grow_capacity;
	self.tail.next = node;
	self.tail = node;
	self.next_free = node.buffer;
	self.allocated += node.capacity;
}

macro void* FixedBlockPool.allocate_page(&self) @private
{
	return self.alignment > mem::DEFAULT_MEM_ALIGNMENT
	       ? allocator::calloc_aligned(self.allocator, self.page_size, self.alignment)!!
           : allocator::calloc(self.allocator, self.page_size);
}

macro void FixedBlockPool.free_page(&self, void* page) @private
{
	if (self.alignment > mem::DEFAULT_MEM_ALIGNMENT)
	{
		allocator::free_aligned(self.allocator, page);
	}
	else
	{
		allocator::free(self.allocator, page);
	}
}

macro usz calculate_actual_capacity(usz capacity, usz block_size) @private
{
	// Assume some overhead
	if (capacity) return capacity;
	capacity = (mem::os_pagesize() - 128) / block_size;
	return capacity ?: 1;
}
