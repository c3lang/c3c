module std::core::mem::mempool;
import std::core::mem, std::core::mem::allocator, std::math;

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
	usz used;
	bool initialized;
}

<*
 Initialize an block pool

 @param [in] allocator : "The allocator to use"
 @param $Type : "The type used for setting the block size"
 @param capacity : "The amount of blocks to be pre-allocated"
 @require !self.initialized : "The block pool must not be initialized"
*>
macro void FixedBlockPool.init(&self, Allocator allocator, $Type, usz capacity = INITIAL_CAPACITY)
{
	self.allocator = allocator;
	self.tail = &self.head;
	self.head.next = null;
	self.block_size = math::max($Type.sizeof, FixedBlockPoolEntry.sizeof);

	capacity = capacity ?: mem::os_pagesize() / self.block_size;
	self.head.buffer = allocator::calloc(allocator, capacity * self.block_size);
	self.head.capacity = capacity;
	self.next_free = self.head.buffer;
	self.freelist = null;
	self.grow_capacity = capacity;
	self.initialized = true;
	self.allocated = capacity;
	self.used = 0;
}

<*
 Initialize an block pool using Temporary allocator

 @require !self.initialized : "The block pool must not be initialized"
 @param capacity : "The amount of blocks to be pre-allocated"
*>
macro void FixedBlockPool.tinit(&self, $Type, usz capacity = INITIAL_CAPACITY) => self.init(tmem, $Type, capacity);

<*
 Free up the entire block pool

 @require self.initialized : "The block pool must be initialized"
*>
fn void FixedBlockPool.free(&self)
{
	allocator::free(self.allocator, self.head.buffer);
	FixedBlockPoolNode* iter = self.head.next;

	while (iter)
	{
		allocator::free(self.allocator, iter.buffer);
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
		self.freelist = entry.previous;
		mem::clear(entry, self.block_size);
		return entry;
	}

	void* end = self.tail.buffer + (self.tail.capacity * self.block_size);
	if (self.next_free >= end) self.new_node();
	void* ptr = self.next_free;
	self.next_free += self.block_size;

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
		if (self.block_size > FixedBlockPoolEntry.sizeof)
		{
			mem::set(ptr + FixedBlockPoolEntry.sizeof, 0xAA, self.block_size);
		}
	$else
		// POINT FOR IMPROVEMENT, something like:
		// asan::poison_memory_region(&ptr, self.block_size);
	$endif

	FixedBlockPoolEntry* entry = ptr;
	entry.previous = self.freelist;
	self.freelist = entry;
	self.used--;
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
	node.buffer = allocator::calloc(self.allocator, self.grow_capacity * self.block_size);
	node.capacity = self.grow_capacity;
	self.tail.next = node;
	self.tail = node;
	self.next_free = node.buffer;
	self.allocated += node.capacity;
}
