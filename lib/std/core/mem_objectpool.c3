module std::core::mem::objectpool;
import std::core::mem, std::core::mem::allocator;

const INITIAL_CAPACITY = 0;

struct SingleSizeObjectPoolNode
{
	void *buffer;
	SingleSizeObjectPoolNode *next;
	usz capacity;
}

union SingleSizeObjectPoolEntry
{
	void *previous;
}

<*
 Object pool pre-allocating objects backed by an Allocator which are then reserved for the user,
 objects deallocated by the user are later re-used by future object allocations

 `grow_capacity` can be changed in order to affect how many objects will be allocated by next pool allocation,
 it has to be greater than 0
 `allocated` number of allocated objects
 `used` number of used objects by the user
*>
struct SingleSizeObjectPool
{
	Allocator allocator;
	SingleSizeObjectPoolNode head;
	SingleSizeObjectPoolNode *tail;
	void *next_free;
	void *freelist;
	usz object_size, grow_capacity;
	usz allocated, used;
	bool initialized;
}

<*
 Initialize an object pool

 @param [in] allocator : "The allocator to use"
 @param $Type : "The type used for setting the object size"
 @param capacity : "The amount of objects to be pre-allocated"
 @require !self.initialized : "The object pool must not be initialized"
*>
macro void SingleSizeObjectPool.init(&self, Allocator allocator, $Type, usz capacity = INITIAL_CAPACITY)
{
	self.allocator = allocator;
	self.tail = &self.head;
	self.head.next = null;
	self.object_size = $Type.sizeof >= (void*).sizeof ? $Type.sizeof : (void*).sizeof;

	capacity = capacity ?: mem::os_pagesize() / self.object_size;
	self.head.buffer = allocator::calloc(allocator, capacity * self.object_size);
	self.head.capacity = capacity;
	self.next_free = self.head.buffer;
	self.freelist = null;
	self.grow_capacity = capacity;
	self.initialized = true;
	self.allocated = capacity;
	self.used = 0;
}

<*
 Initialize an object pool using Temporary allocator

 @require !self.initialized : "The object pool must not be initialized"
 @param capacity : "The amount of objects to be pre-allocated"
*>
macro void SingleSizeObjectPool.tinit(&self, $Type, usz capacity = INITIAL_CAPACITY) => self.init(tmem, $Type, capacity);

<*
 Free up the entire object pool

 @require self.initialized : "The object pool must be initialized"
*>
fn void SingleSizeObjectPool.free(&self)
{
	allocator::free(self.allocator, self.head.buffer);
	SingleSizeObjectPoolNode *iter = self.head.next;

	while (iter != null)
	{
		allocator::free(self.allocator, iter.buffer);
		SingleSizeObjectPoolNode *current = iter;
		iter = iter.next;
		allocator::free(self.allocator, current);
	}
	self.initialized = false;
	self.allocated = 0;
	self.used = 0;
}

<*
 Allocate an object on the object pool, re-uses previously deallocated objects

 @require self.initialized : "The object pool must be initialized"
*>
fn void *SingleSizeObjectPool.alloc(&self)
{
	defer self.used++;

	if (self.freelist != null)
	{
		SingleSizeObjectPoolEntry *entry = self.freelist;
		self.freelist = entry.previous;
		mem::set(entry, 0, self.object_size);
		return entry;
	}

	void *end = self.tail.buffer + (self.tail.capacity * self.object_size);
	if (self.next_free >= end) self.new_node();
	void *ptr = self.next_free;
	self.next_free += self.object_size;

	return ptr;
}

<*
 Deallocate an object from the object pool

 @require self.initialized : "The object pool must be initialized"
 @require self.check_ptr(ptr) : "The pointer should be part of the pool"
*>
fn void SingleSizeObjectPool.dealloc(&self, void *ptr)
{
	SingleSizeObjectPoolEntry *entry = ptr;
	entry.previous = self.freelist;
	self.freelist = entry;
	self.used--;
}

<*
 @require self.initialized : "The object pool must be initialized"
*>
fn bool SingleSizeObjectPool.check_ptr(&self, void *ptr) @local
{
	SingleSizeObjectPoolNode *iter = &self.head;

	while (iter != null)
	{
		void *end = iter.buffer + (iter.capacity * self.object_size);
		if (ptr >= iter.buffer && ptr < end) return true;
		iter = iter.next;
	}

	return false;
}

<*
 @require self.grow_capacity > 0 : "How many objects will it store"
*>
fn void SingleSizeObjectPool.new_node(&self) @local
{
	SingleSizeObjectPoolNode *node = allocator::new(self.allocator, SingleSizeObjectPoolNode);
	node.buffer = allocator::calloc(self.allocator, self.grow_capacity * self.object_size);
	node.capacity = self.grow_capacity;
	self.tail.next = node;
	self.tail = node;
	self.next_free = node.buffer;
	self.allocated += node.capacity;
}
