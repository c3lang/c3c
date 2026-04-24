module free_list_alloc;
import std::io;
import std::math;

const DEFAULT_CAPACITY	   = 4096;
const MIN_BLOCK_SIZE       = mem::DEFAULT_MEM_ALIGNMENT + FreeNode::size;
const MIN_CAPACITY         = MIN_BLOCK_SIZE * 4;

<*
 Free list allocator does not sort the underlying memory and does not require allocations to be powers of two. It supports deallocation in arbitrary order.
 After a deallocation, it performs coalescing of adjacent nodes when possible.
 The user can choose between two strategies: best fit and first fit. Best fit minimizes memory fragmentation but is generally slower than the first fit strategy.
 The allocator uses a binary search tree as its underlying data structure to enable efficient node lookup.
 When the allocator exhausts its memory or fragmentation becomes too high, it falls back to its backing allocator. Fallback allocations are automatically freed when the allocator is destroyed; the user must not free them manually.
 Fallback allocations can also be resized.
*>
struct FreeListAlloc (Allocator, Printable)
{
	char* data;
	sz capacity;
	sz used;
	FreeNode* root;
	FreeNode* tail;
	FallbackAlloc* fallback_root;
	AllocStrategy strategy;
	Allocator backing_alloc;
}

bitstruct NodeState : ulong @local
{
	// Aligned size of memory block + AllocHeader.
	sz size : 0..(sz::size * 8 - 2);
	bool is_free : (sz::size * 8 - 1)..(sz::size * 8 - 1);
}

struct FreeNode @local
{
 	// Embeds 'is_free' inside of 'size' to reduce memory usage.
	inline NodeState _state;
	FreeNode* prev_phys;
	// Overwritten on allocation.
	FreeNode* left;
	FreeNode* right;
	// Store parent to perform deletions in constant time.
	FreeNode* parent;
}

struct AllocHeader @local
{
 	// Embeds 'is_free' inside of 'size' to reduce memory usage.
	inline NodeState _state;
	FreeNode* prev_phys;
}

enum AllocStrategy @local
{
	BEST_FIT,
	FIRST_FIT,
}

struct FallbackAlloc @local
{
	FallbackAlloc* next;
	sz size;
	char[*] memory;
}

<*
 Initializes free list allocator structure.

 @require init_capacity > MIN_CAPACITY : "Capacity should be greater than minimum capacity"
*>
fn void FreeListAlloc.init(
	&self,
	Allocator backing_alloc = mem,
	sz init_capacity = DEFAULT_CAPACITY,
	AllocStrategy strategy = BEST_FIT,
)
{
	self.capacity = mem::aligned_offset(init_capacity, mem::DEFAULT_MEM_ALIGNMENT);
	self.data = (char*)alloc::alloc_with_padding(backing_alloc, AllocHeader, self.capacity)!!;
	self.backing_alloc = backing_alloc;
	self.strategy = strategy;
	self.tail = (FreeNode*)(self.data + self.capacity);
	self.reset();
}

<*
 Implements the Allocator interface method.

 @require !alignment || math::is_power_of_2(alignment)
 @require alignment <= mem::MAX_MEMORY_ALIGNMENT : `alignment too big`
 @require size > 0
*>
fn void*? FreeListAlloc.acquire(&self, sz size, AllocInitType init_type, sz alignment) @dynamic
{
	alignment = allocators::alignment_for_allocation(alignment);
	sz aligned_size = mem::aligned_offset(size + AllocHeader::size, alignment);

	if (aligned_size > self.remain_space())
	{
		return make_fallback_allocation(self, aligned_size, init_type);
	}

	FreeNode* node = null;

	if (self.strategy == BEST_FIT)
	{
		node = find_node_best_fit(self, aligned_size);
	}
	else
	{
		node = find_node_first_fit(self, aligned_size);
	}

	if (!node)
	{
		return make_fallback_allocation(self, aligned_size, init_type);
	}

	remove_node(&self.root, node);

	// Try to insert a new node, if have enough space.
	sz diff = node.size - aligned_size;
	if (@likely(diff >= MIN_BLOCK_SIZE))
	{
		FreeNode* new_node = node_at_offset(node, aligned_size);
		init_node(new_node, diff, node);
		insert_node(&self.root, new_node);
		node.size = aligned_size;
		self.used += aligned_size;
	}
	// Just fill the available space to prevent errors;
	// Otherwise next_block() would give uninitialized memory block.
	else
	{
		node.size += diff;
		self.used += node.size;
	}

	if (init_type == ZERO)
	{
		mem::clear((AllocHeader*)node + 1, aligned_size - AllocHeader::size);
	}

	return (void*)((AllocHeader*)node + 1);
}

<*
 Implements the Allocator interface method.

 @require !alignment || math::is_power_of_2(alignment)
 @require alignment <= mem::MAX_MEMORY_ALIGNMENT : `alignment too big`
 @require old_pointer != null
 @require size > 0
 @return ? mem::INVALID_ALLOC_SIZE, mem::OUT_OF_MEMORY
*>
fn void*? FreeListAlloc.resize(&self, void* old_pointer, sz size, sz alignment) @dynamic
{
	if (@unlikely(!owns_pointer(self, old_pointer)))
	{
		return resize_fallback_allocation(self, old_pointer, size);
	}

	FreeNode* node = (FreeNode*)((AllocHeader*)old_pointer - 1);
	sz old_size = node.size;
	alignment = allocators::alignment_for_allocation(alignment);
	sz new_size = mem::aligned_offset(size + AllocHeader::size, alignment);

	// Shrink.
	if (@unlikely(new_size <= old_size))
	{
		sz diff = old_size - new_size;
		node.size -= diff;
		self.used -= diff;
		if (diff >= MIN_BLOCK_SIZE)
		{
			FreeNode* new_node = node_next(node);
			init_node(new_node, diff, node);
			insert_node(&self.root, new_node);
		}
		return old_pointer;
	}

	FreeNode* next = node_next(node);

	// Try to extend.
	if (next < self.tail && next.is_free)
	{
		sz diff = mem::aligned_offset(new_size - old_size, alignment);
		sz new_node_size = next.size - diff;
		FreeNode* new_node = null;
		// Insert a new node if have enough space.
		if (new_node_size >= MIN_BLOCK_SIZE)
		{
			node.size += diff;
			self.used += diff;
			new_node = node_next(node);
			init_node(new_node, new_node_size, node);
			remove_node(&self.root, next);
			insert_node(&self.root, new_node);
		}
		// Or just consume all space of the next node.
		else
		{
			node.size += next.size;
			self.used += next.size;
			remove_node(&self.root, next);
		}
		return old_pointer;
	}

	// Allocate a new one.
	void* new_allocation = self.acquire(size, NO_ZERO, alignment)!;
	mem::copy(new_allocation, old_pointer, old_size - AllocHeader::size);
	self.release(old_pointer, false);

	return new_allocation;
}

<*
 Implements the Allocator interface method.

 @require ptr != null : "Shouldn't be null"
*>
fn void FreeListAlloc.release(&self, void* ptr, bool) @dynamic
{
	if (!owns_pointer(self, ptr)) return;
	FreeNode* free_node = (FreeNode*)((AllocHeader*)ptr - 1);
	// Prevent double free.
	if (@unlikely(free_node.is_free)) return;

	self.used -= free_node.size;
	// Order of this lines should be preserved.
	free_node = coalesce_nodes_around(self, free_node);
	insert_node(&self.root, free_node);
}

<*
 Frees resources of the allocator.
*>
fn void FreeListAlloc.free(&self)
{
	if (@likely(self.data != null))
	{
		alloc::free(self.backing_alloc, self.data);
	}
	if (self.fallback_root)
	{
		free_fallback_allocations(self);
	}
	*self = { };
}

<*
 Resets the state of the allocator making it available for reuse.
*>
fn void FreeListAlloc.reset(&self)
{
	self.root = (FreeNode*)self.data;
	init_node(self.root, self.capacity, null);
	self.used = 0;
}

<*
 Calculates available memory space.
*>
fn sz FreeListAlloc.remain_space(&self) @inline
{
	return self.capacity - self.used;
}

<*
 Implements 'Printable' interface.
*>
fn sz? FreeListAlloc.to_format(&self, Formatter* fmt) @dynamic
{
	sz fallback_used = calc_used_fallback_mem(self);
	String fmt_string = "FreeList allocator state:\n\tmemory total = %.0fB / %.2fKiB / %.3fMiB / %.4fGiB\n\tmemory used  = %.0fB / %.2fKiB / %.3fMiB / %.4fGiB\n\textra allocated = %.0fB / %.2fKiB / %.3fMiB / %.4fGiB";

	if (self.capacity > self.used)
	{
		return fmt.printf(
			fmt_string,
			(float)self.capacity,
			self.capacity / (float)mem::KB,
			self.capacity / (float)mem::MB,
			self.capacity / (float)mem::GB,
			(float)self.used,
			self.used / (float)mem::KB,
			self.used / (float)mem::MB,
			self.used / (float)mem::GB,
			(float)fallback_used,
			fallback_used / (float)mem::KB,
			fallback_used / (float)mem::MB,
			fallback_used / (float)mem::GB,
		);
	}
	else
	{
		float bytes = (float)self.capacity;
		float kib = self.capacity / (float)mem::KB;
		float mib = self.capacity / (float)mem::MB;
		float gib = self.capacity / (float)mem::GB;

		return fmt.printf(
			fmt_string,
			bytes,
			kib,
			mib,
			gib,
			bytes,
			kib,
			mib,
			gib,
			(float)fallback_used,
			fallback_used / (float)mem::KB,
			fallback_used / (float)mem::MB,
			fallback_used / (float)mem::GB,
		);
	}
}

// ------ Private API ------

<*
 Tries to find node based on best fit strategy.
 (node with minimal required space is returned).
*>
fn FreeNode* find_node_best_fit(FreeListAlloc* self, sz req_size) @local
{
	if (!self.root) return null;

	FreeNode* it_curr = self.root;
	FreeNode* best_node = null;

	while (it_curr && it_curr < self.tail)
	{
		if (it_curr.size >= req_size)
		{
			best_node = it_curr;
			it_curr = it_curr.left;
		}
		else
		{
			it_curr = it_curr.right;
		}
	}

	return best_node;
}

<*
 Tries to find node based on first fit strategy.
 (first appropriate node is returned).
*>
fn FreeNode* find_node_first_fit(FreeListAlloc* self, sz req_size) @local
{
	if (!self.root) return null;

	FreeNode* curr = self.root;

	while (curr && curr < self.tail)
	{
		if (curr.size >= req_size)
		{
			return curr;
		}
		else
		{
			curr = curr.right;
		}
	}

	return null;
}

<*
 Initializes new node and sets its state to 'free'.
*>
fn void init_node(FreeNode* node, sz size, FreeNode* prev_phys) @local
{
	*node = { };
	node.is_free = true;
	node.size = size;
	node.prev_phys = prev_phys;
}

<*
 BST insertion.
 Inserts the node into the list of free nodes, making it available for future allocations.
*>
fn void insert_node(FreeNode** root, FreeNode* new_node) @local
{
	new_node.is_free = true;

	if (!*root)
	{
		*root = new_node;
		return;
	}

	FreeNode* curr = *root;

	while (curr)
	{
		if (new_node.size >= curr.size)
		{
			if (curr.right)
			{
				curr = curr.right;
			}
			else
			{
				curr.right = new_node;
				new_node.parent = curr;
				return;
			}
		}
		else
		{
			if (curr.left)
			{
				curr = curr.left;
			}
			else
			{
				curr.left = new_node;
				new_node.parent = curr;
				return;
			}
		}
	}
}

<*
 BST removal.
 Removes the node from the list of free nodes, so its memory can be provided to the user.

 @require node_to_remove != null : "Shoudn't be null"
*>
fn void remove_node(FreeNode** root, FreeNode* node_to_remove) @local
{
	node_to_remove.is_free = false;

	if (*root == node_to_remove)
	{
		*root = null;
		return;
	}

	FreeNode* parent = node_to_remove.parent;

	if (node_to_remove == parent.left)
	{
		parent.left = null;
	}
	else
	{
		parent.right = null;
	}
}

<*
 Tries to coalesce adjacent nodes: (prev) -> (node) -> (next).

 @return "Returns node that was possibly moved to new address"
 @require node != null : "Shoudn't be null"
*>
fn FreeNode* coalesce_nodes_around(FreeListAlloc* self, FreeNode* node) @local
{
	FreeNode* next = node_next(node);
	bool was_merge = false;

	if (next < self.tail && next.is_free)
	{
		node.size += next.size;
		remove_node(&self.root, next);
		was_merge = true;
	}

	FreeNode* prev = node.prev_phys;
	if (prev && prev.is_free)
	{
		prev.size += node.size;
		node = prev;
		remove_node(&self.root, prev);
		was_merge = true;
	}

	if (was_merge)
	{
		next = node_next(node);
		if (next < self.tail)
		{
			next.prev_phys = node;
		}
	}

	return node;
}

<*
 Checks if provided pointer belongs to the free list allocator.

 @require ptr != null : "Shoudn't be null"
*>
fn bool owns_pointer(FreeListAlloc* self, void* ptr) @inline @local
{
	return ptr >= self.data && ptr < (self.data + self.capacity);
}

<*
 Calculates position of the next node relatively to provided one, and returns it.

 @require node != null : "Shoudn't be null"
*>
fn FreeNode* node_next(FreeNode* node) @inline @local
{
	return (FreeNode*)((char*)node + node.size);
}

<*
 Calculates position of the node relatively to provided one with provided offset, and returns it.

 @require node != null : "Shoudn't be null"
 @require offset > 0 : "Should be bigger than 0"
*>
fn FreeNode* node_at_offset(FreeNode* node, sz offset) @inline @local
{
	return (FreeNode*)((char*)node + offset);
}

<*
 Allocates to the separate linked list in case there's not enough space in the FreeList allocator main buffer.

 @require size > 0 : "Should be bigger than 0"
*>
fn void*? make_fallback_allocation(FreeListAlloc* self, sz size, AllocInitType init_type) @local
{
	FallbackAlloc* curr = self.fallback_root;
	while (curr)
	{
		curr = curr.next;
	}
	curr = alloc::alloc_with_padding(self.backing_alloc, FallbackAlloc, size)!;
	curr.size = size;
	curr.next = null;

	if (!self.fallback_root)
	{
		self.fallback_root = curr;
	}

	if (init_type == ZERO)
	{
		mem::clear(&curr.memory, size);
	}

	return &curr.memory;
}

<*
 Tries to find and resize allocation made as a fallback.

 @require old_ptr != null : "Shouldn't be null"
 @require new_size > 0 : "Should be bigger than 0"
*>
fn void*? resize_fallback_allocation(FreeListAlloc* self, void* old_ptr, sz new_size) @local
{
	FallbackAlloc* curr = self.fallback_root;
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

	assert(alloc != null, "Pointer originates from a different allocator: %p, not in %p - %p", old_ptr, self.data, self.data + self.capacity);

	alloc = alloc::realloc(self.backing_alloc, alloc, FallbackAlloc::size + new_size);
	alloc.size = new_size;

	return &alloc.memory;
}

<*
 Cleans up all fallback allocations.
*>
fn void free_fallback_allocations(FreeListAlloc* self) @local
{
	FallbackAlloc* curr = self.fallback_root;
	while (curr)
	{
		FallbackAlloc* next = curr.next;
		alloc::free(self.backing_alloc, curr);
		curr = next;
	}
	self.fallback_root = null;
}

<*
 Calculates all memory that were allocated as a fallback.
*>
fn sz calc_used_fallback_mem(FreeListAlloc* self) @local
{
	sz res = 0;
	FallbackAlloc* curr = self.fallback_root;
	while (curr)
	{
		res += curr.size;
		curr = curr.next;
	}
	return res;
}
