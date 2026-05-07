module std::core::mem::allocators;
import std::io;
import std::math;

const DEFAULT_CAPACITY	   @local = 4096;
const MIN_BLOCK_SIZE       @local = mem::DEFAULT_MEM_ALIGNMENT + FreeNode::size;
const MIN_CAPACITY         @local = MIN_BLOCK_SIZE * 4;

<*
 Binary search tree heap allocator is an allocator that preallocates a large block of memory and embeds a binary search tree to enable efficient searching for available blocks.
 After a deallocation, it performs coalescing of adjacent nodes when possible.
 The user can choose between two strategies: best fit and first fit. Best fit minimizes memory fragmentation but is generally slower than the first fit strategy.
 Unlike an arena allocator, it supports deallocation in arbitrary order, making it suitable for frequent deallocations and efficient memory reuse.
 Unlike buddy allocator, this allocator doesn't sort the underlying memory and does not require allocations to be powers of two.

 This allocator is intended to be used in environments where there isn't any native libc malloc,
 and it has to be emulated from a memory region, or wrapping linear memory as is the case for plain WASM.
*>
struct BSTHeapAllocator (Allocator, Printable)
{
	char* data;
	sz capacity;
	FreeNode* root;
	FallbackAlloc* fallback_root;
	BSTAllocStrategy strategy;
	MemoryAllocFn alloc_fn;
	MemoryFreeFn free_fn;
}

enum BSTAllocStrategy
{
	BEST_FIT,
	FIRST_FIT,
}

bitstruct NodeState : sz @local
{
	bool is_free : 0..0;
	// Aligned size of memory block + AllocHeader.
	sz size : 1..(bitsizeof(sz) - 1);
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

bitstruct FallbackAllocState : sz @local
{
	sz padding : 0..9;
	sz size : 10..(bitsizeof(sz) - 1);
}

struct FallbackAlloc @local
{
	inline FallbackAllocState _state;
	FallbackAlloc* next;
	char[*] memory;
}

<*
 Initializes bst allocator structure.

 @require alloc_fn != null : "Shouldn't be null"
 @require free_fn != null : "Shouldn't be null"
 @require init_capacity > MIN_CAPACITY : "Capacity should be greater than minimum capacity"
*>
fn void BSTHeapAllocator.init(
	&self,
	MemoryAllocFn alloc_fn,
	MemoryFreeFn free_fn = &dummy_free_fn,
	sz init_capacity = DEFAULT_CAPACITY,
	BSTAllocStrategy strategy = BEST_FIT,
)
{
	init_capacity = mem::aligned_offset(init_capacity, mem::DEFAULT_MEM_ALIGNMENT);
	self.data = (char*)alloc_fn(AllocHeader::size + init_capacity)!!;
	self.capacity = init_capacity; 
	self.alloc_fn = alloc_fn;
	self.free_fn = free_fn;
	self.strategy = strategy;
	self.reset();
}

<*
 Implements the Allocator interface method.

 @require !alignment || math::is_power_of_2(alignment)
 @require alignment <= mem::MAX_MEMORY_ALIGNMENT : `alignment too big`
 @require size > 0
*>
fn void*? BSTHeapAllocator.acquire(&self, sz size, AllocInitType init_type, sz alignment) @dynamic
{
	alignment = allocators::alignment_for_allocation(alignment);
	sz aligned_size = mem::aligned_offset(size + AllocHeader::size, alignment);

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
		return fallback_alloc(self, aligned_size, init_type, alignment);
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
fn void*? BSTHeapAllocator.resize(&self, void* old_pointer, sz size, sz alignment) @dynamic
{
	if (@unlikely(!owns_pointer_in_main_buff(self, old_pointer)))
	{
		return fallback_realloc(self, old_pointer, size, alignment);
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
		if (diff >= MIN_BLOCK_SIZE)
		{
			FreeNode* new_node = node_next(node);
			init_node(new_node, diff, node);
			insert_node(&self.root, new_node);
		}
		return old_pointer;
	}

	FreeNode* next = node_next(node);
	FreeNode* tail = tail_ref(self);

	// Try to extend.
	if (next < tail && next.is_free)
	{
		sz diff = mem::aligned_offset(new_size - old_size, alignment);
		sz new_node_size = next.size - diff;
		FreeNode* new_node = null;
		// Insert a new node if have enough space.
		if (new_node_size >= MIN_BLOCK_SIZE)
		{
			node.size += diff;
			new_node = node_next(node);
			init_node(new_node, new_node_size, node);
			remove_node(&self.root, next);
			insert_node(&self.root, new_node);
		}
		// Or just consume all space of the next node.
		else
		{
			node.size += next.size;
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
fn void BSTHeapAllocator.release(&self, void* ptr, bool) @dynamic
{
	if (!owns_pointer_in_main_buff(self, ptr))
	{
		return fallback_free(self, ptr);
	}

	FreeNode* free_node = (FreeNode*)((AllocHeader*)ptr - 1);
	// Prevent double free.
	if (@unlikely(free_node.is_free)) return;

	// Zero previously allocated memory.
	free_node.parent = null;
	free_node.left = null;
	free_node.right = null;

	// Order of this lines should be preserved.
	free_node = coalesce_nodes_around(self, free_node);
	insert_node(&self.root, free_node);
}

<*
 Frees resources of the allocator.
*>
fn void BSTHeapAllocator.free(&self)
{
	if (@likely(self.data != null))
	{
		self.free_fn(self.data);
	}
	if (self.fallback_root != null)
	{
		fallback_free_all(self);
	}
	*self = { };
}

<*
 Resets the state of the allocator making it available for reuse.
*>
fn void BSTHeapAllocator.reset(&self)
{
	self.root = (FreeNode*)self.data;
	init_node(self.root, self.capacity, null);
}

<*
 Implements 'Printable' interface.

 @require self.root != null : "Must be initialized"
 @require tail_ref(self) != null : "Must be initialized"
*>
fn sz? BSTHeapAllocator.to_format(&self, Formatter* fmt) @dynamic
{
	sz total = fmt.print("Binary tree allocator info:")!;
	sz main_buff_used = 0;

	if (!self.root) {
		total += fmt.print("[Empty Buffer]")!;
		return total;
	};

	FreeNode* curr = (FreeNode*)self.data;
	FreeNode* tail = tail_ref(self);
	String usage_s;

	while (curr && curr < tail)
	{
		if (!curr.is_free)
		{
			usage_s = "used";
			main_buff_used += curr.size;
		}
		else
		{
			usage_s = "free";
		}
		total += fmt.printf("\n\t[%s : %sB]", usage_s, curr.size)!;
		curr = node_next(curr);
	}

	total += fmt.print("\n--- Block report finished ---\n")!;
	total += print_total_usage(self, fmt, main_buff_used)!;

	return total;
}

<*
 Print total memory usage statistics.
*>
fn sz? print_total_usage(BSTHeapAllocator* self, Formatter* fmt, sz main_buff_used) @local
{
	sz fallback_used = calc_used_fallback_mem(self);
	sz main_buff_remain = self.capacity - main_buff_used;
	String fmt_string = "Total usage:\n\tmemory total  = %.0fB / %.2fKiB / %.3fMiB / %.4fGiB\n\tmemory used   = %.0fB / %.2fKiB / %.3fMiB / %.4fGiB\n\tmemory remain = %.0fB / %.2fKiB / %.3fMiB / %.4fGiB\n\textra allocated = %.0fB / %.2fKiB / %.3fMiB / %.4fGiB";

	return fmt.printf(
		fmt_string,
		(float)self.capacity,
		self.capacity / (float)mem::KB,
		self.capacity / (float)mem::MB,
		self.capacity / (float)mem::GB,
		(float)main_buff_used,
		main_buff_used / (float)mem::KB,
		main_buff_used / (float)mem::MB,
		main_buff_used / (float)mem::GB,
		(float)main_buff_remain,
		main_buff_remain / (float)mem::KB,
		main_buff_remain / (float)mem::MB,
		main_buff_remain / (float)mem::GB,
		(float)fallback_used,
		fallback_used / (float)mem::KB,
		fallback_used / (float)mem::MB,
		fallback_used / (float)mem::GB,
	);
}

// ------ Private API ------

<*
 Tries to find node based on best fit strategy.
 (node with minimal required space is returned).
*>
fn FreeNode* find_node_best_fit(BSTHeapAllocator* self, sz req_size) @local
{
	if (!self.root) return null;

	FreeNode* it_curr = self.root;
	FreeNode* tail = tail_ref(self);
	FreeNode* best_node = null;

	while (it_curr && it_curr < tail)
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
fn FreeNode* find_node_first_fit(BSTHeapAllocator* self, sz req_size) @local
{
	if (!self.root) return null;

	FreeNode* curr = self.root;
	FreeNode* tail = tail_ref(self);

	while (curr && curr < tail)
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
 Inserts the node into the tree of free nodes, making it available for future allocations.
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
 Removes the node from the tree of free nodes, so its memory can be provided to the user.

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
fn FreeNode* coalesce_nodes_around(BSTHeapAllocator* self, FreeNode* node) @local
{
	FreeNode* next = node_next(node);
	FreeNode* tail = tail_ref(self);
	bool was_merge = false;

	if (next < tail && next.is_free)
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
		if (next < tail)
		{
			next.prev_phys = node;
		}
	}

	return node;
}

<*
 Checks if provided pointer belongs to the bst allocator main buffer.

 @require ptr != null : "Shoudn't be null"
*>
fn bool owns_pointer_in_main_buff(BSTHeapAllocator* self, void* ptr) @inline @local
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
 Returns a tail node of the allocator, which serves as a memory buffer boundary.
*>
fn FreeNode* tail_ref(BSTHeapAllocator* self) @inline @local
{
	return (FreeNode*)(self.data + self.capacity);
}

// ---- Fallback allocations ----

fn void*? fallback_alloc(BSTHeapAllocator* self, sz size, AllocInitType init_type, sz alignment) @local
{
	if (alignment > 0)
	{
		size = mem::aligned_offset(size + FallbackAlloc::size + alignment, mem::DEFAULT_MEM_ALIGNMENT);
		char* alloc = (char*)self.alloc_fn(size)!;
		FallbackAlloc* aligned_alloc = (FallbackAlloc*)mem::aligned_pointer(alloc + FallbackAlloc::size, alignment);
		aligned_alloc--;
		assert((char*)aligned_alloc >= alloc, "Alignment failed: %p < %p", aligned_alloc, alloc);
		sz padding = (char*)aligned_alloc - alloc;
		aligned_alloc.size = size - padding - FallbackAlloc::size;
		aligned_alloc.padding = padding;
		aligned_alloc.next = self.fallback_root;
		if (init_type == ZERO)
		{
			mem::clear(&aligned_alloc.memory, aligned_alloc.size);
		}
		self.fallback_root = aligned_alloc;
		return &aligned_alloc.memory;
	}
	else
	{
		size = mem::aligned_offset(size + FallbackAlloc::size, mem::DEFAULT_MEM_ALIGNMENT);
		FallbackAlloc* alloc = (FallbackAlloc*)(char*)self.alloc_fn(size)!;
		alloc.size = size - FallbackAlloc::size;
		alloc.padding = 0;
		alloc.next = self.fallback_root;
		if (init_type == ZERO)
		{
			mem::clear(&alloc.memory, alloc.size);
		}
		self.fallback_root = alloc;
		return &alloc.memory;
	}
}

<*
 Reallocates allocation made as a fallback.

 @require old_ptr != null : "Shouldn't be null"
 @require new_size > 0 : "Should be bigger than 0"
*>
fn void*? fallback_realloc(BSTHeapAllocator* self, void* old_ptr, sz new_size, sz alignment) @local
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
fn void fallback_free(BSTHeapAllocator* self, void* ptr) @local
{
	if (!self.fallback_root || !ptr)
	{
		return;
	}

	FallbackAlloc* curr = self.fallback_root;
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
		self.fallback_root = self.fallback_root.next;
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
fn void fallback_free_all(BSTHeapAllocator* self) @local
{
	FallbackAlloc* curr = self.fallback_root;
	while (curr)
	{
		FallbackAlloc* next = curr.next;
		curr = (FallbackAlloc*)((char*)curr - curr.padding);
		self.free_fn(curr);
		curr = next;
	}
	self.fallback_root = null;
}

<*
 Calculates all memory that were allocated as a fallback.
*>
fn sz calc_used_fallback_mem(BSTHeapAllocator* self) @local
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
