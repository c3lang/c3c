module std::core::mem::allocator;
import std::io, std::math;
import std::core::sanitizer::asan;

// This implements the temp allocator.
// The temp allocator is a specialized allocator only intended for use where
// the allocation is strictly stack-like.
//
// It is *not* thread-safe: you cannot safely use the
// temp allocator on a thread and pass it to another thread.
//
// Fundamentally the temp allocator is a thread local arena allocator
// but the stack-like behaviour puts additional constraints to it.
//
// It works great for allocating temporary data in a scope then dropping
// that data, however you should not be storing temporary data in globals
// or locals that have a lifetime outside of the current temp allocator scope.
//
// Furthermore, note that the temp allocator is bounded, with additional
// allocations on top of that causing heap allocations. Such heap allocations
// will be cleaned up but is undesirable from a performance standpoint.
//
// If you want customizable behaviour similar to the temp allocator, consider
// the ArenaAllocator, BackedArenaAllocator or the DynamicArenaAllocator.
//
// Experimenting with the temp allocator to make it work outside of its
// standard usage patterns will invariably end in tears and frustrated
// hair pulling.
//
// Use one of the ArenaAllocators instead.

struct TempAllocator (Allocator)
{
	Allocator backing_allocator;
	TempAllocatorPage* last_page;
	TempAllocator* derived;
	bool allocated;
	usz used;
	usz capacity;
	usz original_capacity;
	char[*] data;
}

struct TempAllocatorChunk @local
{
	usz size;
	char[*] data;
}

const usz PAGE_IS_ALIGNED @private = (usz)isz.max + 1u;

struct TempAllocatorPage
{
	TempAllocatorPage* prev_page;
	void* start;
	usz size;
	usz ident;
	char[*] data;
}

macro usz TempAllocatorPage.pagesize(&self) => self.size & ~PAGE_IS_ALIGNED;
macro bool TempAllocatorPage.is_aligned(&self) => self.size & PAGE_IS_ALIGNED == PAGE_IS_ALIGNED;

<*
 @require size >= 16
 @require allocator.type != TempAllocator.typeid : "You may not create a temp allocator with a TempAllocator as the backing allocator."
*>
fn TempAllocator*? new_temp_allocator(Allocator allocator, usz size)
{
	TempAllocator* temp = allocator::alloc_with_padding(allocator, TempAllocator, size)!;
	temp.last_page = null;
	temp.backing_allocator = allocator;
	temp.used = 0;
	temp.allocated = true;
	temp.derived = null;
	temp.original_capacity = temp.capacity = size;
	return temp;
}
<*
 @require !self.derived
 @require min_size > TempAllocator.sizeof + 64 : "Min size must meaningfully hold the data + some bytes"
 @require mult > 0 : "The multiple can never be zero"
*>
fn TempAllocator*? TempAllocator.derive_allocator(&self, usz min_size, usz buffer, usz mult)
{
	usz remaining = self.capacity - self.used;
	void* mem @noinit;
	usz size @noinit;
	if (min_size + buffer > remaining)
	{
		return self.derived = new_temp_allocator(self.backing_allocator, min_size * mult)!;
	}
	usz start = mem::aligned_offset(self.used + buffer, mem::DEFAULT_MEM_ALIGNMENT);
	void* ptr = &self.data[start];
	TempAllocator* temp = (TempAllocator*)ptr;
	$if env::ADDRESS_SANITIZER:
        asan::unpoison_memory_region(ptr, TempAllocator.sizeof);
	$endif
	temp.last_page = null;
    temp.backing_allocator = self.backing_allocator;
    temp.used = 0;
    temp.allocated = false;
    temp.derived = null;
    temp.original_capacity = temp.capacity = self.capacity - start - TempAllocator.sizeof;
    self.capacity = start;
	self.derived = temp;
    return temp;
}

<*
 Reset the entire temp allocator, which will merge all the children into it.
*>
fn void TempAllocator.reset(&self)
{
	TempAllocator* child = self.derived;
	if (!child) return;
	while (child)
	{
		TempAllocator* old = child;
		child = old.derived;
		old.destroy();
	}
	self.capacity = self.original_capacity;
	$if env::ADDRESS_SANITIZER:
		asan::poison_memory_region(&self.data[self.used], self.capacity - self.used);
	$endif
	self.derived = null;
}

<*
 @require self.allocated : "Only a top level allocator should be freed."
*>
fn void TempAllocator.free(&self)
{
	self.reset();
	self.destroy();
}

fn void TempAllocator.destroy(&self) @local
{
	TempAllocatorPage *last_page = self.last_page;
	while (last_page)
	{
		TempAllocatorPage *to_free = last_page;
		last_page = last_page.prev_page;
		self._free_page(to_free)!!;
	}
	if (self.allocated)
	{
		allocator::free(self.backing_allocator, self);
		return;
	}
	$if env::COMPILER_SAFE_MODE || env::ADDRESS_SANITIZER:
    	$if env::COMPILER_SAFE_MODE && !env::ADDRESS_SANITIZER:
    		self.data[0 : self.used] = 0xAA;
		$else
            asan::poison_memory_region(&self.data[0], self.used);
    	$endif
    $endif
}

fn void TempAllocator.release(&self, void* old_pointer, bool) @dynamic
{
	usz old_size = *(usz*)(old_pointer - DEFAULT_SIZE_PREFIX);
	if (old_pointer + old_size == &self.data[self.used])
	{
		self.used -= old_size;
		asan::poison_memory_region(&self.data[self.used], old_size);
	}
}


fn void? TempAllocator._free_page(&self, TempAllocatorPage* page) @inline @local
{
	void* mem = page.start;
	return self.backing_allocator.release(mem, page.is_aligned());
}

fn void*? TempAllocator._realloc_page(&self, TempAllocatorPage* page, usz size, usz alignment) @inline @local
{
	// Then the actual start pointer:
	void* real_pointer = page.start;

	// Walk backwards to find the pointer to this page.
	TempAllocatorPage **pointer_to_prev = &self.last_page;
	// Remove the page from the list
	while (*pointer_to_prev != page)
	{
		pointer_to_prev = &((*pointer_to_prev).prev_page);
	}
	*pointer_to_prev = page.prev_page;
	usz page_size = page.pagesize();
	// Clear on size > original size.
	void* data = self.acquire(size, NO_ZERO, alignment)!;
	if (page_size > size) page_size = size;
	mem::copy(data, &page.data[0], page_size, mem::DEFAULT_MEM_ALIGNMENT, mem::DEFAULT_MEM_ALIGNMENT);
	self.backing_allocator.release(real_pointer, page.is_aligned());
	return data;
}

fn void*? TempAllocator.resize(&self, void* pointer, usz size, usz alignment) @dynamic
{
	TempAllocatorChunk *chunk = pointer - TempAllocatorChunk.sizeof;
	if (chunk.size == (usz)-1)
	{
		assert(self.last_page, "Realloc of non temp pointer");
		// First grab the page
		TempAllocatorPage *page = pointer - TempAllocatorPage.sizeof;
		return self._realloc_page(page, size, alignment);
	}
	bool is_realloc_of_last = chunk.size + pointer == &self.data[self.used];
	if (is_realloc_of_last)
	{
		isz diff = size - chunk.size;
		if (diff == 0) return pointer;
		if (self.capacity - self.used > diff)
		{
			chunk.size += diff;
			self.used += diff;
			$if env::ADDRESS_SANITIZER:
				if (diff < 0)
				{
					asan::poison_memory_region(pointer + chunk.size, -diff);
				}
				else
				{
					asan::unpoison_memory_region(pointer, chunk.size);
				}
			$endif
			return pointer;
		}
	}
	void* data = self.acquire(size, NO_ZERO, alignment)!;
	usz len_to_copy = chunk.size > size ? size : chunk.size;
	mem::copy(data, pointer, len_to_copy, mem::DEFAULT_MEM_ALIGNMENT, mem::DEFAULT_MEM_ALIGNMENT);
	if (is_realloc_of_last)
	{
		self.used = (uptr)chunk - (uptr)&self.data;
		$if env::ADDRESS_SANITIZER:
			asan::poison_memory_region(chunk, TempAllocatorChunk.sizeof + chunk.size);
		$endif
	}
	return data;
}

<*
 @require size > 0
 @require !alignment || math::is_power_of_2(alignment)
 @require alignment <= mem::MAX_MEMORY_ALIGNMENT : `alignment too big`
*>
fn void*? TempAllocator.acquire(&self, usz size, AllocInitType init_type, usz alignment) @dynamic
{
	alignment = alignment_for_allocation(alignment);
	void* start_mem = &self.data;
	void* starting_ptr = start_mem + self.used;
	void* aligned_header_start = mem::aligned_pointer(starting_ptr, TempAllocatorChunk.alignof);
	void* mem = aligned_header_start + TempAllocatorChunk.sizeof;
	if (alignment > TempAllocatorChunk.alignof)
	{
		mem = mem::aligned_pointer(mem, alignment);
	}
	usz new_usage = (usz)(mem - start_mem) + size;

	// Arena allocation, simple!
	if (new_usage <= self.capacity)
	{
		asan::unpoison_memory_region(starting_ptr, new_usage - self.used);
		TempAllocatorChunk* chunk_start = mem - TempAllocatorChunk.sizeof;
		chunk_start.size = size;
		self.used = new_usage;
		if (init_type == ZERO) mem::clear(mem, size, mem::DEFAULT_MEM_ALIGNMENT);
		return mem;
	}

	// Fallback to backing allocator
	TempAllocatorPage* page;

	// We have something we need to align.
	if (alignment > mem::DEFAULT_MEM_ALIGNMENT)
	{
		// This is actually simpler, since it will create the offset for us.
		usz total_alloc_size = mem::aligned_offset(TempAllocatorPage.sizeof + size, alignment);
		if (init_type == ZERO)
		{
			mem = allocator::calloc_aligned(self.backing_allocator, total_alloc_size, alignment)!;
		}
		else
		{
			mem = allocator::malloc_aligned(self.backing_allocator, total_alloc_size, alignment)!;
		}
		void* start = mem;
		mem += mem::aligned_offset(TempAllocatorPage.sizeof, alignment);
		page = (TempAllocatorPage*)mem - 1;
		page.start = start;
		page.size = size | PAGE_IS_ALIGNED;
	}
	else
	{
		// Here we might need to pad
		usz padded_header_size = mem::aligned_offset(TempAllocatorPage.sizeof, mem::DEFAULT_MEM_ALIGNMENT);
		usz total_alloc_size = padded_header_size + size;
		void* alloc = self.backing_allocator.acquire(total_alloc_size, init_type, 0)!;

		// Find the page.
		page = alloc + padded_header_size - TempAllocatorPage.sizeof;
		assert(mem::ptr_is_aligned(page, TempAllocator.alignof));
		assert(mem::ptr_is_aligned(&page.data[0], mem::DEFAULT_MEM_ALIGNMENT));
		page.start = alloc;
		page.size = size;
	}

	// Mark it as a page
	page.ident = ~(usz)0;
	// Hook up the page.
	page.prev_page = self.last_page;
	self.last_page = page;
	return &page.data[0];
}

