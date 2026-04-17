module std::core::mem::allocators;
import std::io, std::math;

<*
 The backed arena allocator provides an allocator that will allocate from a pre-allocated chunk of memory
 provided by it's backing allocator. The allocator supports mark / reset operations, so it can be used
 as a stack (push-pop) allocator. If the initial memory is used up, it will fall back to regular allocations,
 that will be safely freed on `reset`.

 While this allocator is similar to the dynamic arena, it supports multiple "save points", which the dynamic arena
 doesn't.
*>
struct BackedArenaAllocator (Allocator)
{
	Allocator backing_allocator;
	ExtraPage* last_page;
	sz used;
	sz capacity;
	char[*] data;
}

struct AllocChunk @local
{
	sz size;
	char[*] data;
}

const sz PAGE_IS_ALIGNED @local = sz::min;

struct ExtraPage @local
{
	ExtraPage* prev_page;
	void* start;
	sz mark;
	sz size;
	sz ident;
	char[*] data;
}

macro sz ExtraPage.pagesize(&self) => self.size & ~PAGE_IS_ALIGNED;
macro bool ExtraPage.is_aligned(&self) => self.size & PAGE_IS_ALIGNED == PAGE_IS_ALIGNED;

<*
 @require size >= 16
*>
fn BackedArenaAllocator*? new_backed_allocator(sz size, Allocator allocator)
{
	BackedArenaAllocator* temp = alloc::alloc_with_padding(allocator, BackedArenaAllocator, size)!;
	temp.last_page = null;
	temp.backing_allocator = allocator;
	temp.used = 0;
	temp.capacity = size;
	return temp;
}

fn void BackedArenaAllocator.destroy(&self)
{
	self.reset(0);
	if (self.last_page) (void)_free_page(self, self.last_page);
	alloc::free(self.backing_allocator, self);
}

fn sz BackedArenaAllocator.mark(&self) => self.used;

fn void BackedArenaAllocator.release(&self, void* old_pointer, bool) @dynamic
{
	sz old_size = *(sz*)(old_pointer - DEFAULT_SIZE_PREFIX);
	if (old_pointer + old_size == &self.data[self.used])
	{
		self.used -= old_size;
		asan::poison_memory_region(&self.data[self.used], (usz)old_size);
	}
}
fn void BackedArenaAllocator.reset(&self, sz mark)
{
	ExtraPage *last_page = self.last_page;
	while (last_page && last_page.mark > mark)
	{
		self.used = last_page.mark;
		ExtraPage *to_free = last_page;
		last_page = last_page.prev_page;
		_free_page(self, to_free)!!;
	}
	self.last_page = last_page;
	$if env::COMPILER_SAFE_MODE || env::ADDRESS_SANITIZER:
		if (!last_page)
		{
			sz cleaned = self.used - mark;
			if (cleaned > 0)
			{
				$if env::COMPILER_SAFE_MODE && !env::ADDRESS_SANITIZER:
					self.data[mark : cleaned] = 0xAA;
				$endif
				asan::poison_memory_region(&self.data[mark], (usz)cleaned);
			}
		}
	$endif
	self.used = mark;
}

fn void? _free_page(BackedArenaAllocator* self, ExtraPage* page) @inline @local
{
	void* mem = page.start;
	return self.backing_allocator.release(mem, page.is_aligned());
}

fn void*? _realloc_page(BackedArenaAllocator* self, ExtraPage* page, sz size, sz alignment) @inline @local
{
	// Then the actual start pointer:
	void* real_pointer = page.start;

	// Walk backwards to find the pointer to this page.
	ExtraPage **pointer_to_prev = &self.last_page;
	// Remove the page from the list
	while (*pointer_to_prev != page)
	{
		pointer_to_prev = &((*pointer_to_prev).prev_page);
	}
	*pointer_to_prev = page.prev_page;
	sz page_size = page.pagesize();
	// Clear on size > original size.
	void* data = self.acquire(size, NO_ZERO, alignment)!;
	mem::copy(data, &page.data[0], page_size, mem::DEFAULT_MEM_ALIGNMENT, mem::DEFAULT_MEM_ALIGNMENT);
	self.backing_allocator.release(real_pointer, page.is_aligned());
	return data;
}

fn void*? BackedArenaAllocator.resize(&self, void* pointer, sz size, sz alignment) @dynamic
{
	AllocChunk *chunk = pointer - AllocChunk::size;
	if (chunk.size == -1)
	{
		assert(self.last_page, "Realloc of unrelated pointer");
		// First grab the page
		ExtraPage *page = pointer - ExtraPage::size;
		return _realloc_page(self, page, size, alignment);
	}

	AllocChunk* data = self.acquire(size, NO_ZERO, alignment)!;
	mem::copy(data, pointer, math::min(size, chunk.size), mem::DEFAULT_MEM_ALIGNMENT, mem::DEFAULT_MEM_ALIGNMENT);

	return data;
}

<*
 @require size > 0
 @require !alignment || math::is_power_of_2(alignment)
 @require alignment <= mem::MAX_MEMORY_ALIGNMENT : `alignment too big`
*>
fn void*? BackedArenaAllocator.acquire(&self, sz size, AllocInitType init_type, sz alignment) @dynamic
{
	alignment = alignment_for_allocation(alignment);
	void* start_mem = &self.data;
	void* starting_ptr = start_mem + self.used;
	void* aligned_header_start = mem::aligned_pointer(starting_ptr, AllocChunk::alignment);
	void* mem = aligned_header_start + AllocChunk::size;
	if (alignment > AllocChunk::alignment)
	{
		mem = mem::aligned_pointer(mem, alignment);
	}
	sz new_usage = (sz)(mem - start_mem) + size;

	// Arena allocation, simple!
	if (new_usage <= self.capacity)
	{
		asan::unpoison_memory_region(starting_ptr, (usz)(new_usage - self.used));
		AllocChunk* chunk_start = mem - AllocChunk::size;
		chunk_start.size = size;
		self.used = new_usage;
		if (init_type == ZERO) mem::clear(mem, size, mem::DEFAULT_MEM_ALIGNMENT);
		return mem;
	}

	// Fallback to backing allocator
	ExtraPage* page;

	// We have something we need to align.
	if (alignment > mem::DEFAULT_MEM_ALIGNMENT)
	{
		// This is actually simpler, since it will create the offset for us.
		sz total_alloc_size = mem::aligned_offset(ExtraPage::size + size, alignment);
		if (init_type == ZERO)
		{
			mem = alloc::calloc_aligned(self.backing_allocator, total_alloc_size, alignment)!;
		}
		else
		{
			mem = alloc::malloc_aligned(self.backing_allocator, total_alloc_size, alignment)!;
		}
		void* start = mem;
		mem += mem::aligned_offset(ExtraPage::size, alignment);
		page = (ExtraPage*)mem - 1;
		page.start = start;
		page.size = size | PAGE_IS_ALIGNED;
	}
	else
	{
		// Here we might need to pad
		sz padded_header_size = mem::aligned_offset(ExtraPage::size, mem::DEFAULT_MEM_ALIGNMENT);
		sz total_alloc_size = padded_header_size + size;
		void* alloc = self.backing_allocator.acquire(total_alloc_size, init_type, 0)!;

		// Find the page.
		page = alloc + padded_header_size - ExtraPage::size;
		assert(mem::ptr_is_aligned(page, BackedArenaAllocator::alignment));
		assert(mem::ptr_is_aligned(&page.data[0], mem::DEFAULT_MEM_ALIGNMENT));
		page.start = alloc;
		page.size = size;
	}

	// Mark it as a page
	page.ident = ~(sz)0;
	// Store when it was created
	page.mark = ++self.used;
	// Hook up the page.
	page.prev_page = self.last_page;
	self.last_page = page;
	return &page.data[0];
}
