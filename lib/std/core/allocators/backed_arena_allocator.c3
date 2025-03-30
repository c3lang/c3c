module std::core::mem::allocator;
import std::io, std::math;

struct AllocChunk @local
{
	usz size;
	char[*] data;
}

struct BackedArenaAllocator (Allocator)
{
	Allocator backing_allocator;
	ExtraPage* last_page;
	usz used;
	usz capacity;
	char[*] data;
}

const usz PAGE_IS_ALIGNED @local = (usz)isz.max + 1u;

struct ExtraPage @local
{
	ExtraPage* prev_page;
	void* start;
	usz mark;
	usz size;
	usz ident;
	char[*] data;
}

macro usz ExtraPage.pagesize(&self) => self.size & ~PAGE_IS_ALIGNED;
macro bool ExtraPage.is_aligned(&self) => self.size & PAGE_IS_ALIGNED == PAGE_IS_ALIGNED;

<*
 @require size >= 16
*>
fn BackedArenaAllocator*? new_backed_allocator(usz size, Allocator allocator)
{
	BackedArenaAllocator* temp = allocator::alloc_with_padding(allocator, BackedArenaAllocator, size)!;
	temp.last_page = null;
	temp.backing_allocator = allocator;
	temp.used = 0;
	temp.capacity = size;
	return temp;
}

fn void BackedArenaAllocator.destroy(&self)
{
	self.reset(0);
	if (self.last_page) (void)self._free_page(self.last_page);
	allocator::free(self.backing_allocator, self);
}

fn usz BackedArenaAllocator.mark(&self) => self.used;

fn void BackedArenaAllocator.release(&self, void* old_pointer, bool) @dynamic
{
	usz old_size = *(usz*)(old_pointer - DEFAULT_SIZE_PREFIX);
	if (old_pointer + old_size == &self.data[self.used])
	{
		self.used -= old_size;
		asan::poison_memory_region(&self.data[self.used], old_size);
	}
}
fn void BackedArenaAllocator.reset(&self, usz mark)
{
	ExtraPage *last_page = self.last_page;
	while (last_page && last_page.mark > mark)
	{
		self.used = last_page.mark;
		ExtraPage *to_free = last_page;
		last_page = last_page.prev_page;
		self._free_page(to_free)!!;
	}
	self.last_page = last_page;
	$if env::COMPILER_SAFE_MODE || env::ADDRESS_SANITIZER:
		if (!last_page)
		{
			usz cleaned = self.used - mark;
			if (cleaned > 0)
			{
				$if env::COMPILER_SAFE_MODE && !env::ADDRESS_SANITIZER:
					self.data[mark : cleaned] = 0xAA;
				$endif
				asan::poison_memory_region(&self.data[mark], cleaned);
			}
		}
	$endif
	self.used = mark;
}

fn void? BackedArenaAllocator._free_page(&self, ExtraPage* page) @inline @local
{
	void* mem = page.start;
	return self.backing_allocator.release(mem, page.is_aligned());
}

fn void*? BackedArenaAllocator._realloc_page(&self, ExtraPage* page, usz size, usz alignment) @inline @local
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
	usz page_size = page.pagesize();
	// Clear on size > original size.
	void* data = self.acquire(size, NO_ZERO, alignment)!;
	mem::copy(data, &page.data[0], page_size, mem::DEFAULT_MEM_ALIGNMENT, mem::DEFAULT_MEM_ALIGNMENT);
	self.backing_allocator.release(real_pointer, page.is_aligned());
	return data;
}

fn void*? BackedArenaAllocator.resize(&self, void* pointer, usz size, usz alignment) @dynamic
{
	AllocChunk *chunk = pointer - AllocChunk.sizeof;
	if (chunk.size == (usz)-1)
	{
		assert(self.last_page, "Realloc of unrelated pointer");
		// First grab the page
		ExtraPage *page = pointer - ExtraPage.sizeof;
		return self._realloc_page(page, size, alignment);
	}

	AllocChunk* data = self.acquire(size, NO_ZERO, alignment)!;
	mem::copy(data, pointer, chunk.size, mem::DEFAULT_MEM_ALIGNMENT, mem::DEFAULT_MEM_ALIGNMENT);

	return data;
}

<*
 @require size > 0
 @require !alignment || math::is_power_of_2(alignment)
 @require alignment <= mem::MAX_MEMORY_ALIGNMENT : `alignment too big`
*>
fn void*? BackedArenaAllocator.acquire(&self, usz size, AllocInitType init_type, usz alignment) @dynamic
{
	alignment = alignment_for_allocation(alignment);
	void* start_mem = &self.data;
	void* starting_ptr = start_mem + self.used;
	void* aligned_header_start = mem::aligned_pointer(starting_ptr, AllocChunk.alignof);
	void* mem = aligned_header_start + AllocChunk.sizeof;
	if (alignment > AllocChunk.alignof)
	{
		mem = mem::aligned_pointer(mem, alignment);
	}
	usz new_usage = (usz)(mem - start_mem) + size;

	// Arena allocation, simple!
	if (new_usage <= self.capacity)
	{
		asan::unpoison_memory_region(starting_ptr, new_usage - self.used);
		AllocChunk* chunk_start = mem - AllocChunk.sizeof;
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
		usz total_alloc_size = mem::aligned_offset(ExtraPage.sizeof + size, alignment);
		if (init_type == ZERO)
		{
			mem = allocator::calloc_aligned(self.backing_allocator, total_alloc_size, alignment)!;
		}
		else
		{
			mem = allocator::malloc_aligned(self.backing_allocator, total_alloc_size, alignment)!;
		}
		void* start = mem;
		mem += mem::aligned_offset(ExtraPage.sizeof, alignment);
		page = (ExtraPage*)mem - 1;
		page.start = start;
		page.size = size | PAGE_IS_ALIGNED;
	}
	else
	{
		// Here we might need to pad
		usz padded_header_size = mem::aligned_offset(ExtraPage.sizeof, mem::DEFAULT_MEM_ALIGNMENT);
		usz total_alloc_size = padded_header_size + size;
		void* alloc = self.backing_allocator.acquire(total_alloc_size, init_type, 0)!;

		// Find the page.
		page = alloc + padded_header_size - ExtraPage.sizeof;
		assert(mem::ptr_is_aligned(page, BackedArenaAllocator.alignof));
		assert(mem::ptr_is_aligned(&page.data[0], mem::DEFAULT_MEM_ALIGNMENT));
		page.start = alloc;
		page.size = size;
	}

	// Mark it as a page
	page.ident = ~(usz)0;
	// Store when it was created
	page.mark = ++self.used;
	// Hook up the page.
	page.prev_page = self.last_page;
	self.last_page = page;
	return &page.data[0];
}
