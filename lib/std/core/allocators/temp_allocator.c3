module std::core::mem::allocator;
import std::io;

struct TempAllocatorChunk @local
{
	usz size;
	char[*] data;
}

struct TempAllocator (Allocator)
{
	Allocator* backing_allocator;
	TempAllocatorPage* last_page;
	usz used;
	usz capacity;
	char[*] data;
}

const usz PAGE_IS_ALIGNED @private = (usz)isz.max + 1u;


struct TempAllocatorPage
{
	TempAllocatorPage* prev_page;
	void* start;
	usz mark;
	usz size;
	usz ident;
	char[*] data;
}

macro usz TempAllocatorPage.pagesize(&self) => self.size & ~PAGE_IS_ALIGNED;
macro bool TempAllocatorPage.is_aligned(&self) => self.size & PAGE_IS_ALIGNED == PAGE_IS_ALIGNED;

/**
 * @require size >= 16
 **/
fn TempAllocator*! new_temp_allocator(usz size, Allocator* allocator)
{
	TempAllocator* temp = allocator::alloc_with_padding(allocator, TempAllocator, size)!;
	temp.last_page = null;
	temp.backing_allocator = allocator;
	temp.used = 0;
	temp.capacity = size;
	return temp;
}

fn TempAllocator*! new_temp(usz size, Allocator* allocator) @deprecated("Use new_temp_allocator")
{
	return new_temp_allocator(size, allocator);
}

fn usz TempAllocator.mark(&self) @dynamic => self.used;

fn void TempAllocator.release(&self, void* old_pointer, bool) @dynamic
{
	usz old_size = *(usz*)(old_pointer - DEFAULT_SIZE_PREFIX);
	if (old_pointer + old_size == &self.data[self.used])
	{
		self.used -= old_size;
	}
}
fn void TempAllocator.reset(&self, usz mark) @dynamic
{
	TempAllocatorPage *last_page = self.last_page;
	while (last_page && last_page.mark > mark)
	{
		TempAllocatorPage *to_free = last_page;
		last_page = last_page.prev_page;
		self._free_page(to_free)!!;
	}
	self.last_page = last_page;
	self.used = mark;
}

fn void! TempAllocator._free_page(&self, TempAllocatorPage* page) @inline @local
{
	void* mem = page.start;
	return self.backing_allocator.release(mem, page.is_aligned());
}

fn void*! TempAllocator._realloc_page(&self, TempAllocatorPage* page, usz size, usz alignment, usz offset) @inline @local
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
	void* data = self.acquire(size, size > page_size, alignment, offset)!;
	mem::copy(data, &page.data[0], page_size, mem::DEFAULT_MEM_ALIGNMENT, mem::DEFAULT_MEM_ALIGNMENT);
	self.backing_allocator.release(real_pointer, page.is_aligned());
	return data;
}

fn void*! TempAllocator.resize(&self, void* pointer, usz size, usz alignment, usz offset) @dynamic
{
	if (!size)
	{
		self.release(pointer, alignment > 0);
		return null;
	}
	if (!pointer)
	{
		return self.acquire(size, true, alignment, offset);
	}
	TempAllocatorChunk *chunk = pointer - TempAllocatorChunk.sizeof;
	if (chunk.size == (usz)-1)
	{
		assert(self.last_page, "Realloc of non temp pointer");
		// First grab the page
		TempAllocatorPage *page = pointer - TempAllocatorPage.sizeof;
		return self._realloc_page(page, size, alignment, offset);
	}

	// TODO optimize last allocation
	TempAllocatorChunk* data = self.acquire(size, size > chunk.size, alignment, offset)!;
	mem::copy(data, pointer, chunk.size, mem::DEFAULT_MEM_ALIGNMENT, mem::DEFAULT_MEM_ALIGNMENT);

	return data;
}

/**
 * @require !alignment || math::is_power_of_2(alignment)
 * @require alignment <= mem::MAX_MEMORY_ALIGNMENT `alignment too big`
 **/
fn void*! TempAllocator.acquire(&self, usz size, bool clear, usz alignment, usz offset) @dynamic
{
	if (!size) return null;
	alignment = alignment_for_allocation(alignment);
	void* start_mem = &self.data;
	void* starting_ptr = start_mem + self.used;
	void* aligned_header_start = mem::aligned_pointer(starting_ptr, TempAllocatorChunk.alignof);
	void* mem = aligned_header_start + TempAllocatorChunk.sizeof;
	if (alignment > TempAllocatorChunk.alignof)
	{
		mem = mem::aligned_pointer(mem + offset, alignment) - offset;
	}
	usz new_usage = (usz)(mem - start_mem) + size;

	// Arena alignment, simple!
	if (new_usage <= self.capacity)
	{
		TempAllocatorChunk* chunk_start = mem - TempAllocatorChunk.sizeof;
		chunk_start.size = size;
		self.used = new_usage;
		if (clear) mem::clear(mem, size, mem::DEFAULT_MEM_ALIGNMENT);
		return mem;
	}

	// Fallback to backing allocator
	TempAllocatorPage* page;

	// We have something we need to align.
	if (alignment > mem::DEFAULT_MEM_ALIGNMENT || offset)
	{
		// This is actually simpler, since it will create the offset for us.
		usz total_alloc_size = TempAllocatorPage.sizeof + size;
		if (clear)
		{
			page = allocator::calloc_aligned(self.backing_allocator, total_alloc_size, alignment, TempAllocatorPage.sizeof + offset)!;
		}
		else
		{
			page = allocator::malloc_aligned(self.backing_allocator, total_alloc_size, alignment, TempAllocatorPage.sizeof + offset)!;
		}
		page.start = page;
		page.size = size | PAGE_IS_ALIGNED;
	}
	else
	{
		// Here we might need to pad
		usz padded_header_size = mem::aligned_offset(TempAllocatorPage.sizeof, mem::DEFAULT_MEM_ALIGNMENT);
		usz total_alloc_size = padded_header_size + size;
		void* alloc = self.backing_allocator.acquire(total_alloc_size, clear, 0, 0)!;

		// Find the page.
		page = alloc + padded_header_size - TempAllocatorPage.sizeof;
		assert(mem::ptr_is_aligned(page, TempAllocator.alignof));
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

fn void! TempAllocator.print_pages(&self, File* f)
{
	TempAllocatorPage *last_page = self.last_page;
	if (!last_page)
	{
		io::fprintf(f, "No pages.\n")!;
		return;
	}
	io::fprintf(f, "---Pages----\n")!;
	uint index = 0;
	while (last_page)
	{
		bool is_not_aligned = !(last_page.size & (1u64 << 63));
		io::fprintf(f, "%d. Alloc: %d %d at %p%s\n", ++index,
				last_page.size & ~(1u64 << 63), last_page.mark, &last_page.data[0], is_not_aligned ? "" : " [aligned]")!;
		last_page = last_page.prev_page;
	}
}
