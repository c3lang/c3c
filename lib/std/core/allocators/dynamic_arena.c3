// Copyright (c) 2021-2024 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by the MIT license
// a copy of which can be found in the LICENSE_STDLIB file.
module std::core::mem::allocators;
import std::math;

<*
 The dynamic arena allocator is an arena allocator that can grow by adding additional arena "pages".
 It only supports reset, at which point all pages except the first one is released to the backing
 allocator.

 If you want multiple save points, use the BackedArenaAllocator instead.

 The advantage over the BackedArenaAllocator, is that when allocating beyond the first "page", it will
 retain the characteristics of an arena allocator (allocating a large piece of memory then handing off
 memory from that memory), whereas the BackedArenaAllocator will have heap allocator characteristics.
*>
struct DynamicArenaAllocator (Allocator)
{
	Allocator backing_allocator;
	DynamicArenaPage* page;
	DynamicArenaPage* unused_page;
	sz page_size;
}

<*
 @param [&inout] allocator
 @require page_size >= 128
*>
fn void DynamicArenaAllocator.init(&self, Allocator allocator, sz page_size)
{
	self.page = null;
	self.unused_page = null;
	self.page_size = page_size;
	self.backing_allocator = allocator;
}

fn void DynamicArenaAllocator.free(&self)
{
	DynamicArenaPage* page = self.page;
	while (page)
	{
		DynamicArenaPage* next_page = page.prev_arena;
		alloc::free(self.backing_allocator, page.memory);
		alloc::free(self.backing_allocator, page);
		page = next_page;
	}
	page = self.unused_page;
	while (page)
	{
		DynamicArenaPage* next_page = page.prev_arena;
		alloc::free(self.backing_allocator, page.memory);
		alloc::free(self.backing_allocator, page);
		page = next_page;
	}
	self.page = null;
	self.unused_page = null;
}

struct DynamicArenaPage @local
{
	void* memory;
	void* prev_arena;
	sz total;
	sz used;
	void* current_stack_ptr;
}

struct DynamicArenaChunk @local
{
	sz size;
}

<*
 @require ptr != null
 @require self.page != null  : `tried to free pointer on invalid allocator`
*>
fn void DynamicArenaAllocator.release(&self, void* ptr, bool) @dynamic
{
	DynamicArenaPage* current_page = self.page;
	if (ptr == current_page.current_stack_ptr)
	{
		current_page.used = (sz)((ptr - DEFAULT_SIZE_PREFIX) - current_page.memory);
	}
	current_page.current_stack_ptr = null;
}

<*
 @require size > 0 : `Resize doesn't support zeroing`
 @require old_pointer != null : `Resize doesn't handle null pointers`
 @require self.page != null  : `tried to realloc pointer on invalid allocator`
 @return? mem::INVALID_ALLOC_SIZE, mem::OUT_OF_MEMORY
*>
fn void*? DynamicArenaAllocator.resize(&self, void* old_pointer, sz size, sz alignment) @dynamic
{
	DynamicArenaPage* current_page = self.page;
	alignment = alignment_for_allocation(alignment);
	sz* old_size_ptr = old_pointer - DEFAULT_SIZE_PREFIX;
	sz old_size = *old_size_ptr;
	// We have the old pointer and it's correctly aligned.
	if (old_size >= size && mem::ptr_is_aligned(old_pointer, alignment))
	{
		*old_size_ptr = size;
		if (current_page.current_stack_ptr == old_pointer)
		{
			current_page.used = (sz)((old_pointer - DEFAULT_SIZE_PREFIX) - current_page.memory);
		}
		return old_pointer;
	}
	if REUSE: (current_page.current_stack_ptr == old_pointer && mem::ptr_is_aligned(old_pointer, alignment))
	{
		assert(size > old_size);
		sz add_size = size - old_size;
		if (add_size + current_page.used > current_page.total) break REUSE;
		*old_size_ptr = size;
		current_page.used += add_size;
		return old_pointer;
	}
	void* new_mem = self.acquire(size, NO_ZERO, alignment)!;
	mem::copy(new_mem, old_pointer, math::min(old_size, size), mem::DEFAULT_MEM_ALIGNMENT);
	return new_mem;
}

fn void DynamicArenaAllocator.reset(&self)
{
	DynamicArenaPage* page = self.page;
	DynamicArenaPage** unused_page_ptr = &self.unused_page;
	while (page)
	{
		DynamicArenaPage* next_page = page.prev_arena;
		page.used = 0;
		DynamicArenaPage* prev_unused = *unused_page_ptr;
		*unused_page_ptr = page;
		page.prev_arena = prev_unused;
		page = next_page;
	}
	self.page = page;
}



<*
 @require size > 0 : `acquire expects size > 0`
 @require !alignment || math::is_power_of_2(alignment)
 @return? mem::INVALID_ALLOC_SIZE, mem::OUT_OF_MEMORY
*>
fn void*? DynamicArenaAllocator.acquire(&self, sz size, AllocInitType init_type, sz alignment) @dynamic
{
	alignment = alignment_for_allocation(alignment);
	DynamicArenaPage* page = self.page;

	void* ptr @noinit;
	do SET_DONE:
	{
		if (!page && self.unused_page)
		{
			self.page = page = self.unused_page;
			self.unused_page = page.prev_arena;
			page.prev_arena = null;
		}
		if (!page)
		{
			ptr = _alloc_new(self, size, alignment)!;
			break SET_DONE;
		}
		void* start = mem::aligned_pointer(page.memory + page.used + DynamicArenaChunk::size, alignment);
		sz new_used = start - page.memory + size;
		if ALLOCATE_NEW: (new_used > page.total)
		{
			if ((page = self.unused_page))
			{
				start = mem::aligned_pointer(page.memory + page.used + DynamicArenaChunk::size, alignment);
				new_used = start + size - page.memory;
				if (page.total >= new_used)
				{
					self.unused_page = page.prev_arena;
					page.prev_arena = self.page;
					self.page = page;
					break ALLOCATE_NEW;
				}
			}
			ptr = _alloc_new(self, size, alignment)!;
			break SET_DONE;
		}
		page.used = new_used;
		assert(start + size == page.memory + page.used);
		ptr = start;
		DynamicArenaChunk* chunk = (DynamicArenaChunk*)ptr - 1;
		chunk.size = size;
	};
	if (init_type == ZERO) mem::clear(ptr, size, mem::DEFAULT_MEM_ALIGNMENT);
	return ptr;
}

<*
 @require math::is_power_of_2(alignment)
 @require size > 0
 @return? mem::INVALID_ALLOC_SIZE, mem::OUT_OF_MEMORY
*>
fn void*? _alloc_new(DynamicArenaAllocator* self, sz size, sz alignment) @local
{
	// First, make sure that we can align it, extending the page size if needed.
	sz page_size = max(self.page_size, mem::aligned_offset(size + DynamicArenaChunk::size + alignment, alignment));
	assert(page_size > size + DynamicArenaChunk::size);
	// Grab the page without alignment (we do it ourselves)
	void* mem = alloc::malloc_try(self.backing_allocator, page_size)!;
	DynamicArenaPage*? page = alloc::new_try(self.backing_allocator, DynamicArenaPage);
	if (catch err = page)
	{
		alloc::free(self.backing_allocator, mem);
		return err~;
	}
	page.memory = mem;
	void* mem_start = mem::aligned_pointer(mem + DynamicArenaChunk::size, alignment);
	assert(mem_start + size < mem + page_size);
	DynamicArenaChunk* chunk = (DynamicArenaChunk*)mem_start - 1;
	chunk.size = size;
	page.prev_arena = self.page;
	page.total = page_size;
	page.used = mem_start + size - page.memory;
	self.page = page;
	page.current_stack_ptr = mem_start;
	return mem_start;
}