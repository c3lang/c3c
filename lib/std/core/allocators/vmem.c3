module std::core::mem::allocators @if(env::POSIX || env::WIN32);
import std::math, std::os::posix, libc, std::bits;



// Virtual Memory allocator

faultdef VMEM_RESERVE_FAILED;

struct Vmem (Allocator)
{
	VirtualMemory memory;
	sz allocated;
	sz pagesize;
	sz page_pot;
	sz last_page;
	sz high_water;
	VmemOptions options;
}

bitstruct VmemOptions : int
{
	<* Release memory on reset *>
	bool shrink_on_reset;
	<* Protect unused pages on reset *>
	bool protect_unused_pages;
	<* Overwrite released data with 0xAA *>
	bool scratch_released_data;
}

<*
 Implements the Allocator interface method.

 @require !reserve_page_size || math::is_power_of_2(reserve_page_size)
 @require reserve_page_size <= preferred_size : "The min reserve_page_size size must be less or equal to the preferred size"
 @require preferred_size >= 1 * mem::KB : "The preferred size must exceed 1 KB"
 @return? mem::INVALID_ALLOC_SIZE, mem::OUT_OF_MEMORY, VMEM_RESERVE_FAILED
*>
fn void? Vmem.init(&self, sz preferred_size, sz reserve_page_size = 0, VmemOptions options = { true, true, env::COMPILER_SAFE_MODE }, sz min_size = 0)
{
	static sz page_size = 0;
	if (!page_size) page_size = mem::os_pagesize();
	if (page_size < reserve_page_size) page_size = reserve_page_size;
	preferred_size = mem::aligned_offset(preferred_size, page_size);
	if (!min_size) min_size = max(preferred_size / 1024, 1);
	VirtualMemory? memory = mem::OUT_OF_MEMORY~;
	while (preferred_size >= min_size)
	{
		memory = vm::virtual_alloc(preferred_size, PROTECTED);
		// It worked?
		if (try memory) break;
		switch (@catch(memory))
		{
			case mem::OUT_OF_MEMORY:
			case vm::RANGE_OVERFLOW:
				// Try a smaller size.
				preferred_size /= 2;
				continue;
			default:
				break;
		}
	}
	if (catch memory) return VMEM_RESERVE_FAILED~;
	if (page_size > preferred_size) page_size = preferred_size;
	$if env::ADDRESS_SANITIZER:
		asan::poison_memory_region(memory.ptr, memory.size);
	$endif
	*self = { .memory = memory,
	          .high_water = 0,
			  .pagesize = page_size,
			  .page_pot = page_size.ctz(),
			  .options  = options,
	        };
}

<*
 Implements the Allocator interface method.

 @require !alignment || math::is_power_of_2(alignment)
 @require alignment <= mem::MAX_MEMORY_ALIGNMENT : `alignment too big`
 @require size > 0
 @return? mem::INVALID_ALLOC_SIZE, mem::OUT_OF_MEMORY
*>
fn void*? Vmem.acquire(&self, sz size, AllocInitType init_type, sz alignment) @dynamic
{
	alignment = alignment_for_allocation(alignment);
	sz total_len = self.memory.size;
	if (size > total_len) return mem::INVALID_ALLOC_SIZE~;
	void* start_mem = self.memory.ptr;
	void* unaligned_pointer_to_offset = start_mem + self.allocated + VmemHeader::size;
	void* mem = mem::aligned_pointer(unaligned_pointer_to_offset, alignment);
	sz after = (sz)(mem - start_mem) + size;
	if (after > total_len) return mem::OUT_OF_MEMORY~;
	if (init_type == ZERO && self.high_water <= self.allocated)
	{
		init_type = NO_ZERO;
	}
	protect(self, after)!;
	VmemHeader* header = mem - VmemHeader::size;
	header.size = size;
	if (init_type == ZERO) mem::clear(mem, size, mem::DEFAULT_MEM_ALIGNMENT);
	return mem;
}

fn bool Vmem.owns_pointer(&self, void* ptr) @inline
{
	return (uptr)ptr >= (uptr)self.memory.ptr && (uptr)ptr < (uptr)self.memory.ptr + (usz)self.memory.size;
}
<*
 Implements the Allocator interface method.

 @require !alignment || math::is_power_of_2(alignment)
 @require alignment <= mem::MAX_MEMORY_ALIGNMENT : `alignment too big`
 @require old_pointer != null
 @require size > 0
 @return? mem::INVALID_ALLOC_SIZE, mem::OUT_OF_MEMORY
*>
fn void*? Vmem.resize(&self, void *old_pointer, sz size, sz alignment) @dynamic
{
	if (size > self.memory.size) return mem::INVALID_ALLOC_SIZE~;
	alignment = alignment_for_allocation(alignment);
	assert(self.owns_pointer(old_pointer), "Pointer originates from a different allocator: %p, not in %p - %p", old_pointer, self.memory.ptr, self.memory.ptr + self.allocated);
	VmemHeader* header = old_pointer - VmemHeader::size;
    sz old_size = header.size;
    if (old_size == size) return old_pointer;
    // Do last allocation and alignment match?
    if (self.memory.ptr + self.allocated == old_pointer + old_size && mem::ptr_is_aligned(old_pointer, alignment))
    {
    	if (old_size > size)
    	{
    		unprotect(self, self.allocated + size - old_size);
    	}
    	else
    	{
    		sz allocated = self.allocated + size - old_size;
    		if (allocated > self.memory.size) return mem::OUT_OF_MEMORY~;
    		protect(self, allocated)!;
    	}
    	header.size = size;
    	return old_pointer;
    }
    if (old_size > size)
    {
        $if env::ADDRESS_SANITIZER:
    	    asan::poison_memory_region(old_pointer + size, old_size - size);
    	$endif
    	header.size = size;
    	return old_pointer;
    }
    // Otherwise just allocate new memory.
    void* mem = self.acquire(size, NO_ZERO, alignment)!;
    assert(size > old_size);
    mem::copy(mem, old_pointer, old_size, mem::DEFAULT_MEM_ALIGNMENT, mem::DEFAULT_MEM_ALIGNMENT);
    return mem;
}

<*
 Implements the Allocator interface method.

 @require ptr != null
*>
fn void Vmem.release(&self, void* ptr, bool) @dynamic
{
	assert(self.owns_pointer(ptr), "Pointer originates from a different allocator %p.", ptr);
	VmemHeader* header = ptr - VmemHeader::size;
	// Reclaim memory if it's the last element.
	if (ptr + header.size == self.memory.ptr + self.allocated)
	{
		unprotect(self, self.allocated - header.size - VmemHeader::size);
	}
}

fn sz Vmem.mark(&self)
{
	return self.allocated;
}

<*
 @require mark <= self.allocated : "Invalid mark"
*>
fn void Vmem.reset(&self, sz mark)
{
	if (mark == self.allocated) return;
	unprotect(self, mark);
}

fn void Vmem.free(&self)
{
	if (!self.memory.ptr) return;
	$switch:
		$case env::ADDRESS_SANITIZER:
			asan::poison_memory_region(self.memory.ptr, self.memory.size);
		$case env::COMPILER_SAFE_MODE:
    	    ((char*)self.memory.ptr)[0:self.allocated] = 0xAA;
	$endswitch
	(void)self.memory.destroy();
	*self = {};
}

// Internal data

struct VmemHeader @local
{
	sz size;
	char[*] data;
}

macro void? protect(Vmem* mem, sz after) @local
{
	sz shift = mem.page_pot;
	sz page_after = (after + mem.pagesize - 1) >> shift;
	sz last_page = mem.last_page;
	bool over_high_water = mem.high_water < after;
	if (page_after > last_page)
	{
		sz page_start = last_page << shift;
		sz page_len = (page_after - last_page) << shift;
		mem.memory.commit(page_start, page_len)!;
		if (mem.options.protect_unused_pages || over_high_water)
		{
			mem.memory.protect(page_start, page_len, READWRITE)!;
		}
		mem.last_page = page_after;
	}
	$if env::ADDRESS_SANITIZER:
		asan::unpoison_memory_region(mem.memory.ptr + mem.allocated, after - mem.allocated);
	$endif
	mem.allocated = after;
	if (over_high_water) mem.high_water = after;
}

macro void unprotect(Vmem* mem, sz after) @local
{
	sz shift = mem.page_pot;
	sz last_page = mem.last_page;
	sz page_after = mem.last_page = (after + mem.pagesize - 1) >> shift;
	$if env::ADDRESS_SANITIZER:
		asan::poison_memory_region(mem.memory.ptr + after, mem.allocated - after);
	$else
		if (mem.options.scratch_released_data)
		{
			mem::set(mem.memory.ptr + after, 0xAA, mem.allocated - after);
		}
	$endif
	if ((mem.options.shrink_on_reset || mem.options.protect_unused_pages) && page_after < last_page)
	{
		sz start = page_after << shift;
		sz len = (last_page - page_after) << shift;
		if (mem.options.shrink_on_reset) (void)mem.memory.decommit(start, len, false);
		if (mem.options.protect_unused_pages) (void)mem.memory.protect(start, len, PROTECTED);
	}
	mem.allocated = after;
}
