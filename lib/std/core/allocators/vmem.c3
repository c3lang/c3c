module std::core::mem::allocator @if(env::POSIX);
import std::math, std::os::posix, libc, std::bits;

// Virtual Memory allocator

faultdef VMEM_RESERVE_FAILED, VMEM_PROTECT_FAILED;

struct Vmem (Allocator)
{
	void* ptr;
	usz allocated;
	usz capacity;
	usz pagesize;
	usz page_pot;
	usz last_page;
	usz high_water;
	VmemOptions options;
}

bitstruct VmemOptions : int
{
	bool shrink_on_reset;         // Release memory on reset
	bool protect_unused_pages;    // Protect unused pages on reset
	bool scratch_released_data;   // Overwrite released data with 0xAA
}

<*
 Implements the Allocator interface method.

 @require !reserve_page_size || math::is_power_of_2(reserve_page_size)
 @require reserve_page_size <= preferred_size : "The min reserve_page_size size must be less or equal to the preferred size"
 @require preferred_size >= 1 * mem::KB : "The preferred size must exceed 1 KB"
 @return? mem::INVALID_ALLOC_SIZE, mem::OUT_OF_MEMORY, VMEM_RESERVE_FAILED
*>
fn void? Vmem.init(&self, usz preferred_size, usz reserve_page_size = 0, VmemOptions options = { true, true, env::COMPILER_SAFE_MODE }, usz min_size = 0)
{
	void* ptr;
    static usz page_size = 0;
    if (!page_size) page_size = posix::getpagesize();
    if (page_size < reserve_page_size) page_size = reserve_page_size;
    preferred_size = mem::aligned_offset(preferred_size, page_size);
    if (!min_size) min_size = max(preferred_size / 1024, 1);
	while (preferred_size >= min_size)
	{
		ptr = posix::mmap(null, preferred_size, posix::PROT_NONE, posix::MAP_PRIVATE | posix::MAP_ANONYMOUS, -1, 0);
		// It worked?
		if (ptr != posix::MAP_FAILED && ptr) break;
		// Did it fail in a non-retriable way?
		switch (libc::errno())
		{
			case errno::ENOMEM:
			case errno::EOVERFLOW:
			case errno::EAGAIN:
				// Try a smaller size.
				preferred_size /= 2;
				continue;
		}
		break;
	}
	// Check if we ended on a failure.
	if ((ptr == posix::MAP_FAILED) || !ptr) return VMEM_RESERVE_FAILED?;
	if (page_size > preferred_size) page_size = preferred_size;
	$if env::ADDRESS_SANITIZER:
		asan::poison_memory_region(self.ptr, self.capacity);
	$endif
	*self = { .ptr = ptr, .high_water = 0,
			  .capacity = preferred_size,
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
fn void*? Vmem.acquire(&self, usz size, AllocInitType init_type, usz alignment) @dynamic
{
	alignment = alignment_for_allocation(alignment);
	usz total_len = self.capacity;
	if (size > total_len) return mem::INVALID_ALLOC_SIZE?;
	void* start_mem = self.ptr;
	void* unaligned_pointer_to_offset = start_mem + self.allocated + VmemHeader.sizeof;
	void* mem = mem::aligned_pointer(unaligned_pointer_to_offset, alignment);
	usz after = (usz)(mem - self.ptr) + size;
	if (after > total_len) return mem::OUT_OF_MEMORY?;
	if (init_type == ZERO && self.high_water <= self.allocated)
	{
		init_type = NO_ZERO;
	}
	protect(self, after)!;
	VmemHeader* header = mem - VmemHeader.sizeof;
	header.size = size;
	if (init_type == ZERO) mem::clear(mem, size, mem::DEFAULT_MEM_ALIGNMENT);
	return mem;
}

fn bool Vmem.owns_pointer(&self, void* ptr) @inline
{
	return (uptr)ptr >= (uptr)self.ptr && (uptr)ptr < (uptr)self.ptr + self.capacity;
}
<*
 Implements the Allocator interface method.

 @require !alignment || math::is_power_of_2(alignment)
 @require alignment <= mem::MAX_MEMORY_ALIGNMENT : `alignment too big`
 @require old_pointer != null
 @require size > 0
 @return? mem::INVALID_ALLOC_SIZE, mem::OUT_OF_MEMORY
*>
fn void*? Vmem.resize(&self, void *old_pointer, usz size, usz alignment) @dynamic
{
	if (size > self.capacity) return mem::INVALID_ALLOC_SIZE?;
	alignment = alignment_for_allocation(alignment);
	assert(self.owns_pointer(old_pointer), "Pointer originates from a different allocator: %p, not in %p - %p", old_pointer, self.ptr, self.ptr + self.allocated);
	VmemHeader* header = old_pointer - VmemHeader.sizeof;
    usz old_size = header.size;
    // Do last allocation and alignment match?
    if (self.ptr + self.allocated == old_pointer + old_size && mem::ptr_is_aligned(old_pointer, alignment))
    {
        if (old_size == size) return old_pointer;
    	if (old_size >= size)
    	{
    		unprotect(self, self.allocated + size - old_size);
    	}
    	else
    	{
    		usz allocated = self.allocated + size - old_size;
    		if (allocated > self.capacity) return mem::OUT_OF_MEMORY?;
    		protect(self, allocated)!;
    	}
    	header.size = size;
    	return old_pointer;
    }
    // Otherwise just allocate new memory.
    void* mem = self.acquire(size, NO_ZERO, alignment)!;
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
	VmemHeader* header = ptr - VmemHeader.sizeof;
	// Reclaim memory if it's the last element.
	if (ptr + header.size == self.ptr + self.allocated)
	{
		unprotect(self, self.allocated - header.size - VmemHeader.sizeof);
	}
}

fn usz Vmem.mark(&self)
{
	return self.allocated;
}

<*
 @require mark <= self.allocated : "Invalid mark"
*>
fn void Vmem.reset(&self, usz mark)
{
	if (mark == self.allocated) return;
	unprotect(self, mark);
}

fn void Vmem.free(&self)
{
	if (!self.ptr) return;
	$switch:
		$case env::ADDRESS_SANITIZER:
			asan::poison_memory_region(self.ptr, self.capacity);
		$case env::COMPILER_SAFE_MODE:
    	    ((char*)self.ptr)[0:self.allocated] = 0xAA;
	$endswitch
	posix::munmap(self.ptr, self.capacity);
	*self = {};
}

// Internal data

struct VmemHeader @local
{
	usz size;
	char[*] data;
}

macro void? protect(Vmem* mem, usz after) @local
{
	usz shift = mem.page_pot;
	usz page_after = (after + mem.pagesize - 1) >> shift;
	usz last_page = mem.last_page;
	bool over_high_water = mem.high_water < after;
	if (page_after > last_page)
	{
		if (mem.options.protect_unused_pages || over_high_water)
		{
			if (posix::mprotect(mem.ptr + last_page << shift, (page_after - last_page) << shift, posix::PROT_WRITE | posix::PROT_READ)) return VMEM_PROTECT_FAILED?;
		}
		mem.last_page = page_after;
	}
	$if env::ADDRESS_SANITIZER:
		asan::unpoison_memory_region(mem.ptr + mem.allocated, after - mem.allocated);
	$endif
	mem.allocated = after;
	if (over_high_water) mem.high_water = after;
}

macro void unprotect(Vmem* mem, usz after) @local
{
	usz shift = mem.page_pot;
	usz last_page = mem.last_page;
	usz page_after = mem.last_page = (after + mem.pagesize - 1) >> shift;
	$if env::ADDRESS_SANITIZER:
		asan::poison_memory_region(mem.ptr + after, mem.allocated - after);
	$else
		if (mem.options.scratch_released_data)
		{
			mem::set(mem.ptr + after, 0xAA, mem.allocated - after);
		}
	$endif
	if ((mem.options.shrink_on_reset || mem.options.protect_unused_pages) && page_after < last_page)
	{
		void* start = mem.ptr + page_after << shift;
		usz len = (last_page - page_after) << shift;
		if (mem.options.shrink_on_reset) posix::madvise(start, len, posix::MADV_DONTNEED);
		if (mem.options.protect_unused_pages) posix::mprotect(start, len, posix::PROT_NONE);
	}
	mem.allocated = after;
}
