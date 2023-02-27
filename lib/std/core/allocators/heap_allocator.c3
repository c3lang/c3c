module std::core::mem::allocator;

typedef MemoryAllocFn = fn char[]!(usz);

struct SimpleHeapAllocator
{
	inline Allocator allocator;
	MemoryAllocFn alloc_fn;
	Header* free_list;
}

/**
 * @require this "Unexpectedly missing the allocator"
 * @require allocator "An underlying memory provider must be given"
 * @require !this.free_list "The allocator may not be already initialized"
 **/
fn void SimpleHeapAllocator.init(SimpleHeapAllocator* this, MemoryAllocFn allocator)
{
	this.alloc_fn = allocator;
	this.allocator = { &simple_heap_allocator_function };
	this.free_list = null;
}

/**
 * @require !alignment || math::is_power_of_2(alignment)
 * @require this `unexpectedly missing the allocator`
 */
fn void*! simple_heap_allocator_function(Allocator* this, usz size, usz alignment, usz offset, void* old_pointer, AllocationKind kind) @private
{
	SimpleHeapAllocator* heap = (SimpleHeapAllocator*)this;
	switch (kind)
    {
        case ALIGNED_ALLOC:
            return @aligned_alloc(heap._alloc, size, alignment, offset);
		case ALLOC:
            return heap._alloc(size);
        case ALIGNED_CALLOC:
            return @aligned_calloc(heap._calloc, size, alignment, offset);
    	case CALLOC:
            return heap._calloc(size);
        case ALIGNED_REALLOC:
            if (!size) nextcase ALIGNED_FREE;
            if (!old_pointer) nextcase ALIGNED_CALLOC;
            return @aligned_realloc(heap._calloc, heap._free, old_pointer, size, alignment, offset);
        case REALLOC:
            if (!size) nextcase FREE;
            if (!old_pointer) nextcase CALLOC;
            return heap._realloc(old_pointer, size);
        case RESET:
            return AllocationFailure.UNSUPPORTED_OPERATION!;
        case ALIGNED_FREE:
            @aligned_free(heap._free, old_pointer)?;
            return null;
        case FREE:
            heap._free(old_pointer);
            return null;
        default:
            unreachable();
	}
}

/**
 * @require this && old_pointer && bytes > 0
 **/
fn void*! SimpleHeapAllocator._realloc(SimpleHeapAllocator* this, void* old_pointer, usz bytes)
{
	// Find the block header.
	Header* block = (Header*)old_pointer - 1;
	if (block.size >= bytes) return old_pointer;
	void* new = this._alloc(bytes)?;
	usz max_to_copy = math::min(block.size, bytes);
	mem::copy(new, old_pointer, max_to_copy);
	this._free(old_pointer);
	return new;
}

fn void*! SimpleHeapAllocator._calloc(SimpleHeapAllocator* this, usz bytes) @local
{
	void* data = this._alloc(bytes)?;
	mem::clear(data, bytes, mem::DEFAULT_MEM_ALIGNMENT);
	return data;
}

fn void*! SimpleHeapAllocator._alloc(SimpleHeapAllocator* this, usz bytes) @local
{
	usz aligned_bytes = mem::aligned_offset(bytes, mem::DEFAULT_MEM_ALIGNMENT);
    if (!this.free_list)
    {
		this.add_block(aligned_bytes)?;
    }

	Header* current = this.free_list;
	Header* previous = current;
	while (current)
	{
		switch
		{
			case current.size >= aligned_bytes && current.size <= aligned_bytes + Header.sizeof + 64:
				if (current == previous)
				{
					this.free_list = current.next;
				}
				else
				{
					previous.next = current.next;
                }
                current.next = null;
                return current + 1;
            case current.size > aligned_bytes:
				Header* unallocated = (Header*)((char*)current + aligned_bytes + Header.sizeof);
				unallocated.size = current.size - aligned_bytes;
				unallocated.next = current.next;
				if (current == this.free_list)
				{
					this.free_list = unallocated;
				}
				else
				{
					previous.next = unallocated;
				}
				current.size = aligned_bytes;
				current.next = null;
				return current + 1;
			default:
				previous = current;
				current = current.next;
		}
	}
	this.add_block(aligned_bytes)?;
	return this.alloc(aligned_bytes);
}

fn void! SimpleHeapAllocator.add_block(SimpleHeapAllocator* this, usz aligned_bytes) @local
{
	assert(mem::aligned_offset(aligned_bytes, mem::DEFAULT_MEM_ALIGNMENT) == aligned_bytes);
	char[] result = this.alloc_fn(aligned_bytes + Header.sizeof)?;
	Header* new_block = (Header*)result.ptr;
	new_block.size = result.len - Header.sizeof;
	new_block.next = null;
	this._free(new_block + 1);
}


fn void SimpleHeapAllocator._free(SimpleHeapAllocator* this, void* ptr) @local
{
	// Empty ptr -> do nothing.
	if (!ptr) return;

	// Find the block header.
	Header* block = (Header*)ptr - 1;

	// No free list? Then just return this.
	if (!this.free_list)
	{
		this.free_list = block;
		return;
	}

	// Find where in the list it should be inserted.
	Header* current = this.free_list;
	Header* prev = current;
	while (current)
	{
		if (block < current)
		{
			// Between prev and current
			if (block > prev) break;
			// Before current
			if (current == prev) break;
		}
		prev = current;
		current = prev.next;
	}
	if (current)
	{
		// Insert after the current block.
        // Are the blocks adjacent?
        if (current == (Header*)((char*)(block + 1) + block.size))
        {
            // Merge
    		block.size += current.size + Header.sizeof;
            block.next = current.next;
        }
        else
        {
            // Chain to current
            block.next = current;
        }
	}
	if (prev == current)
	{
		// Swap new start of free list
		this.free_list = block;
	}
	else
	{
		// Prev adjacent?
		if (block == (Header*)((char*)(prev + 1) + prev.size))
		{
    		prev.size += block.size + Header.sizeof;
			prev.next = block.next;
		}
		else
		{
	        // Link prev to block
	        prev.next = block;
		}
	}
}

union Header @private
{
	struct
	{
		Header* next;
  	    usz size;
    }
    usz align;
}
