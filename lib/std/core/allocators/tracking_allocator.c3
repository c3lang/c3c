// Copyright (c) 2021 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by the MIT license
// a copy of which can be found in the LICENSE_STDLIB file.

module std::core::mem::allocator;
import std::collections::map;

typedef PtrMap = HashMap<uptr, usz>;

// A simple tracking allocator.
// It tracks allocations using a hash map but
// is not compatible with allocators that uses mark()
struct TrackingAllocator
{
	inline Allocator allocator;
	Allocator* inner_allocator;
	PtrMap map;
	usz mem_total;
	usz allocs_total;
}

/**
 * Initialize a memory arena for use using the provided bytes.
 *
 * @require this != null
 **/
fn void TrackingAllocator.init(TrackingAllocator* this, Allocator* using)
{
	*this = { .inner_allocator = using, .allocator.function = &tracking_allocator_fn };
	this.map.init(.using = using);
}

fn void TrackingAllocator.free(TrackingAllocator* this)
{
	this.map.free();
	*this = {};
}

/**
 * @param [inout] data
 * @require !alignment || math::is_power_of_2(alignment)
 */
fn void*! tracking_allocator_fn(Allocator* data, usz size, usz alignment, usz offset, void* old_pointer, AllocationKind kind) @private
{
	TrackingAllocator* this = (TrackingAllocator*)data;
	void* result = this.inner_allocator.function(this.inner_allocator, size, alignment, offset, old_pointer, kind)!;
	switch (kind)
	{
		case CALLOC:
		case ALIGNED_CALLOC:
		case ALLOC:
		case ALIGNED_ALLOC:
			this.map.set((uptr)result, size);
			this.mem_total += size;
			this.allocs_total++;
			return result;
		case REALLOC:
		case ALIGNED_REALLOC:
			this.map.remove((uptr)old_pointer);
			this.map.set((uptr)result, size);
			this.mem_total += size;
			if (size > 0) this.allocs_total++;
			return result;
		case ALIGNED_FREE:
		case FREE:
			if (!old_pointer) return null;
			this.map.remove((uptr)old_pointer);
			return null;
		case MARK:
			// Unsupported
			return null;
		case RESET:
			this.map.clear();
			return null;
	}
	unreachable();
}

fn usz TrackingAllocator.allocated(TrackingAllocator* this)
{
	usz allocated = 0;
	@pool()
	{
		foreach (usz allocation : this.map.value_tlist())
		{
			allocated += allocation;
		}
	};
	return allocated;
}

fn usz TrackingAllocator.total_allocated(TrackingAllocator* this)
{
	return this.mem_total;
}

fn usz TrackingAllocator.total_allocation_count(TrackingAllocator* this)
{
	return this.allocs_total;
}

fn usz TrackingAllocator.allocation_count(TrackingAllocator* this)
{
	return this.map.count;
}