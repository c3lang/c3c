// Copyright (c) 2021 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by the MIT license
// a copy of which can be found in the LICENSE_STDLIB file.

module std::core::mem::allocator;
import std::collections::map;

def PtrMap = HashMap(<uptr, usz>);

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
 * Initialize a tracking allocator to wrap (and track) another allocator.
 *
 * @param [&inout] using "The allocator to track"
 **/
fn void TrackingAllocator.init(&self, Allocator* using)
{
	*self = { .inner_allocator = using, .allocator.function = &tracking_allocator_fn };
	self.map.init(.using = using);
}

/**
 * Free this tracking allocator.
 **/
fn void TrackingAllocator.free(&self)
{
	self.map.free();
	*self = {};
}

/**
 * @return "the total allocated memory not yet freed."
 **/
fn usz TrackingAllocator.allocated(&self)
{
	usz allocated = 0;
	@pool()
	{
		foreach (usz allocation : self.map.value_tlist()) allocated += allocation;
	};
	return allocated;
}

/**
 * @return "the total memory allocated (freed or not)."
 **/
fn usz TrackingAllocator.total_allocated(&self) => self.mem_total;

/**
 * @return "the total number of allocations (freed or not)."
 **/
fn usz TrackingAllocator.total_allocation_count(&self) => self.allocs_total;

/**
 * @return "the number of non-freed allocations."
 **/
fn usz TrackingAllocator.allocation_count(&self) => self.map.count;

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
