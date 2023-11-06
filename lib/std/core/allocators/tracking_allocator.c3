// Copyright (c) 2021 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by the MIT license
// a copy of which can be found in the LICENSE_STDLIB file.

module std::core::mem::allocator;
import std::collections::map;

def PtrMap = HashMap(<uptr, usz>);

// A simple tracking allocator.
// It tracks allocations using a hash map but
// is not compatible with allocators that uses mark()
struct TrackingAllocator (Allocator)
{
	Allocator* inner_allocator;
	PtrMap map;
	usz mem_total;
	usz allocs_total;
}

/**
 * Initialize a tracking allocator to wrap (and track) another allocator.
 *
 * @param [&inout] allocator "The allocator to track"
 **/
fn void TrackingAllocator.init(&self, Allocator* allocator)
{
	*self = { .inner_allocator = allocator };
	self.map.init_new(.allocator = allocator);
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

fn void*! TrackingAllocator.acquire(&self, usz size, bool clear, usz alignment, usz offset) @dynamic
{
	void* data = self.inner_allocator.acquire(size, clear, alignment, offset)!;
	self.allocs_total++;
	if (data)
	{
		self.map.set((uptr)data, size);
    	self.mem_total += size;
		self.allocs_total++;
	}
	return data;
}

fn void*! TrackingAllocator.resize(&self, void* old_pointer, usz size, usz alignment, usz offset) @dynamic
{
	void* data = self.inner_allocator.resize(old_pointer, size, alignment, offset)!;
	if (old_pointer)
	{
		self.map.remove((uptr)old_pointer);
	}
	if (data)
	{
		self.map.set((uptr)data, size);
    	self.mem_total += size;
		self.allocs_total++;
	}
	return data;
}

fn void TrackingAllocator.release(&self, void* old_pointer, bool is_aligned) @dynamic
{
	self.inner_allocator.release(old_pointer, is_aligned);
	if (old_pointer) self.map.remove((uptr)old_pointer);
}

fn void TrackingAllocator.clear(&self)
{
	self.map.clear();
}
