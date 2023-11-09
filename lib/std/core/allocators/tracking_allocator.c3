// Copyright (c) 2021 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by the MIT license
// a copy of which can be found in the LICENSE_STDLIB file.

module std::core::mem::allocator;
import std::collections::map;
import std::collections::list;

struct Allocation
{
	usz size;
	TrackingEnv tracking_env;
	void* ptr;
}

def AllocMap = HashMap(<uptr, Allocation>);

// A simple tracking allocator.
// It tracks allocations using a hash map but
// is not compatible with allocators that uses mark()
struct TrackingAllocator (Allocator)
{
	Allocator* inner_allocator;
	AllocMap map;
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
		foreach (&allocation : self.map.value_tlist()) allocated += allocation.size;
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

fn Allocation[] TrackingAllocator.allocations_tlist(&self, Allocator* allocator)
{
	return self.map.value_tlist();
}

/**
 * @return "the number of non-freed allocations."
 **/
fn usz TrackingAllocator.allocation_count(&self) => self.map.count;

fn void*! TrackingAllocator.acquire(&self, usz size, bool clear, usz alignment, usz offset, TrackingEnv* env) @dynamic
{
	void* data = self.inner_allocator.acquire(size, clear, alignment, offset, env)!;
	self.allocs_total++;
	if (data)
	{
		self.map.set((uptr)data, { .size = size, .ptr = data, .tracking_env = env ? *env : TrackingEnv{} });
    	self.mem_total += size;
		self.allocs_total++;
	}
	return data;
}

fn void*! TrackingAllocator.resize(&self, void* old_pointer, usz size, usz alignment, usz offset, TrackingEnv* env) @dynamic
{
	void* data = self.inner_allocator.resize(old_pointer, size, alignment, offset, env)!;
	if (old_pointer)
	{
		self.map.remove((uptr)old_pointer);
	}
	if (data)
	{
		self.map.set((uptr)data, { .size = size, .ptr = data, .tracking_env = env ? *env : TrackingEnv{} });
    	self.mem_total += size;
		self.allocs_total++;
	}
	return data;
}

fn void TrackingAllocator.release(&self, void* old_pointer, bool is_aligned, TrackingEnv* env) @dynamic
{
	self.inner_allocator.release(old_pointer, is_aligned, env);
	if (old_pointer) self.map.remove((uptr)old_pointer);
}

fn void TrackingAllocator.clear(&self)
{
	self.map.clear();
}

fn void TrackingAllocator.print_report(&self) => self.fprint_report(io::stdout())!!;

fn void! TrackingAllocator.fprint_report(&self, OutStream* out)
{

	usz total = 0;
	usz entries = 0;
	@pool()
	{
		Allocation[] allocs = self.map.value_tlist();
		if (allocs.len)
		{
			$if (!env::TRACK_MEMORY):
			io::fprintn(out, "======== Memory Report ========")!;
			io::fprintn(out, "Size in bytes   Address")!;
			foreach (i, &allocation : allocs)
			{
				entries++;
				total += allocation.size;
				io::fprintfn(out, "%13s   %p", allocation.size, allocation.ptr)!;
			}
			io::fprintn(out, "===============================")!;
			$else
			io::fprintn(out, "================================== Memory Report ==================================")!;
			io::fprintn(out, "Size in bytes   Address          Function                       File")!;
			foreach (i, &allocation : allocs)
			{
				entries++;
				total += allocation.size;
				TrackingEnv *env = &allocation.tracking_env;
				io::fprintfn(out, "%13s   %p   %-30s %s:%d", allocation.size, allocation.ptr, env.function, env.file, env.line)!;
			}
			io::fprintn(out, "===================================================================================")!;
			$endif
		}
		else
		{
			io::fprintn(out, "* NO ALLOCATIONS FOUND *")!;
		}
	};
	io::fprintfn(out, "- Total currently allocated memory:            %d", total)!;
	io::fprintfn(out, "- Total current allocations:                   %d", entries)!;
	io::fprintfn(out, "- Total allocations (freed and retained):      %d", self.allocs_total)!;
	io::fprintfn(out, "- Total allocated memory (freed and retained): %d", self.mem_total)!;
}