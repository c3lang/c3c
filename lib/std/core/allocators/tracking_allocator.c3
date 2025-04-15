// Copyright (c) 2021-2025 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by the MIT license
// a copy of which can be found in the LICENSE_STDLIB file.

module std::core::mem::allocator;
import std::collections, std::io, std::os::backtrace;

const MAX_BACKTRACE = 16;
struct Allocation
{
	void* ptr;
	usz size;
	void*[MAX_BACKTRACE] backtrace;
}

alias AllocMap = HashMap { uptr, Allocation };

// A simple tracking allocator.
// It tracks allocations using a hash map but
// is not compatible with allocators that uses mark()
//
// It is also embarassingly single-threaded, so
// do not use it to track allocations that cross threads.

struct TrackingAllocator (Allocator)
{
	Allocator inner_allocator;
	AllocMap map;
	usz mem_total;
	usz allocs_total;
}

<*
 Initialize a tracking allocator to wrap (and track) another allocator.

 @param [&inout] allocator : "The allocator to track"
*>
fn void TrackingAllocator.init(&self, Allocator allocator)
{
	*self = { .inner_allocator = allocator };
	self.map.init(allocator);
}

<*
 Free this tracking allocator.
*>
fn void TrackingAllocator.free(&self)
{
	self.map.free();
	*self = {};
}

<*
 @return "the total allocated memory not yet freed."
*>
fn usz TrackingAllocator.allocated(&self) => @pool()
{
	usz allocated = 0;
	foreach (&allocation : self.map.tvalues()) allocated += allocation.size;
	return allocated;
}

<*
 @return "the total memory allocated (freed or not)."
*>
fn usz TrackingAllocator.total_allocated(&self) => self.mem_total;

<*
 @return "the total number of allocations (freed or not)."
*>
fn usz TrackingAllocator.total_allocation_count(&self) => self.allocs_total;

fn Allocation[] TrackingAllocator.allocations_tlist(&self, Allocator allocator)
{
	return self.map.tvalues();
}

<*
 @return "the number of non-freed allocations."
*>
fn usz TrackingAllocator.allocation_count(&self) => self.map.count;

fn void*? TrackingAllocator.acquire(&self, usz size, AllocInitType init_type, usz alignment) @dynamic
{
	void* data = self.inner_allocator.acquire(size, init_type, alignment)!;
	self.allocs_total++;
	void*[MAX_BACKTRACE] bt;
	backtrace::capture_current(&bt);
	self.map.set((uptr)data, { data, size, bt });
	self.mem_total += size;
	return data;
}

fn void*? TrackingAllocator.resize(&self, void* old_pointer, usz size, usz alignment) @dynamic
{
	void* data = self.inner_allocator.resize(old_pointer, size, alignment)!;
	self.map.remove((uptr)old_pointer);
	void*[MAX_BACKTRACE] bt;
	backtrace::capture_current(&bt);
	self.map.set((uptr)data, { data, size, bt });
	self.mem_total += size;
	self.allocs_total++;
	return data;
}

fn void TrackingAllocator.release(&self, void* old_pointer, bool is_aligned) @dynamic
{
	if (catch self.map.remove((uptr)old_pointer))
	{
		unreachable("Attempt to release untracked pointer %p, this is likely a bug.", old_pointer);
	}
	self.inner_allocator.release(old_pointer, is_aligned);
}

fn void TrackingAllocator.clear(&self)
{
	self.map.clear();
}

fn bool TrackingAllocator.has_leaks(&self)
{
	return self.map.len() > 0;
}

fn void TrackingAllocator.print_report(&self) => self.fprint_report(io::stdout())!!;


fn void? TrackingAllocator.fprint_report(&self, OutStream out) => @pool()
{
	usz total = 0;
	usz entries = 0;
	bool leaks = false;

	Allocation[] allocs = self.map.tvalues();
	if (allocs.len)
	{
		if (!allocs[0].backtrace[0])
		{
			io::fprintn(out, "======== Memory Report ========")!;
			io::fprintn(out, "Size in bytes   Address")!;
			foreach (i, &allocation : allocs)
			{
				entries++;
				total += allocation.size;
				io::fprintfn(out, "%13s   %p", allocation.size, allocation.ptr)!;
			}
			io::fprintn(out, "===============================")!;

		}
		else
		{
			io::fprintn(out, "================================== Memory Report ==================================")!;
			io::fprintn(out, "Size in bytes   Address          Function                       ")!;
			foreach (i, &allocation : allocs)
			{
				entries++;
				total += allocation.size;
				BacktraceList backtraces = {};
				Backtrace trace = backtrace::BACKTRACE_UNKNOWN;
				if (allocation.backtrace[3])
				{
					trace = backtrace::symbolize_backtrace(tmem, allocation.backtrace[3:1]).get(0) ?? backtrace::BACKTRACE_UNKNOWN;
				}
				if (trace.function.len)	leaks = true;
				io::fprintfn(out, "%13s   %p   %s:%d", allocation.size,
					allocation.ptr, trace.function.len ? trace.function : "???",
					trace.line ? trace.line : 0)!;
			}
			io::fprintn(out, "===================================================================================")!;
		}
	}
	else
	{
		io::fprintn(out, "* NO ALLOCATIONS FOUND *")!;
	}
	io::fprintfn(out, "- Total currently allocated memory:            %d", total)!;
	io::fprintfn(out, "- Total current allocations:                   %d", entries)!;
	io::fprintfn(out, "- Total allocations (freed and retained):      %d", self.allocs_total)!;
	io::fprintfn(out, "- Total allocated memory (freed and retained): %d", self.mem_total)!;
	if (leaks)
	{
		io::fprintn(out)!;
		io::fprintn(out, "Full leak report:")!;
		foreach (i, &allocation : allocs)
		{
			if (!allocation.backtrace[3])
			{
				io::fprintfn(out, "Allocation %d (%d bytes) - no backtrace available.", i + 1, allocation.size)!;
				continue;
			}
			BacktraceList backtraces = {};
			usz end = MAX_BACKTRACE;
			foreach (j, val : allocation.backtrace)
			{
				if (!val)
				{
					end = j;
					break;
				}
			}
			BacktraceList list = backtrace::symbolize_backtrace(tmem, allocation.backtrace[3..(end - 1)])!;
			io::fprintfn(out, "Allocation %d (%d bytes): ", i + 1, allocation.size)!;
			foreach (trace : list)
			{
				if (trace.has_file())
				{
					io::fprintfn(out, "   %s (in %s:%d)", trace.function, trace.file, trace.line);
					continue;
				}
				if (trace.is_unknown())
				{
					io::fprintfn(out, "   ??? (in unknown)");
					continue;
				}
				io::fprintfn(out, "   %s (source unavailable)", trace.function);
			}
		}
	}
}