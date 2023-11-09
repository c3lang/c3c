module std::core::mem @if(env::TRACK_MEMORY);

macro @clone(value, String file = $$FILE, String func = $$FUNC, uint line = $$LINE) @builtin
{
	return mem::heap().clone(value, .file = file, .func = func, .line = line);
}

macro @tclone(value, String file = $$FILE, String func = $$FUNC, uint line = $$LINE) @builtin
{
	return mem::temp().clone(value, .file = file, .func = func, .line = line);
}

fn void* malloc(usz size, String file = $$FILE, String func = $$FUNC, uint line = $$LINE) @builtin @inline
{
	return mem::heap().alloc(size, .file = file, .func = func, .line = line);
}

fn void* tmalloc(usz size, usz alignment = 0, usz offset = 0, String file = $$FILE, String func = $$FUNC, uint line = $$LINE) @builtin @inline
{
	return temp().acquire(size, false, alignment, offset, .env = &&TrackingEnv{ file, func, line})!!;
}

macro new($Type, String file = $$FILE, String func = $$FUNC, uint line = $$LINE)
{
	return heap().new($Type, .file = file, .func = func, .line = line);
}

macro new_clear($Type, String file = $$FILE, String func = $$FUNC, uint line = $$LINE)
{
	return heap().new_clear($Type, .file = file, .func = func, .line = line);
}

macro new_temp($Type, String file = $$FILE, String func = $$FUNC, uint line = $$LINE)
{
	return tmalloc($Type.sizeof, .file = file, .func = func, .line = line);
}

macro new_temp_clear($Type, String file = $$FILE, String func = $$FUNC, uint line = $$LINE)
{
	return tcalloc($Type.sizeof, .file = file, .func = func, .line = line);
}

macro new_array($Type, usz elements, String file = $$FILE, String func = $$FUNC, uint line = $$LINE)
{
	return heap().new_array($Type, elements, .file = file, .func = func, .line = line);
}

macro temp_array($Type, usz elements, String file = $$FILE, String func = $$FUNC, uint line = $$LINE)
{
	return (($Type*)tmalloc($Type.sizeof * elements, $Type.alignof, .file = file, .func = func, .line = line))[:elements];
}

macro new_zero_array($Type, usz elements, String file = $$FILE, String func = $$FUNC, uint line = $$LINE)
{
	return heap().new_zero_array($Type, elements, .file = file, .func = func, .line = line);
}

macro temp_zero_array($Type, usz elements, String file = $$FILE, String func = $$FUNC, uint line = $$LINE)
{
	return (($Type*)tcalloc($Type.sizeof * elements, $Type.alignof, .file = file, .func = func, .line = line))[:elements];
}

fn void* calloc(usz size, String file = $$FILE, String func = $$FUNC, uint line = $$LINE) @builtin @inline
{
	return heap().calloc(size, .file = file, .func = func, .line = line);
}

fn void* tcalloc(usz size, usz alignment = 0, usz offset = 0, String file = $$FILE, String func = $$FUNC, uint line = $$LINE) @builtin @inline
{
	return temp().acquire(size, false, alignment, offset, .env = &&TrackingEnv{ file, func, line})!!;
}

fn void* realloc(void *ptr, usz new_size, String file = $$FILE, String func = $$FUNC, uint line = $$LINE) @builtin @inline
{
	return heap().realloc(ptr, new_size, .file = file, .func = func, .line = line);
}

fn void free(void* ptr, String file = $$FILE, String func = $$FUNC, uint line = $$LINE) @builtin @inline
{
	heap().free(ptr, .file = file, .func = func, .line = line);
}

fn void* trealloc(void* ptr, usz size, usz alignment = mem::DEFAULT_MEM_ALIGNMENT, String file = $$FILE, String func = $$FUNC, uint line = $$LINE) @builtin @inline
{
	return temp().resize(ptr, size, alignment, 0, .env = &&TrackingEnv{ file, func, line})!!;
}

module std::core::mem::allocator @if(env::TRACK_MEMORY);

macro void*! Allocator.alloc_checked(&self, usz size, String file = $$FILE, String func = $$FUNC, uint line = $$LINE)
{
	char* data = self.acquire(size, false, 0, 0, .env = &&TrackingEnv{ file, func, line})!;
	mem::set(data, 0xAA, size, mem::DEFAULT_MEM_ALIGNMENT);
	return data;
}

macro void*! Allocator.calloc_checked(&self, usz size, String file = $$FILE, String func = $$FUNC, uint line = $$LINE)
{
	return self.acquire(size, true, 0, 0, .env = &&TrackingEnv{ file, func, line});
}
macro void*! Allocator.realloc_checked(&self, void* ptr, usz new_size, String file = $$FILE, String func = $$FUNC, uint line = $$LINE)
{
	return self.resize(ptr, new_size, 0, 0, .env = &&TrackingEnv{ file, func, line});
}

macro Allocator.new_array(&self, $Type, usz size, usz end_padding = 0, String file = $$FILE, String func = $$FUNC, uint line = $$LINE)
{
	return (($Type*)self.alloc_checked($Type.sizeof * size + end_padding, .file = file, .func = func, .line = line))[:size]!!;
}

macro Allocator.new_array_checked(&self, $Type, usz size, usz end_padding = 0, String file = $$FILE, String func = $$FUNC, uint line = $$LINE)
{
	return (($Type*)self.alloc_checked($Type.sizeof * size + end_padding, .file = file, .func = func, .line = line))[:size];
}

macro Allocator.new_zero_array(&self, $Type, usz size, usz end_padding = 0, String file = $$FILE, String func = $$FUNC, uint line = $$LINE)
{
	return (($Type*)self.calloc_checked($Type.sizeof * size + end_padding, .file = file, .func = func, .line = line))[:size]!!;
}

macro Allocator.new_zero_array_checked(&self, $Type, usz size, usz end_padding = 0, String file = $$FILE, String func = $$FUNC, uint line = $$LINE)
{
	return (($Type*)self.calloc_checked($Type.sizeof * size + end_padding, .file = file, .func = func, .line = line))[:size];
}

macro Allocator.new(&self, $Type, usz end_padding = 0, String file = $$FILE, String func = $$FUNC, uint line = $$LINE) @nodiscard
{
	return ($Type*)self.alloc_checked($Type.sizeof + end_padding, .file = file, .func = func, .line = line)!!;
}

macro Allocator.new_checked(&self, $Type, usz end_padding = 0, String file = $$FILE, String func = $$FUNC, uint line = $$LINE) @nodiscard
{
	return ($Type*)self.alloc_checked($Type.sizeof + end_padding, .file = file, .func = func, .line = line);
}

macro Allocator.new_clear(&self, $Type, usz end_padding = 0, String file = $$FILE, String func = $$FUNC, uint line = $$LINE) @nodiscard
{
	return ($Type*)self.calloc_checked($Type.sizeof + end_padding, .file = file, .func = func, .line = line)!!;
}

macro Allocator.new_clear_checked(&self, $Type, usz end_padding = 0, String file = $$FILE, String func = $$FUNC, uint line = $$LINE) @nodiscard
{
	return ($Type*)self.calloc_checked($Type.sizeof + end_padding, .file = file, .func = func, .line = line);
}

macro Allocator.clone(&self, value, String file = $$FILE, String func = $$FUNC, uint line = $$LINE)
{
	var x = self.alloc($typeof(value), .file = file, .func = func, .line = line);
	*x = value;
	return x;
}

macro void* Allocator.alloc(&self, usz size, String file = $$FILE, String func = $$FUNC, uint line = $$LINE) @nodiscard
{
	return self.alloc_checked(size, .file = file, .func = func, .line = line)!!;
}
macro void* Allocator.calloc(&self, usz size, String file = $$FILE, String func = $$FUNC, uint line = $$LINE) @nodiscard
{
	return self.acquire(size, true, 0, 0, .env = &&TrackingEnv{ file, func, line})!!;
}
macro void* Allocator.realloc(&self, void* ptr, usz new_size, String file = $$FILE, String func = $$FUNC, uint line = $$LINE) @nodiscard
{
	return self.resize(ptr, new_size, 0, 0, .env = &&TrackingEnv{ file, func, line})!!;
}
macro void*! Allocator.alloc_aligned(&self, usz size, usz alignment, usz offset = 0, String file = $$FILE, String func = $$FUNC, uint line = $$LINE)
{
	$if env::TESTING:
		char* data = self.acquire(size, false, alignment, offset, .env = &&TrackingEnv{ file, func, line})!;
		mem::set(data, 0xAA, size, mem::DEFAULT_MEM_ALIGNMENT);
		return data;
	$else
		return self.acquire(size, false, alignment, offset, .env = &&TrackingEnv{ file, func, line});
	$endif
}
macro void*! Allocator.calloc_aligned(&self, usz size, usz alignment, usz offset = 0, String file = $$FILE, String func = $$FUNC, uint line = $$LINE)
{
	return self.acquire(size, true, alignment, offset, .env = &&TrackingEnv{ file, func, line});
}
macro void*! Allocator.realloc_aligned(&self, void* ptr, usz new_size, usz alignment = 0, usz offset = 0, String file = $$FILE, String func = $$FUNC, uint line = $$LINE)
{
	return self.resize(ptr, new_size, alignment, offset, .env = &&TrackingEnv{ file, func, line});
}

macro void Allocator.free(&self, void* ptr, String file = $$FILE, String func = $$FUNC, uint line = $$LINE)
{
	if (ptr) ((char*)ptr)[0] = 0xBA;
	self.release(ptr, false, .env = &&TrackingEnv{ file, func, line});
}

macro void Allocator.free_aligned(&self, void* ptr, String file = $$FILE, String func = $$FUNC, uint line = $$LINE)
{
	if (ptr) ((char*)ptr)[0] = 0xBA;
	self.release(ptr, true, .env = &&TrackingEnv{ file, func, line});
}