module std::collections::deque <Type>;
import std::io, std::math;

const sz DEFAULT_CAPACITY = 16;

<*
 Deque is a dynamically resizable double-ended queue/ring-buffer.
 Being double-ended means that elements can be pushed or popped from
 either end of the same memory block, in any order,
 allowing the structure to act as both a stack and a queue.
*>
struct Deque (Printable)
{
	Type* entries;
	sz capacity;
	sz start;
	sz size;
	Allocator allocator;
}

<*
 @require capacity > 0 : "Capacity should be greater than 0"
 @require capacity < sz::max / Type::size : "Capacity too great"
*>
fn void Deque.init(&self, Allocator allocator, sz capacity = DEFAULT_CAPACITY)
{
	*self = { };
	self.entries = alloc::alloc_array(allocator, Type, capacity);
	self.capacity = capacity;
	self.allocator = allocator;
}

<*
 @require capacity > 0 : "Capacity should be greater than 0"
 @require capacity < sz::max / Type::size : "Capacity too great"
*>
fn void Deque.tinit(&self, sz capacity = DEFAULT_CAPACITY)
{
	*self = { };
	self.entries = mem::temp_array(Type, capacity);
	self.capacity = capacity;
	self.allocator = tmem;
}

<*
 Push an element to the back of the deque.
*>
fn void Deque.push(&self, Type element)
{
	self.reserve(1);
	sz idx = next_back_idx(self);
	self.entries[idx] = element;
	self.size++;
}

<*
 Push an element to the front of the deque.
*>
fn void Deque.push_front(&self, Type element)
{
	self.reserve(1);
	self.start = next_front_idx(self);
	self.entries[self.start] = element;
	self.size++;
}

<*
 Push multiple elements to the back of the deque.
*>
fn void Deque.push_all(&self, Type[] elements)
{
	foreach (el : elements)
	{
		self.push(el);
	}
}

<*
 Push multiple elements to the front of the deque.
*>
fn void Deque.push_front_all(&self, Type[] elements)
{
	foreach (el : elements)
	{
		self.push_front(el);
	}
}

<*
 Pop an element from the back of the deque.

 @return? NO_MORE_ELEMENT : "If size is 0"
*>
fn Type? Deque.pop(&self)
{
	if (@unlikely(self.is_empty())) return NO_MORE_ELEMENT~;
	self.size--;
	sz idx = next_back_idx(self);
	return self.entries[idx];
}

<*
 Pop an element from the front of the deque.

 @return? NO_MORE_ELEMENT : "If size is 0"
*>
fn Type? Deque.pop_first(&self)
{
	if (@unlikely(self.is_empty())) return NO_MORE_ELEMENT~;
	Type elem = self.entries[self.start];
	self.start = wrap_inc(self.start, self.capacity);
	self.size--;
	return elem;
}

<*
 Pop multiple elements from the front of the deque into 'target'.
 Count of the popped elements calculated by taking the 'min'
 on the 'target' length and deque's internal length
*>
fn Type[] Deque.pop_first_into(&self, Type[] target)
{
	if (@unlikely(self.is_empty())) return target[:0];
	sz len = math::min(target.len, self.size);
	foreach (&el : target[:len])
	{
		*el = _pop_front(self);
	}
	return target[:len];
}

<*
 Pop multiple elements from the back of the deque into 'target'.
 Count of the popped elements calculated by taking the 'min'
 on the 'target' length and deque's internal length

*>
fn Type[] Deque.pop_into(&self, Type[] target)
{
	if (@unlikely(self.is_empty())) return target[:0];
	sz len = math::min(target.len, self.size);
	foreach (&el : target[:len])
	{
		*el = _pop_back(self);
	}
	return target[:len];
}

<*
 Reserve enough space in the queue for at least the specified value.

 @param added : "Specifies how many extra capacity is required at least"
*>
fn void Deque.reserve(&self, sz added)
{
	sz diff = (self.size + added) - self.capacity;
	if (@unlikely(diff <= 0)) return;
	sz req_capacity = self.capacity + diff;

	assert(req_capacity > 0);
	assert(self.capacity < sz::max / 2);

	sz new_capacity = self.capacity * 2;
	while (new_capacity < req_capacity)
	{
		new_capacity *= 2;
	}
	_grow(self, new_capacity);
}

<*
 Makes deque's internal buffer contiguous and sets the 'capacity' to 'size'.

 @require self.capacity > self.size : "Capacity should be bigger than size"
*>
fn void Deque.shrink(&self)
{
	self.normalize();
	self.capacity = self.size;
	self.entries = alloc::realloc(self.allocator, self.entries, self.capacity * Type::size);
}

<*
 Makes deque's internal buffer contiguous.
 Same as 'shrink' but preserves current capacity.
*>
fn void Deque.normalize(&self)
{
	if (self.size < 1 || self.start < 1) return;
	// Simple case [ _, _, 1, 2, 3]
	if (self.start + self.size <= self.capacity)
	{
		self.entries[:self.size] = self.entries[self.start:self.size];
		self.start = 0;
		return;
	}
	@pool()
	{
		Type[] tbuf = mem::temp_array(Type, self.start);
		tbuf[..] = self.entries[0:self.start];
		sz after_start = self.capacity - self.start;
		self.entries[:after_start] = self.entries[self.start:after_start];
		self.entries[after_start:self.start] = tbuf[..];
		self.start = 0;
	};

}

<*
 Reset the deque's length and offset to zero, letting it write new elements over
 old memory, in effect clearing the accessible contents.
*>
fn void Deque.clear(&self) @inline
{
	self.size = 0;
	self.start = 0;
}

<*
 Free the memory that has been dynamically allocated for deque.
*>
fn void Deque.free(&self)
{
	if (@unlikely(!self.entries)) return;

	alloc::free(self.allocator, self.entries);
	self.entries = null;
	self.clear();
}

// --- Utilities ---

fn sz Deque.remain_space(Deque* self) @inline
{
	return self.capacity - self.size;
}

fn bool Deque.is_empty(Deque* self) @inline
{
	return self.size == 0;
}

<*
 @require i < self.size && i >= 0 && j < self.size && j >= 0 : `Access out of bounds`
*>
fn void Deque.swap(&self, sz i, sz j)
{
	i = wrap_add(self.start, i, self.capacity);
	j = wrap_add(self.start, j, self.capacity);
	@swap(self.entries[i], self.entries[j]);
}

<*
 Get an element at the front of the deque.

 @require self.size > 0 : "Size should be greater than 0"
*>
fn Type Deque.first(&self) @inline
{
	return self.entries[self.start];
}

<*
 Get a reference to an element at the front of the deque.

 @require self.size > 0 : "Size should be greater than 0"
*>
fn Type* Deque.first_ref(&self) @inline
{
	return &self.entries[self.start];
}

<*
 Get an element at the back of the deque.

 @require self.size > 0 : "Size should be greater than 0"
*>
fn Type Deque.last(&self) @inline
{
	return self.entries[curr_back_idx(self)];
}

<*
 Get a reference to an element at the back of the deque.

 @require self.size > 0 : "Size should be greater than 0"
*>
fn Type* Deque.last_ref(&self) @inline
{
	return &self.entries[curr_back_idx(self)];
}

<*
 @require index >= 0 && index < self.size : `Access out of bounds`
*>
fn Type Deque.get(&self, sz index) @operator([]) @inline
{
	sz idx = wrap_add(self.start, index, self.capacity);
	return self.entries[idx];
}

<*
 @require index >= 0 && index < self.size : `Access out of bounds`
*>
fn Type* Deque.get_ref(&self, sz index) @operator(&[]) @inline
{
	sz idx = wrap_add(self.start, index, self.capacity);
	return &self.entries[idx];
}

<*
 @require index >= 0 && index < self.size : `Access out of bounds`
*>
fn void Deque.set(&self, sz index, Type value) @operator([]=)
{
	sz idx = wrap_add(self.start, index, self.capacity);
	self.entries[idx] = value;
}

fn sz Deque.len(&self) @operator(len) @inline
{
	return self.size;
}

fn sz? Deque.to_format(&self, Formatter* fmt) @dynamic
{
	if (@unlikely(self.is_empty()))
	{
		return fmt.print("[]");
	}

	sz total = fmt.printf("[%s", self.entries[self.start])!;
	sz curr_idx = wrap_inc(self.start, self.capacity);

	for (sz i = 0; i < self.size - 1; ++i)
	{
		total += fmt.printf(", %s", self.entries[curr_idx])!;
		curr_idx = wrap_inc(curr_idx, self.capacity);
	}

	return total + fmt.print("]");
}

// --- Private API ---

fn void _grow(Deque* self, sz new_capacity) @private
{
	self.entries = alloc::realloc(self.allocator, self.entries, Type::size * new_capacity);
	sz old_capacity = self.capacity;

	if (self.start + self.size > old_capacity)
	{
		sz diff = old_capacity - self.start;
		self.entries[new_capacity - diff:diff] = self.entries[self.start:diff];
		self.start += new_capacity - old_capacity;
	}
	self.capacity = new_capacity;
}

fn sz next_back_idx(Deque* self) @inline @private
{
	return wrap_add(self.start, self.size, self.capacity);
}

fn sz curr_back_idx(Deque* self) @inline @private
{
	return wrap_add(self.start, self.size - 1, self.capacity);
}

fn sz next_front_idx(Deque* self) @inline @private
{
	return wrap_dec(self.start, self.capacity);
}

<*
 More optimized version of 'pop_back', when we know that the size is not 0.

 @require self.size > 0 : "Size should be greater than 0"
*>
fn Type _pop_back(Deque* self) @private
{
	self.size--;
	sz idx = next_back_idx(self);
	return self.entries[idx];
}

<*
 More optimized version of 'pop_front', when we know that the size is not 0.

 @require self.size > 0 : "Size should be greater than 0"
*>
fn Type _pop_front(Deque* self) @private
{
	Type elem = self.entries[self.start];
	self.start = wrap_inc(self.start, self.capacity);
	self.size--;
	return elem;
}

macro wrap_inc(sz val, sz boundary) @local
{
	return val == boundary - 1 ? 0 : val + 1;
}

macro wrap_dec(sz val, sz boundary) @local
{
	return val == 0 ? boundary - 1 : val - 1;
}

<*
 @require add >= 0
*>
macro wrap_add(sz val, sz add, sz boundary) @local
{
	return (val + add) % boundary;
}
