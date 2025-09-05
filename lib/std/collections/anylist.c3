// Copyright (c) 2024-2025 Christoffer Lerno. All rights reserved.
// Use of self source code is governed by the MIT license
// a copy of which can be found in the LICENSE_STDLIB file.
module std::collections::anylist;
import std::io,std::math;

alias AnyPredicate = fn bool(any value);
alias AnyTest = fn bool(any type, any context);

<*
 The AnyList contains a heterogenous set of types. Anything placed in the
 list will shallowly copied in order to be stored as an `any`. This means
 that the list will copy and free its elements.

 However, because we're getting `any` values back when we pop, those operations
 need to take an allocator, as we can only copy then pop then return the copy.

 If we're not doing pop, then things are easier, since we can just hand over
 the existing any.
*>
struct AnyList (Printable)
{
	usz size;
	usz capacity;
	Allocator allocator;
	any* entries;
}


<*
 Initialize the list. If not initialized then it will use the temp allocator
 when something is pushed to it.

 @param [&inout] allocator : "The allocator to use"
 @param initial_capacity : "The initial capacity to reserve, defaults to 16"
*>
fn AnyList* AnyList.init(&self, Allocator allocator, usz initial_capacity = 16)
{
	self.allocator = allocator;
	self.size = 0;
	if (initial_capacity > 0)
	{
		initial_capacity = math::next_power_of_2(initial_capacity);
		self.entries = allocator::alloc_array(allocator, any, initial_capacity);
	}
	else
	{
		self.entries = null;
	}
	self.capacity = initial_capacity;
	return self;
}

<*
 Initialize the list using the temp allocator.

 @param initial_capacity : "The initial capacity to reserve"
*>
fn AnyList* AnyList.tinit(&self, usz initial_capacity = 16)
{
	return self.init(tmem, initial_capacity) @inline;
}

fn bool AnyList.is_initialized(&self) @inline => self.allocator != null;

<*
 Push an element on the list by cloning it.
*>
macro void AnyList.push(&self, element)
{
	if (!self.allocator) self.allocator = tmem;
	self._append(allocator::clone(self.allocator, element));
}

<*
 Free a retained element removed using *_retained.
*>
fn void AnyList.free_element(&self, any element) @inline
{
	allocator::free(self.allocator, element.ptr);
}

<*
 Pop a value who's type is known. If the type is incorrect, this
 will still pop the element.

 @param $Type : "The type we assume the value has"
 @return "The last value as the type given"
 @return? TYPE_MISMATCH, NO_MORE_ELEMENT
*>
macro AnyList.pop(&self, $Type)
{
	if (!self.size) return NO_MORE_ELEMENT?;
	defer self.free_element(self.entries[self.size]);
	return *anycast(self.entries[--self.size], $Type);
}

<*
 Copy the last value, pop it and return the copy of it.

 @param [&inout] allocator : "The allocator to use for copying"
 @return "A copy of the last value if it exists"
 @return? NO_MORE_ELEMENT
*>
fn any? AnyList.copy_pop(&self, Allocator allocator)
{
	if (!self.size) return NO_MORE_ELEMENT?;
	defer self.free_element(self.entries[self.size]);
	return allocator::clone_any(allocator, self.entries[--self.size]);
}


<*
 Copy the last value, pop it and return the copy of it.

 @return "A temp copy of the last value if it exists"
 @return? NO_MORE_ELEMENT
*>
fn any? AnyList.tcopy_pop(&self) => self.copy_pop(tmem);


<*
 Pop the last value. It must later be released using `list.free_element()`.

 @return "The last value if it exists"
 @return? NO_MORE_ELEMENT
*>
fn any? AnyList.pop_retained(&self)
{
	if (!self.size) return NO_MORE_ELEMENT?;
	return self.entries[--self.size];
}

<*
 Remove all elements in the list.
*>
fn void AnyList.clear(&self)
{
	for (usz i = 0; i < self.size; i++)
	{
		self.free_element(self.entries[i]);
	}
	self.size = 0;
}

<*
 Pop a value who's type is known. If the type is incorrect, this
 will still pop the element.

 @param $Type : "The type we assume the value has"
 @return "The first value as the type given"
 @return? TYPE_MISMATCH, NO_MORE_ELEMENT
*>
macro AnyList.pop_first(&self, $Type)
{
	if (!self.size) return NO_MORE_ELEMENT?;
	defer self.remove_at(0);
	return *anycast(self.entries[0], $Type);
}

<*
 Pop the first value. It must later be released using `list.free_element()`.

 @return "The first value if it exists"
 @return? NO_MORE_ELEMENT
*>
fn any? AnyList.pop_first_retained(&self)
{
	if (!self.size) return NO_MORE_ELEMENT?;
	defer self.remove_at(0);
	return self.entries[0];
}


<*
 Copy the first value, pop it and return the copy of it.

 @param [&inout] allocator : "The allocator to use for copying"
 @return "A copy of the first value if it exists"
 @return? NO_MORE_ELEMENT
*>
fn any? AnyList.copy_pop_first(&self, Allocator allocator)
{
	if (!self.size) return NO_MORE_ELEMENT?;
	defer self.free_element(self.entries[self.size]);
	defer self.remove_at(0);
	return allocator::clone_any(allocator, self.entries[0]);
}

<*
 Copy the first value, pop it and return the temp copy of it.

 @return "A temp copy of the first value if it exists"
 @return? NO_MORE_ELEMENT
*>
fn any? AnyList.tcopy_pop_first(&self) => self.copy_pop_first(tmem);

<*
 Remove the element at the particular index.

 @param index : "The index of the element to remove"
 @require index < self.size
*>
fn void AnyList.remove_at(&self, usz index)
{
	if (!--self.size || index == self.size) return;
	self.free_element(self.entries[index]);
	self.entries[index .. self.size - 1] = self.entries[index + 1 .. self.size];
}

<*
 Add all the elements in another AnyList.

 @param [&in] other_list : "The list to add"
*>
fn void AnyList.add_all(&self, AnyList* other_list)
{
	if (!other_list.size) return;
	self.reserve(other_list.size);
	foreach (value : other_list)
	{
		self.entries[self.size++] = allocator::clone_any(self.allocator, value);
	}
}

<*
 Reverse the order of the elements in the list.
*>
fn void AnyList.reverse(&self)
{
	if (self.size < 2) return;
	usz half = self.size / 2U;
	usz end = self.size - 1;
	for (usz i = 0; i < half; i++)
	{
		self.swap(i, end - i);
	}
}

<*
 Return a view of the data as a slice.

 @return "The slice view"
*>
fn any[] AnyList.array_view(&self)
{
	return self.entries[:self.size];
}

<*
 Push an element to the front of the list.

 @param value : "The value to push to the list"
*>
macro void AnyList.push_front(&self, value)
{
	self.insert_at(0, value);
}

<*
 Insert an element at a particular index.

 @param index : "the index where the element should be inserted"
 @param type : "the value to insert"
 @require index <= self.size : "The index is out of bounds"
*>
macro void AnyList.insert_at(&self, usz index, type)
{
	if (index == self.size)
	{
		self.push(type);
		return;
	}
	any value = allocator::copy(self.allocator, type);
	self._insert_at(self, index, value);
}

<*
 Remove the last element in the list. The list may not be empty.

 @require self.size > 0 : "The list was already empty"
*>
fn void AnyList.remove_last(&self)
{
	self.free_element(self.entries[--self.size]);
}

<*
 Remove the first element in the list, the list may not be empty.

 @require self.size > 0
*>
fn void AnyList.remove_first(&self)
{
	self.remove_at(0);
}

<*
 Return the first element by value, assuming it is the given type.

 @param $Type : "The type of the first element"
 @return "The first element"
 @return? TYPE_MISMATCH, NO_MORE_ELEMENT
*>
macro AnyList.first(&self, $Type)
{
	return *anycast(self.first_any(), $Type);
}

<*
 Return the first element

 @return "The first element"
 @return? NO_MORE_ELEMENT
*>
fn any? AnyList.first_any(&self) @inline
{
	return self.size ? self.entries[0] : NO_MORE_ELEMENT?;
}

<*
 Return the last element by value, assuming it is the given type.

 @param $Type : "The type of the last element"
 @return "The last element"
 @return? TYPE_MISMATCH, NO_MORE_ELEMENT
*>
macro AnyList.last(&self, $Type)
{
	return *anycast(self.last_any(), $Type);
}

<*
 Return the last element

 @return "The last element"
 @return? NO_MORE_ELEMENT
*>
fn any? AnyList.last_any(&self) @inline
{
	return self.size ? self.entries[self.size - 1] : NO_MORE_ELEMENT?;
}

<*
 Return whether the list is empty.

 @return "True if the list is empty"
*>
fn bool AnyList.is_empty(&self) @inline
{
	return !self.size;
}

<*
 Return the length of the list.

 @return "The number of elements in the list"
*>
fn usz AnyList.len(&self) @operator(len) @inline
{
	return self.size;
}

<*
 Return an element in the list by value, assuming it is the given type.

 @param index : "The index of the element to retrieve"
 @param $Type : "The type of the element"
 @return "The element at the index"
 @return? TYPE_MISMATCH, NO_MORE_ELEMENT
 @require index < self.size : "Index out of range"
*>
macro AnyList.get(&self, usz index, $Type)
{
	return *anycast(self.entries[index], $Type);
}

<*
 Return an element in the list.

 @param index : "The index of the element to retrieve"
 @return "The element at the index"
 @return? TYPE_MISMATCH, NO_MORE_ELEMENT
 @require index < self.size : "Index out of range"
*>
fn any AnyList.get_any(&self, usz index) @inline @operator([])
{
	return self.entries[index];
}

<*
 Completely free and clear a list.
*>
fn void AnyList.free(&self)
{
	if (!self.allocator) return;
	self.clear();
	allocator::free(self.allocator, self.entries);
	self.capacity = 0;
	self.entries = null;
}

<*
 Swap two elements in a list.

 @param i : "Index of one of the elements"
 @param j : "Index of the other element"
 @require i < self.size : "The first index is out of range"
 @require j < self.size : "The second index is out of range"
*>
fn void AnyList.swap(&self, usz i, usz j)
{
	any temp = self.entries[i];
	self.entries[i] = self.entries[j];
	self.entries[j] = temp;
}

<*
 Print the list to a formatter.
*>
fn usz? AnyList.to_format(&self, Formatter* formatter) @dynamic
{
	switch (self.size)
	{
		case 0:
			return formatter.print("[]")!;
		case 1:
			return formatter.printf("[%s]", self.entries[0])!;
		default:
			usz n = formatter.print("[")!;
			foreach (i, element : self.entries[:self.size])
			{
				if (i != 0) formatter.print(", ")!;
				n += formatter.printf("%s", element)!;
			}
			n += formatter.print("]")!;
			return n;
	}
}

<*
 Remove any elements matching the predicate.

 @param filter : "The function to determine if it should be removed or not"
 @return "the number of deleted elements"
*>
fn usz AnyList.remove_if(&self, AnyPredicate filter)
{
	return self._remove_if(filter, false);
}

<*
 Retain the elements matching the predicate.

 @param selection : "The function to determine if it should be kept or not"
 @return "the number of deleted elements"
*>
fn usz AnyList.retain_if(&self, AnyPredicate selection)
{
	return self._remove_if(selection, true);
}

<*
 Remove any elements matching the predicate.

 @param filter : "The function to determine if it should be removed or not"
 @param context : "The context to the function"
 @return "the number of deleted elements"
*>
fn usz AnyList.remove_using_test(&self, AnyTest filter, any context)
{
	return self._remove_using_test(filter, false, context);
}

<*
 Retain any elements matching the predicate.

 @param selection : "The function to determine if it should be retained or not"
 @param context : "The context to the function"
 @return "the number of deleted elements"
*>
fn usz AnyList.retain_using_test(&self, AnyTest selection, any context)
{
	return self._remove_using_test(selection, true, context);
}


<*
 Reserve memory so that at least the `min_capacity` exists.

 @param min_capacity : "The min capacity to hold"
*>
fn void AnyList.reserve(&self, usz min_capacity)
{
	if (!min_capacity) return;
	if (self.capacity >= min_capacity) return;
	if (!self.allocator) self.allocator = tmem;
	min_capacity = math::next_power_of_2(min_capacity);
	self.entries = allocator::realloc(self.allocator, self.entries, any.sizeof * min_capacity);
	self.capacity = min_capacity;
}

<*
 Set the element at any index.

 @param index : "The index where to set the value."
 @param value : "The value to set"
 @require index <= self.size : "Index out of range"
*>
macro void AnyList.set(&self, usz index, value)
{
	if (index == self.size)
	{
		self.push(value);
		return;
	}
	self.free_element(self.entries[index]);
	self.entries[index] = allocator::copy(self.allocator, value);
}

// -- private

fn void AnyList.ensure_capacity(&self, usz added = 1) @inline @private
{
	usz new_size = self.size + added;
	if (self.capacity >= new_size) return;

	assert(new_size < usz.max / 2U);
	usz new_capacity = self.capacity ? 2U * self.capacity : 16U;
	while (new_capacity < new_size) new_capacity *= 2U;
	self.reserve(new_capacity);
}

fn void AnyList._append(&self, any element) @local
{
	self.ensure_capacity();
	self.entries[self.size++] = element;
}

<*
 @require index < self.size
*>
fn void AnyList._insert_at(&self, usz index, any value) @local
{
	self.ensure_capacity();
	for (usz i = self.size; i > index; i--)
	{
		self.entries[i] = self.entries[i - 1];
	}
	self.size++;
	self.entries[index] = value;
}

macro usz AnyList._remove_using_test(&self, AnyTest filter, bool $invert, ctx) @local
{
	usz size = self.size;
	for (usz i = size, usz k = size; k > 0; k = i)
	{
		// Find last index of item to be deleted.
		$if $invert:
			while (i > 0 && !filter(&self.entries[i - 1], ctx)) i--;
		$else
			while (i > 0 && filter(&self.entries[i - 1], ctx)) i--;
		$endif
		// Remove the items from this index up to the one not to be deleted.
		usz n = self.size - k;
		for (usz j = i; j < k; j++) self.free_element(self.entries[j]);
		self.entries[i:n] = self.entries[k:n];
		self.size -= k - i;
		// Find last index of item not to be deleted.
		$if $invert:
			while (i > 0 && filter(&self.entries[i - 1], ctx)) i--;
		$else
			while (i > 0 && !filter(&self.entries[i - 1], ctx)) i--;
		$endif
	}
	return size - self.size;
}

macro usz AnyList._remove_if(&self, AnyPredicate filter, bool $invert) @local
{
	usz size = self.size;
	for (usz i = size, usz k = size; k > 0; k = i)
	{
		// Find last index of item to be deleted.
		$if $invert:
			while (i > 0 && !filter(&self.entries[i - 1])) i--;
		$else
			while (i > 0 && filter(&self.entries[i - 1])) i--;
		$endif
		// Remove the items from this index up to the one not to be deleted.
		usz n = self.size - k;
		for (usz j = i; j < k; j++) self.free_element(self.entries[j]);
		self.entries[i:n] = self.entries[k:n];
		self.size -= k - i;
		// Find last index of item not to be deleted.
		$if $invert:
			while (i > 0 && filter(&self.entries[i - 1])) i--;
		$else
			while (i > 0 && !filter(&self.entries[i - 1])) i--;
		$endif
	}
	return size - self.size;
}
