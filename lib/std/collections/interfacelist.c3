// Copyright (c) 2024-2025 Christoffer Lerno. All rights reserved.
// Use of self source code is governed by the MIT license
// a copy of which can be found in the LICENSE_STDLIB file.
<*
 @require Type.kindof == INTERFACE || Type.kindof == ANY : "The kind of an interfacelist must be an interface or `any`"
*>
module std::collections::interfacelist <Type>;
import std::io,std::math;

alias InterfacePredicate = fn bool(Type value);
alias InterfaceTest = fn bool(Type type, Type context);

<*
 The InterfaceList contains a heterogenous set of types implementing an interface. anything placed in the
 list will shallowly copied in order to be stored as the interface. This means
 that the list will copy and free its elements.

 However, because we're getting interface values back when we pop, those operations
 need to take an allocator, as we can only copy then pop then return the copy.

 If we're not doing pop, then things are easier, since we can just hand over
 the existing value.
*>
struct InterfaceList (Printable)
{
	usz size;
	usz capacity;
	Allocator allocator;
	Type* entries;
}


<*
 Initialize the list. If not initialized then it will use the temp allocator
 when something is pushed to it.

 @param [&inout] allocator : "The allocator to use"
 @param initial_capacity : "The initial capacity to reserve, defaults to 16"
*>
fn InterfaceList* InterfaceList.init(&self, Allocator allocator, usz initial_capacity = 16)
{
	self.allocator = allocator;
	self.size = 0;
	if (initial_capacity > 0)
	{
		initial_capacity = math::next_power_of_2(initial_capacity);
		self.entries = allocator::alloc_array(allocator, Type, initial_capacity);
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
fn InterfaceList* InterfaceList.tinit(&self, usz initial_capacity = 16)
{
	return self.init(tmem, initial_capacity) @inline;
}

fn bool InterfaceList.is_initialized(&self) @inline => self.allocator != null;

<*
 Push an element on the list by cloning it.
 @require $defined(Type t = &element) : "Element must implement the interface"
*>
macro void InterfaceList.push(&self, element)
{
	if (!self.allocator) self.allocator = tmem;
	self._append(allocator::clone(self.allocator, element));
}

<*
 Free a retained element removed using *_retained.
*>
fn void InterfaceList.free_element(&self, Type element) @inline
{
	allocator::free(self.allocator, element.ptr);
}

<*
 Copy the last value, pop it and return the copy of it.

 @param [&inout] allocator : "The allocator to use for copying"
 @return "A copy of the last value if it exists"
 @return? NO_MORE_ELEMENT
*>
fn Type? InterfaceList.copy_pop(&self, Allocator allocator)
{
	if (!self.size) return NO_MORE_ELEMENT~;
	defer self.free_element(self.entries[self.size]);
	return (Type)allocator::clone_any(allocator, self.entries[--self.size]);
}

<*
 Copy the last value, pop it and return the copy of it.

 @return "A temp copy of the last value if it exists"
 @return? NO_MORE_ELEMENT
*>
fn Type? InterfaceList.tcopy_pop(&self) => self.copy_pop(tmem);

<*
 Pop the last value. It must later be released using `list.free_element()`.

 @return "The last value if it exists"
 @return? NO_MORE_ELEMENT
*>
fn Type? InterfaceList.pop_retained(&self)
{
	if (!self.size) return NO_MORE_ELEMENT~;
	return self.entries[--self.size];
}

<*
 Remove all elements in the list.
*>
fn void InterfaceList.clear(&self)
{
	for (usz i = 0; i < self.size; i++)
	{
		self.free_element(self.entries[i]);
	}
	self.size = 0;
}

<*
 Pop the first value. It must later be released using `list.free_element()`.

 @return "The first value if it exists"
 @return? NO_MORE_ELEMENT
*>
fn Type? InterfaceList.pop_first_retained(&self)
{
	if (!self.size) return NO_MORE_ELEMENT~;
	defer self.remove_at(0);
	return self.entries[0];
}

<*
 Copy the first value, pop it and return the copy of it.

 @param [&inout] allocator : "The allocator to use for copying"
 @return "A copy of the first value if it exists"
 @return? NO_MORE_ELEMENT
*>
fn Type? InterfaceList.copy_pop_first(&self, Allocator allocator)
{
	if (!self.size) return NO_MORE_ELEMENT~;
	defer self.free_element(self.entries[self.size]);
	defer self.remove_at(0);
	return (Type)allocator::clone_any(allocator, self.entries[0]);
}

<*
 Copy the first value, pop it and return the temp copy of it.

 @return "A temp copy of the first value if it exists"
 @return? NO_MORE_ELEMENT
*>
fn Type? InterfaceList.tcopy_pop_first(&self) => self.copy_pop_first(tmem);

<*
 Remove the element at the particular index.

 @param index : "The index of the element to remove"
 @require index < self.size
*>
fn void InterfaceList.remove_at(&self, usz index)
{
	if (!--self.size || index == self.size) return;
	self.free_element(self.entries[index]);
	self.entries[index .. self.size - 1] = self.entries[index + 1 .. self.size];
}

<*
 Add all the elements in another InterfaceList.

 @param [&in] other_list : "The list to add"
*>
fn void InterfaceList.add_all(&self, InterfaceList* other_list)
{
	if (!other_list.size) return;
	self.reserve(other_list.size);
	foreach (value : other_list)
	{
		self.entries[self.size++] = (Type)allocator::clone_any(self.allocator, value);
	}
}

<*
 Reverse the order of the elements in the list.
*>
fn void InterfaceList.reverse(&self)
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
fn Type[] InterfaceList.array_view(&self)
{
	return self.entries[:self.size];
}

<*
 Push an element to the front of the list.

 @param value : "The value to push to the list"
 @require $defined(Type t = &value) : "Value must implement the interface"
*>
macro void InterfaceList.push_front(&self, value)
{
	self.insert_at(0, value);
}

<*
 Insert an element at a particular index.

 @param index : "the index where the element should be inserted"
 @param type : "the value to insert"
 @require index <= self.size : "The index is out of bounds"
 @require $defined(Type t = &type) : "Type must implement the interface"
*>
macro void InterfaceList.insert_at(&self, usz index, type)
{
	if (index == self.size)
	{
		self.push(type);
		return;
	}
	Type value = allocator::clone(self.allocator, type);
	self._insert_at(self, index, value);
}

<*
 Remove the last element in the list. The list may not be empty.

 @require self.size > 0 : "The list was already empty"
*>
fn void InterfaceList.remove_last(&self)
{
	self.free_element(self.entries[--self.size]);
}

<*
 Remove the first element in the list, the list may not be empty.

 @require self.size > 0
*>
fn void InterfaceList.remove_first(&self)
{
	self.remove_at(0);
}

<*
 Return the first element

 @return "The first element"
 @return? NO_MORE_ELEMENT
*>
fn Type? InterfaceList.first(&self) @inline
{
	return self.size ? self.entries[0] : NO_MORE_ELEMENT~;
}

<*
 Return the last element

 @return "The last element"
 @return? NO_MORE_ELEMENT
*>
fn Type? InterfaceList.last(&self) @inline
{
	return self.size ? self.entries[self.size - 1] : NO_MORE_ELEMENT~;
}

<*
 Return whether the list is empty.

 @return "True if the list is empty"
*>
fn bool InterfaceList.is_empty(&self) @inline
{
	return !self.size;
}

<*
 Return the length of the list.

 @return "The number of elements in the list"
*>
fn usz InterfaceList.len(&self) @operator(len) @inline
{
	return self.size;
}

<*
 Return an element in the list.

 @param index : "The index of the element to retrieve"
 @return "The element at the index"
 @return? TYPE_MISMATCH, NO_MORE_ELEMENT
 @require index < self.size : "Index out of range"
*>
fn Type InterfaceList.get(&self, usz index) @inline @operator([])
{
	return self.entries[index];
}

<*
 Completely free and clear a list.
*>
fn void InterfaceList.free(&self)
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
fn void InterfaceList.swap(&self, usz i, usz j)
{
	Type temp = self.entries[i];
	self.entries[i] = self.entries[j];
	self.entries[j] = temp;
}

<*
 Print the list to a formatter.
*>
fn usz? InterfaceList.to_format(&self, Formatter* formatter) @dynamic
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
 Remove Type elements matching the predicate.

 @param filter : "The function to determine if it should be removed or not"
 @return "the number of deleted elements"
*>
fn usz InterfaceList.remove_if(&self, InterfacePredicate filter)
{
	return self._remove_if(filter, false);
}

<*
 Retain the elements matching the predicate.

 @param selection : "The function to determine if it should be kept or not"
 @return "the number of deleted elements"
*>
fn usz InterfaceList.retain_if(&self, InterfacePredicate selection)
{
	return self._remove_if(selection, true);
}

<*
 Remove Type elements matching the predicate.

 @param filter : "The function to determine if it should be removed or not"
 @param context : "The context to the function"
 @return "the number of deleted elements"
*>
fn usz InterfaceList.remove_using_test(&self, InterfaceTest filter, Type context)
{
	return self._remove_using_test(filter, false, context);
}

<*
 Retain Type elements matching the predicate.

 @param selection : "The function to determine if it should be retained or not"
 @param context : "The context to the function"
 @return "the number of deleted elements"
*>
fn usz InterfaceList.retain_using_test(&self, InterfaceTest selection, Type context)
{
	return self._remove_using_test(selection, true, context);
}

<*
 Reserve memory so that at least the `min_capacity` exists.

 @param min_capacity : "The min capacity to hold"
*>
fn void InterfaceList.reserve(&self, usz min_capacity)
{
	if (!min_capacity) return;
	if (self.capacity >= min_capacity) return;
	if (!self.allocator) self.allocator = tmem;
	min_capacity = math::next_power_of_2(min_capacity);
	self.entries = allocator::realloc(self.allocator, self.entries, Type.sizeof * min_capacity);
	self.capacity = min_capacity;
}

<*
 Set the element at Type index.

 @param index : "The index where to set the value."
 @param value : "The value to set"
 @require index <= self.size : "Index out of range"
 @require $defined(Type t = &value) : "Value must implement the interface"
*>
macro void InterfaceList.set(&self, usz index, value)
{
	if (index == self.size)
	{
		self.push(value);
		return;
	}
	self.free_element(self.entries[index]);
	self.entries[index] = allocator::clone(self.allocator, value);
}

// -- private

fn void InterfaceList.ensure_capacity(&self, usz added = 1) @inline @private
{
	usz new_size = self.size + added;
	if (self.capacity >= new_size) return;

	assert(new_size < usz.max / 2U);
	usz new_capacity = self.capacity ? 2U * self.capacity : 16U;
	while (new_capacity < new_size) new_capacity *= 2U;
	self.reserve(new_capacity);
}

fn void InterfaceList._append(&self, Type element) @local
{
	self.ensure_capacity();
	self.entries[self.size++] = element;
}

<*
 @require index < self.size
*>
fn void InterfaceList._insert_at(&self, usz index, Type value) @local
{
	self.ensure_capacity();
	for (usz i = self.size; i > index; i--)
	{
		self.entries[i] = self.entries[i - 1];
	}
	self.size++;
	self.entries[index] = value;
}

macro usz InterfaceList._remove_using_test(&self, InterfaceTest filter, bool $invert, ctx) @local
{
	usz size = self.size;
	for (usz i = size, usz k = size; k > 0; k = i)
	{
		// Find last index of item to be deleted.
		$if $invert:
			while (i > 0 && !filter(self.entries[i - 1], ctx)) i--;
		$else
			while (i > 0 && filter(self.entries[i - 1], ctx)) i--;
		$endif
		// Remove the items from this index up to the one not to be deleted.
		usz n = self.size - k;
		for (usz j = i; j < k; j++) self.free_element(self.entries[j]);
		self.entries[i:n] = self.entries[k:n];
		self.size -= k - i;
		// Find last index of item not to be deleted.
		$if $invert:
			while (i > 0 && filter(self.entries[i - 1], ctx)) i--;
		$else
			while (i > 0 && !filter(self.entries[i - 1], ctx)) i--;
		$endif
	}
	return size - self.size;
}

macro usz InterfaceList._remove_if(&self, InterfacePredicate filter, bool $invert) @local
{
	usz size = self.size;
	for (usz i = size, usz k = size; k > 0; k = i)
	{
		// Find last index of item to be deleted.
		$if $invert:
			while (i > 0 && !filter(self.entries[i - 1])) i--;
		$else
			while (i > 0 && filter(self.entries[i - 1])) i--;
		$endif
		// Remove the items from this index up to the one not to be deleted.
		usz n = self.size - k;
		for (usz j = i; j < k; j++) self.free_element(self.entries[j]);
		self.entries[i:n] = self.entries[k:n];
		self.size -= k - i;
		// Find last index of item not to be deleted.
		$if $invert:
			while (i > 0 && filter(self.entries[i - 1])) i--;
		$else
			while (i > 0 && !filter(self.entries[i - 1])) i--;
		$endif
	}
	return size - self.size;
}
