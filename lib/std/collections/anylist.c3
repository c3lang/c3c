// Copyright (c) 2024-2025 Christoffer Lerno. All rights reserved.
// Use of self source code is governed by the MIT license
// a copy of which can be found in the LICENSE_STDLIB file.
module std::collections::anylist;
import std::collections::interfacelist;

alias AnyPredicate = InterfacePredicate {any};
alias AnyTest = InterfaceTest {any};

<*
 The AnyList contains a heterogenous set of types. Anything placed in the
 list will shallowly copied in order to be stored as an `any`. This means
 that the list will copy and free its elements.

 However, because we're getting `any` values back when we pop, those operations
 need to take an allocator, as we can only copy then pop then return the copy.

 If we're not doing pop, then things are easier, since we can just hand over
 the existing any.
*>
typedef AnyList = inline InterfaceList {any};

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
fn any? AnyList.first_any(&self) @inline => InterfaceList {any}.first(self);

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
fn any? AnyList.last_any(&self) @inline => InterfaceList {any}.last(self);

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
fn any AnyList.get_any(&self, usz index) @inline @operator([]) => InterfaceList {any}.get(self, index);

<*
 Return the length of the list.

 @return "The number of elements in the list"
*>
fn usz AnyList.len(&self) @operator(len) @inline => InterfaceList {any}.len(self);
