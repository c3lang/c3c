//  sortedmap.c3
//  An sorted map using a skip list for C3.
//
//  Copyright (c) 2026 Christian Reifberger
//
//  Permission is hereby granted, free of charge, to any person obtaining a copy
//  of this software and associated documentation files (the "Software"), to deal
//  in the Software without restriction, including without limitation the rights
//  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
//  copies of the Software, and to permit persons to whom the Software is
//  furnished to do so, subject to the following conditions:
//
//  The above copyright notice and this permission notice shall be included in all
//  copies or substantial portions of the Software.
//
//  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
//  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
//  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
//  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
//  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
//  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
//  SOFTWARE.

module std::collections::sortedmap <Key, Value>;
import std::io, std::math::random;
import std::core::mem::alloc;


const int MAX_LEVEL = 32;

int dummy @local;
const Allocator ONHEAP_ALLOCATOR = (Allocator)&dummy;

struct SkipNode
{
	Key key;
	Value value;
	int level;
	SkipNode*[*] forward;
}

// CmpFn is a comparator: returns true if a should come before b in the list.
alias CmpFn = fn bool(Key, Key);

struct Entry
{
	Key key;
	Value value;
}

<*
 @require types::@comparable_value((Key){}) : "The Key must support ordered comparison"
*>
struct SortedMap (Printable) @mustinit
{
	Allocator allocator;
	SkipNode* head;
	int current_level;
	sz count;
	SimpleRandom rng;
	CmpFn custom_cmp; // null = use built-in less() ordering
}

const SortedMap ONHEAP = { .allocator = ONHEAP_ALLOCATOR };

struct SortedMapIterator
{
	SortedMap* map;
	SkipNode* current;
	sz index;
}

struct SortedMapRangeIterator
{
	SortedMap* map;
	SkipNode* start_node;
	SkipNode* current;
	sz current_idx;
	sz count;
}

// Returns true if 'a' should come before 'b' in the list.
macro bool sortedmap_cmp(custom_cmp, a, b) @private
{
	if (custom_cmp != null) return custom_cmp(a, b);
	return less(a, b);
}

<*
 @require !self.is_initialized() : "Map was already initialized"
*>
fn SortedMap* SortedMap.init(&self, Allocator allocator)
{
	*self = { .allocator = allocator };
	// Single allocation for head + forward slice, calloc for zeroing of slice pointers
	self.head = alloc::new_with_padding(allocator, SkipNode, SkipNode*[MAX_LEVEL]::size)!!;
	random::seed_entropy(&self.rng);
	return self;
}

<*
 @require !self.is_initialized() : "Map was already initialized"
*>
fn SortedMap* SortedMap.tinit(&self)
{
	return self.init(tmem) @inline;
}

<*
 Init with a custom comparator. The comparator replaces the built-in ordering.
 @param cmp_fn : "Returns true if a should come before b in iteration order."
 @require !self.is_initialized() : "Map was already initialized"
*>
fn SortedMap* SortedMap.init_with_cmp(&self, Allocator allocator, CmpFn cmp_fn)
{
	self.init(allocator);
	self.custom_cmp = cmp_fn;
	return self;
}

<*
 @require !self.is_initialized() : "Map was already initialized"
*>
fn SortedMap* SortedMap.tinit_with_cmp(&self, CmpFn cmp_fn)
{
	return self.init_with_cmp(tmem, cmp_fn) @inline;
}

fn bool SortedMap.is_initialized(&self)
{
	return self.allocator && self.allocator.ptr != &dummy;
}

fn int SortedMap.random_level(&self) 
{
	int limit_level = min(MAX_LEVEL, self.current_level + 2);
	int level = 1;
	while (level < limit_level && (self.rng.next_int() & 1) == 0) level++;
	return level;
}

<*
 Insert or update the value associated with key.
*>
fn bool SortedMap.set(&self, Key key, Value value) @operator([]=)
{
	// If the map isn't initialized, use the defaults to initialize it.
	switch (self.allocator.ptr)
	{
		case &dummy:
			self.init(mem);
		case null:
			self.tinit();
		default:
			break;
	}
	SkipNode*[MAX_LEVEL] update;
	SkipNode* current = self.head;

	for (int i = self.current_level - 1; i >= 0; i--)
	{
		while (current.forward[i] && sortedmap_cmp(self.custom_cmp, current.forward[i].key, key))
		{
			current = current.forward[i];
		}
		update[i] = current;
	}

	current = current.forward[0];

	// Key already exists -- update value
	if (current && !sortedmap_cmp(self.custom_cmp, current.key, key) && !sortedmap_cmp(self.custom_cmp, key, current.key))
	{
		current.value = value;
		return true;
	}

	// Insert new node
	int new_level = self.random_level();
	if (new_level > self.current_level)
	{
		update[self.current_level..new_level - 1] = self.head;
		self.current_level = new_level;
	}

	// Single allocation for node + forward slice
	SkipNode* node = alloc::alloc_with_padding(self.allocator, SkipNode, new_level * SkipNode*::size)!!;
	*node = { .key = key, .value = value, .level = new_level };

	for (int i = 0; i < new_level; i++)
	{
		node.forward[i] = update[i].forward[i];
		update[i].forward[i] = node;
	}

	self.count++;
	return false;
}

<*
 @return "the value for key, or NOT_FOUND if absent"
*>
fn Value? SortedMap.get(&self, Key key) @operator([])
{
	SkipNode* current = self.head;
	for (int i = self.current_level - 1; i >= 0; i--)
	{
		while (current.forward[i] && sortedmap_cmp(self.custom_cmp, current.forward[i].key, key))
		{
			current = current.forward[i];
		}
	}
	current = current.forward[0];
	if (current && !sortedmap_cmp(self.custom_cmp, current.key, key) && !sortedmap_cmp(self.custom_cmp, key, current.key))
	{
		return current.value;
	}
	return NOT_FOUND~;
}

<*
 @return "a pointer to the value for key, or NOT_FOUND if absent"
*>
fn Value*? SortedMap.get_ref(&self, Key key) @operator(&[])
{
	SkipNode* current = self.head;
	for (int i = self.current_level - 1; i >= 0; i--)
	{
		while (current.forward[i] && sortedmap_cmp(self.custom_cmp, current.forward[i].key, key))
		{
			current = current.forward[i];
		}
	}
	current = current.forward[0];
	if (current && !sortedmap_cmp(self.custom_cmp, current.key, key) && !sortedmap_cmp(self.custom_cmp, key, current.key))
	{
		return &current.value;
	}
	return NOT_FOUND~;
}

<*
 Returns the value for key; inserts and returns #default_value if key is absent.
 #default_value is only evaluated if the key doesn't exist.

 @require $defined(Value val = #default_value)
*>
macro Value SortedMap.@get_or_set(&self, Key key, Value #default_value)
{
	if (try v = self.get(key)) return v;
	Value val = #default_value;
	self.set(key, val);
	return val;
}

<*
 @return "true if key exists in the map"
*>
fn bool SortedMap.has_key(&self, Key key) => @ok(self.get(key));

<*
 @return "true if the key was found and removed"
*>
fn bool SortedMap.remove(&self, Key key)
{
	SkipNode*[MAX_LEVEL] update;
	SkipNode* current = self.head;

	for (int i = self.current_level - 1; i >= 0; i--)
	{
		while (current.forward[i] && sortedmap_cmp(self.custom_cmp, current.forward[i].key, key))
		{
			current = current.forward[i];
		}
		update[i] = current;
	}

	current = current.forward[0];

	if (!current || sortedmap_cmp(self.custom_cmp, current.key, key) || sortedmap_cmp(self.custom_cmp, key, current.key)) return false;

	for (int i = 0; i < self.current_level; i++)
	{
		if (update[i].forward[i] != current) break;
		update[i].forward[i] = current.forward[i];
	}

	alloc::free(self.allocator, current);

	while (self.current_level > 0 && !self.head.forward[self.current_level - 1])
	{
		self.current_level--;
	}

	self.count--;
	return true;
}

<*
 @return "the first key in iteration order, or NOT_FOUND if empty"
*>
fn Key? SortedMap.first_key(&self)
{
	if (!self.head.forward[0]) return NOT_FOUND~;
	return self.head.forward[0].key;
}

<*
 @return "the last key in iteration order, or NOT_FOUND if empty"
*>
fn Key? SortedMap.last_key(&self)
{
	SkipNode* current = self.head;
	for (int i = self.current_level - 1; i >= 0; i--)
	{
		while (current.forward[i] != null) current = current.forward[i];
	}
	if (current == self.head) return NOT_FOUND~;
	return current.key;
}

<*
 Remove and return the first entry in iteration order.
 @return "the first Entry, or NOT_FOUND if empty"
 @return? NOT_FOUND
*>
fn Entry? SortedMap.pop_first(&self)
{
	SkipNode* node = self.head.forward[0];
	if (!node) return NOT_FOUND~;
	Entry result = { node.key, node.value };
	self.remove(node.key);
	return result;
}

<*
 Remove and return the last entry in iteration order.
 @return "the last Entry, or NOT_FOUND if empty"
 @return? NOT_FOUND
*>
fn Entry? SortedMap.pop_last(&self)
{
	if (!self.head.forward[0]) return NOT_FOUND~;
	Key k = self.last_key()!!;
	Value v = self.get(k)!!;
	self.remove(k);
	return { k, v };
}

// --- Floor / ceiling / lower / higher ---
//
// All four operate in the map's iteration (cmp) order.

<*
 @return "the greatest key <= key in cmp order, or NOT_FOUND"
 @return? NOT_FOUND
*>
fn Key? SortedMap.floor_key(&self, Key key)
{
	SkipNode* current = self.head;
	for (int i = self.current_level - 1; i >= 0; i--)
	{
		while (current.forward[i] && sortedmap_cmp(self.custom_cmp, current.forward[i].key, key))
		{
			current = current.forward[i];
		}
	}
	// current is the last node strictly before key; check for exact match
	SkipNode* next = current.forward[0];
	if (next && !sortedmap_cmp(self.custom_cmp, next.key, key) && !sortedmap_cmp(self.custom_cmp, key, next.key)) return next.key;
	if (current != self.head) return current.key;
	return NOT_FOUND~;
}

<*
 @return "the least key >= key in cmp order, or NOT_FOUND"
 @return? NOT_FOUND
*>
fn Key? SortedMap.ceiling_key(&self, Key key)
{
	SkipNode* current = self.head;
	for (int i = self.current_level - 1; i >= 0; i--)
	{
		while (current.forward[i] && sortedmap_cmp(self.custom_cmp, current.forward[i].key, key))
		{
			current = current.forward[i];
		}
	}
	SkipNode* next = current.forward[0];
	if (!next) return NOT_FOUND~;
	return next.key;
}

<*
 @return "the greatest key strictly before key in cmp order, or NOT_FOUND"
 @return? NOT_FOUND
*>
fn Key? SortedMap.lower_key(&self, Key key)
{
	SkipNode* current = self.head;
	for (int i = self.current_level - 1; i >= 0; i--)
	{
		while (current.forward[i] && sortedmap_cmp(self.custom_cmp, current.forward[i].key, key))
		{
			current = current.forward[i];
		}
	}
	return current == self.head ? NOT_FOUND~ : current.key;
}

<*
 @return "the least key strictly after key in cmp order, or NOT_FOUND"
 @return? NOT_FOUND
*>
fn Key? SortedMap.higher_key(&self, Key key)
{
	SkipNode* current = self.head;
	for (int i = self.current_level - 1; i >= 0; i--)
	{
		while (current.forward[i] != null && sortedmap_cmp(self.custom_cmp, current.forward[i].key, key))
		{
			current = current.forward[i];
		}
	}
	SkipNode* next = current.forward[0];
	if (!next) return NOT_FOUND~;
	// If next equals key, advance one more
	if (!sortedmap_cmp(self.custom_cmp, next.key, key) && !sortedmap_cmp(self.custom_cmp, key, next.key)) next = next.forward[0];
	return next ? next.key : NOT_FOUND~;
}

fn sz SortedMap.len(&self) @inline => self.count;

fn bool SortedMap.is_empty(&self) @inline => !self.count;

fn void SortedMap.clear(&self)
{
	if (!self.allocator) return;
	SkipNode* node = self.head.forward[0];
	while (node)
	{
		SkipNode* next = node.forward[0];
		alloc::free(self.allocator, node);
		node = next;
	}
	self.head.forward[..self.current_level] = null;
	self.count = 0;
	self.current_level = 0;
}

fn void SortedMap.free(&self)
{
	self.clear();
	alloc::free(self.allocator, self.head);
	self.head = null;
}

fn sz? SortedMap.to_format(&self, Formatter* f) @dynamic
{
	sz len;
	len += f.print("{ ")!;
	SkipNode* node = self.head ? self.head.forward[0] : null;
	while (node)
	{
		if (len > 2) len += f.print(", ")!;
		len += f.printf("%s: %s", node.key, node.value)!;
		node = node.forward[0];
	}
	return len + f.print(" }");
}

<*
 Copy all entries of this map into a new map backed by allocator.
 The clone inherits the custom comparator (if any).
*>
fn SortedMap SortedMap.clone(&self, Allocator allocator)
{
	SortedMap result;
	result.init_with_cmp(allocator, self.custom_cmp);
	SkipNode* node = self.head.forward[0];
	while (node)
	{
		result.set(node.key, node.value);
		node = node.forward[0];
	}
	return result;
}

fn SortedMap SortedMap.tclone(&self)
{
	return self.clone(tmem) @inline;
}

<*
 Insert all entries from other into self, overwriting on key collision.
*>
fn void SortedMap.merge(&self, SortedMap* other)
{
	SkipNode* node = other.head.forward[0];
	while (node)
	{
		self.set(node.key, node.value);
		node = node.forward[0];
	}
}

// --- Iteration ---

<*
 Returns an iterator that yields Entry values in sorted key order.
 Use with: foreach (entry : m.iter()) { ... }
*>
fn SortedMapIterator SortedMap.iter(&self)
{
	return { .map = self, .current = self.head.forward[0], .index = 0 };
}

<*
 Returns an iterator starting at the first key >= from_key in cmp order.
 Use with: foreach (entry : m.iter_from(key)) { ... }
*>
fn SortedMapRangeIterator SortedMap.iter_from(&self, Key from_key)
{
	SkipNode* current = self.head;
	for (int i = self.current_level - 1; i >= 0; i--)
	{
		while (current.forward[i] != null && sortedmap_cmp(self.custom_cmp, current.forward[i].key, from_key))
		{
			current = current.forward[i];
		}
	}
	SkipNode* start = current.forward[0];

	sz count = 0;
	SkipNode* n = start;
	while (n)
	{
		count++;
		n = n.forward[0];
	}

	return { .map = self, .start_node = start, .current = start, .current_idx = 0, .count = count };
}

<*
 Returns an iterator over keys in [from_key, to_key) in cmp order.
 Use with: foreach (entry : m.iter_range(from, to)) { ... }
*>
fn SortedMapRangeIterator SortedMap.iter_range(&self, Key from_key, Key to_key)
{
	SkipNode* current = self.head;
	for (int i = self.current_level - 1; i >= 0; i--)
	{
		while (current.forward[i] && sortedmap_cmp(self.custom_cmp, current.forward[i].key, from_key))
		{
			current = current.forward[i];
		}
	}
	SkipNode* start = current.forward[0];

	sz count = 0;
	SkipNode* n = start;
	while (n && sortedmap_cmp(self.custom_cmp, n.key, to_key))
	{
		count++;
		n = n.forward[0];
	}

	return { .map = self, .start_node = start, .current = start, .current_idx = 0, .count = count };
}

<*
 @require idx < self.map.count
*>
fn Entry SortedMapIterator.get(&self, sz idx) @operator([])
{
	if (idx < self.index)
	{
		self.current = self.map.head.forward[0];
		self.index = 0;
	}
	while (self.index < idx)
	{
		self.current = self.current.forward[0];
		self.index++;
	}
	return { .key = self.current.key, .value = self.current.value };
}

fn sz SortedMapIterator.len(&self) @operator(len) => self.map.count;

<*
 @require idx < self.count
*>
fn Entry SortedMapRangeIterator.get(&self, sz idx) @operator([])
{
	if (idx < self.current_idx)
	{
		self.current = self.start_node;
		self.current_idx = 0;
	}
	while (self.current_idx < idx)
	{
		self.current = self.current.forward[0];
		self.current_idx++;
	}
	return { .key = self.current.key, .value = self.current.value };
}

fn sz SortedMapRangeIterator.len(&self) @operator(len) => self.count;
