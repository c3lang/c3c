//  orderedmap.c3
//  An ordered map using a skip list for C3.
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

module std::collections::orderedmap;
import std::io, std::math::random;

const int MAX_LEVEL = 32;

struct SkipNode <Key, Value>
{
	Key key;
	Value value;
	int level;
	SkipNode*[MAX_LEVEL] forward;
}

// CmpFn is a comparator: returns true if a should come before b in the list.
alias CmpFn <Key> = fn bool(Key, Key);

typedef OrderedMap     <Key, Value> = inline PrivateOrderedMap{Key, Value, false};
typedef OrderedMapDesc <Key, Value> = inline PrivateOrderedMap{Key, Value, true};

struct Entry <Key, Value>
{
	Key key;
	Value value;
}

struct PrivateOrderedMap <Key, Value, DESCENDING>
{
	Allocator allocator;
	SkipNode{Key, Value}* head;
	int current_level;
	usz count;
	SimpleRandom rng;
	CmpFn{Key} custom_cmp; // null = use built-in ordering (less / DESCENDING)
}

struct OrderedMapIterator <Key, Value, DESCENDING>
{
	PrivateOrderedMap{Key, Value, DESCENDING}* map;
	SkipNode{Key, Value}* current;
	usz index;
}

struct OrderedMapRangeIterator <Key, Value, DESCENDING>
{
	PrivateOrderedMap{Key, Value, DESCENDING}* map;
	SkipNode{Key, Value}* start_node;
	SkipNode{Key, Value}* current;
	usz current_idx;
	usz count;
}

fn int orderedmap_random_level(SimpleRandom* rng) @private
{
	int level = 1;
	while (level < MAX_LEVEL && (rng.next_int() & 1) == 0) level++;
	return level;
}

// Returns true if 'a' should come before 'b' in the list.
macro bool PrivateOrderedMap.cmp(&self, Key a, Key b) @private
{
	if (self.custom_cmp != null) return self.custom_cmp(a, b);
	$if DESCENDING:
		return less(b, a);
	$else
		return less(a, b);
	$endif
}

fn PrivateOrderedMap* PrivateOrderedMap.init(&self, Allocator allocator)
{
	*self = { .allocator = allocator };
	self.head = alloc::alloc(allocator, SkipNode{Key, Value});
	*self.head = {};
	random::seed_entropy(&self.rng);
	return self;
}

fn PrivateOrderedMap* PrivateOrderedMap.tinit(&self)
{
	return self.init(tmem) @inline;
}

<*
 Init with a custom comparator. The comparator replaces the built-in ordering.
 @param cmp_fn : "Returns true if a should come before b in iteration order."
*>
fn PrivateOrderedMap* PrivateOrderedMap.init_with_cmp(&self, Allocator allocator, CmpFn{Key} cmp_fn)
{
	self.init(allocator);
	self.custom_cmp = cmp_fn;
	return self;
}

fn PrivateOrderedMap* PrivateOrderedMap.tinit_with_cmp(&self, CmpFn{Key} cmp_fn)
{
	return self.init_with_cmp(tmem, cmp_fn) @inline;
}

<*
 Insert or update the value associated with key.
*>
fn bool PrivateOrderedMap.set(&self, Key key, Value value) @operator([]=)
{
	SkipNode{Key, Value}*[MAX_LEVEL] update;
	SkipNode{Key, Value}* current = self.head;

	for (int i = self.current_level - 1; i >= 0; i--)
	{
		while (current.forward[i] != null && self.cmp(current.forward[i].key, key))
		{
			current = current.forward[i];
		}
		update[i] = current;
	}

	current = current.forward[0];

	// Key already exists — update value
	if (current != null && !self.cmp(current.key, key) && !self.cmp(key, current.key))
	{
		current.value = value;
		return true;
	}

	// Insert new node
	int new_level = orderedmap_random_level(&self.rng);
	if (new_level > self.current_level)
	{
		for (int i = self.current_level; i < new_level; i++) update[i] = self.head;
		self.current_level = new_level;
	}

	SkipNode{Key, Value}* node = alloc::alloc(self.allocator, SkipNode{Key, Value});
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
fn Value? PrivateOrderedMap.get(&self, Key key) @operator([])
{
	SkipNode{Key, Value}* current = self.head;
	for (int i = self.current_level - 1; i >= 0; i--)
	{
		while (current.forward[i] != null && self.cmp(current.forward[i].key, key))
		{
			current = current.forward[i];
		}
	}
	current = current.forward[0];
	if (current != null && !self.cmp(current.key, key) && !self.cmp(key, current.key))
	{
		return current.value;
	}
	return NOT_FOUND~;
}

<*
 @return "a pointer to the value for key, or null if absent"
*>
fn Value* PrivateOrderedMap.get_ref(&self, Key key) @operator(&[])
{
	SkipNode{Key, Value}* current = self.head;
	for (int i = self.current_level - 1; i >= 0; i--)
	{
		while (current.forward[i] != null && self.cmp(current.forward[i].key, key))
		{
			current = current.forward[i];
		}
	}
	current = current.forward[0];
	if (current != null && !self.cmp(current.key, key) && !self.cmp(key, current.key))
	{
		return &current.value;
	}
	return null;
}

<*
 Returns the value for key; inserts and returns default_value if key is absent.
*>
fn Value PrivateOrderedMap.get_or_set(&self, Key key, Value default_value)
{
	if (try v = self.get(key)) return v;
	self.set(key, default_value);
	return default_value;
}

<*
 @return "true if key exists in the map"
*>
fn bool PrivateOrderedMap.has_key(&self, Key key) => @ok(self.get(key));

<*
 @return "true if the key was found and removed"
*>
fn bool PrivateOrderedMap.remove(&self, Key key)
{
	SkipNode{Key, Value}*[MAX_LEVEL] update;
	SkipNode{Key, Value}* current = self.head;

	for (int i = self.current_level - 1; i >= 0; i--)
	{
		while (current.forward[i] != null && self.cmp(current.forward[i].key, key))
		{
			current = current.forward[i];
		}
		update[i] = current;
	}

	current = current.forward[0];

	if (current == null || self.cmp(current.key, key) || self.cmp(key, current.key)) return false;

	for (int i = 0; i < self.current_level; i++)
	{
		if (update[i].forward[i] != current) break;
		update[i].forward[i] = current.forward[i];
	}

	alloc::free(self.allocator, current);

	while (self.current_level > 0 && self.head.forward[self.current_level - 1] == null)
	{
		self.current_level--;
	}

	self.count--;
	return true;
}

<*
 @return "the first key in iteration order, or NOT_FOUND if empty"
*>
fn Key? PrivateOrderedMap.first_key(&self)
{
	if (!self.head.forward[0]) return NOT_FOUND~;
	return self.head.forward[0].key;
}

<*
 @return "the last key in iteration order, or NOT_FOUND if empty"
*>
fn Key? PrivateOrderedMap.last_key(&self)
{
	SkipNode{Key, Value}* current = self.head;
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
*>
fn Entry{Key, Value}? PrivateOrderedMap.pop_first(&self)
{
	SkipNode{Key, Value}* node = self.head.forward[0];
	if (!node) return NOT_FOUND~;
	Entry{Key, Value} result = { node.key, node.value };
	self.remove(node.key);
	return result;
}

<*
 Remove and return the last entry in iteration order.
 @return "the last Entry, or NOT_FOUND if empty"
*>
fn Entry{Key, Value}? PrivateOrderedMap.pop_last(&self)
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
// For ascending maps this matches natural key ordering.
// For descending maps, "before" means "greater" in natural ordering.

<*
 @return "the greatest key ≤ key in cmp order, or NOT_FOUND"
*>
fn Key? PrivateOrderedMap.floor_key(&self, Key key)
{
	SkipNode{Key, Value}* current = self.head;
	for (int i = self.current_level - 1; i >= 0; i--)
	{
		while (current.forward[i] != null && self.cmp(current.forward[i].key, key))
		{
			current = current.forward[i];
		}
	}
	// current is the last node strictly before key; check for exact match
	SkipNode{Key, Value}* next = current.forward[0];
	if (next != null && !self.cmp(next.key, key) && !self.cmp(key, next.key)) return next.key;
	if (current != self.head) return current.key;
	return NOT_FOUND~;
}

<*
 @return "the least key ≥ key in cmp order, or NOT_FOUND"
*>
fn Key? PrivateOrderedMap.ceiling_key(&self, Key key)
{
	SkipNode{Key, Value}* current = self.head;
	for (int i = self.current_level - 1; i >= 0; i--)
	{
		while (current.forward[i] != null && self.cmp(current.forward[i].key, key))
		{
			current = current.forward[i];
		}
	}
	SkipNode{Key, Value}* next = current.forward[0];
	if (next != null) return next.key;
	return NOT_FOUND~;
}

<*
 @return "the greatest key strictly before key in cmp order, or NOT_FOUND"
*>
fn Key? PrivateOrderedMap.lower_key(&self, Key key)
{
	SkipNode{Key, Value}* current = self.head;
	for (int i = self.current_level - 1; i >= 0; i--)
	{
		while (current.forward[i] != null && self.cmp(current.forward[i].key, key))
		{
			current = current.forward[i];
		}
	}
	if (current != self.head) return current.key;
	return NOT_FOUND~;
}

<*
 @return "the least key strictly after key in cmp order, or NOT_FOUND"
*>
fn Key? PrivateOrderedMap.higher_key(&self, Key key)
{
	SkipNode{Key, Value}* current = self.head;
	for (int i = self.current_level - 1; i >= 0; i--)
	{
		while (current.forward[i] != null && self.cmp(current.forward[i].key, key))
		{
			current = current.forward[i];
		}
	}
	SkipNode{Key, Value}* next = current.forward[0];
	if (next == null) return NOT_FOUND~;
	// If next equals key, advance one more
	if (!self.cmp(next.key, key) && !self.cmp(key, next.key)) next = next.forward[0];
	if (next != null) return next.key;
	return NOT_FOUND~;
}

fn usz PrivateOrderedMap.len(&self) @inline => self.count;

fn bool PrivateOrderedMap.is_empty(&self) @inline => self.count == 0;

fn void PrivateOrderedMap.clear(&self)
{
	if (!self.allocator) return;
	SkipNode{Key, Value}* node = self.head.forward[0];
	while (node != null)
	{
		SkipNode{Key, Value}* next = node.forward[0];
		alloc::free(self.allocator, node);
		node = next;
	}
	*self.head = {};
	self.count = 0;
	self.current_level = 0;
}

fn void PrivateOrderedMap.free(&self)
{
	self.clear();
	alloc::free(self.allocator, self.head);
	self.head = null;
}

<*
 Copy all entries of this map into a new map backed by allocator.
 The clone inherits the custom comparator (if any).
*>
fn PrivateOrderedMap PrivateOrderedMap.clone(&self, Allocator allocator)
{
	PrivateOrderedMap result;
	result.init_with_cmp(allocator, self.custom_cmp);
	SkipNode{Key, Value}* node = self.head.forward[0];
	while (node != null)
	{
		result.set(node.key, node.value);
		node = node.forward[0];
	}
	return result;
}

fn PrivateOrderedMap PrivateOrderedMap.tclone(&self)
{
	return self.clone(tmem) @inline;
}

<*
 Insert all entries from other into self, overwriting on key collision.
*>
fn void PrivateOrderedMap.merge(&self, PrivateOrderedMap{Key, Value, DESCENDING}* other)
{
	SkipNode{Key, Value}* node = other.head.forward[0];
	while (node != null)
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
fn OrderedMapIterator{Key, Value, DESCENDING} PrivateOrderedMap.iter(&self)
{
	return { .map = self, .current = self.head.forward[0], .index = 0 };
}

<*
 Returns an iterator starting at the first key >= from_key in cmp order.
 Use with: foreach (entry : m.iter_from(key)) { ... }
*>
fn OrderedMapRangeIterator{Key, Value, DESCENDING} PrivateOrderedMap.iter_from(&self, Key from_key)
{
	SkipNode{Key, Value}* current = self.head;
	for (int i = self.current_level - 1; i >= 0; i--)
	{
		while (current.forward[i] != null && self.cmp(current.forward[i].key, from_key))
		{
			current = current.forward[i];
		}
	}
	SkipNode{Key, Value}* start = current.forward[0];

	usz count = 0;
	SkipNode{Key, Value}* n = start;
	while (n != null) { count++; n = n.forward[0]; }

	return { .map = self, .start_node = start, .current = start, .current_idx = 0, .count = count };
}

<*
 Returns an iterator over keys in [from_key, to_key) in cmp order.
 For ascending maps: from_key <= key < to_key.
 For descending maps: pass from and to in descending order (from > to naturally).
 Use with: foreach (entry : m.iter_range(from, to)) { ... }
*>
fn OrderedMapRangeIterator{Key, Value, DESCENDING} PrivateOrderedMap.iter_range(&self, Key from_key, Key to_key)
{
	SkipNode{Key, Value}* current = self.head;
	for (int i = self.current_level - 1; i >= 0; i--)
	{
		while (current.forward[i] != null && self.cmp(current.forward[i].key, from_key))
		{
			current = current.forward[i];
		}
	}
	SkipNode{Key, Value}* start = current.forward[0];

	usz count = 0;
	SkipNode{Key, Value}* n = start;
	while (n != null && self.cmp(n.key, to_key)) { count++; n = n.forward[0]; }

	return { .map = self, .start_node = start, .current = start, .current_idx = 0, .count = count };
}

<*
 @require idx < self.map.count
*>
fn Entry{Key, Value} OrderedMapIterator.get(&self, usz idx) @operator([])
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

fn usz OrderedMapIterator.len(&self) @operator(len) => self.map.count;

<*
 @require idx < self.count
*>
fn Entry{Key, Value} OrderedMapRangeIterator.get(&self, usz idx) @operator([])
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

fn usz OrderedMapRangeIterator.len(&self) @operator(len) => self.count;
