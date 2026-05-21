<*
 @require $defined((Value){}.hash()) : `No .hash function found on the value`
*>
module std::collections::set <Value>;
import std::math;
import std::io @norecurse;

alias LINKEDONHEAP = ORDEREDONHEAP;
const OrderedSet ORDEREDONHEAP = { .allocator = SET_HEAP_ALLOCATOR };

struct LinkedEntry
{
	uint hash;
	Value value;
	<* For bucket chain *>
	LinkedEntry* next;
	<* Previous in insertion order *>
	LinkedEntry* before;
	<* Next in insertion order *>
	LinkedEntry* after;
}

alias LinkedHashSet @deprecated("Use OrderedSet") = OrderedSet;
struct OrderedSet (Printable)
{
	LinkedEntry*[] table;
	Allocator allocator;
	<* Number of elements *>
	int count;
	<* Resize limit *>
	int threshold;
	float load_factor;
	LinkedEntry* head;
	<* First inserted LinkedEntry *>
	LinkedEntry* tail;
}

fn sz OrderedSet.len(&self) @operator(len) => self.count;

<*
 @param [&inout] allocator : "The allocator to use"
 @require capacity > 0 : "The capacity must be 1 or higher"
 @require load_factor > 0.0 && load_factor <= 1 : "The load factor must be higher than 0 and less or equal to 1"
 @require !self.is_initialized() : "Set was already initialized"
 @require capacity <= MAXIMUM_CAPACITY : "Capacity cannot exceed maximum"
*>
fn OrderedSet* OrderedSet.init(&self, Allocator allocator, sz capacity = DEFAULT_INITIAL_CAPACITY, float load_factor = DEFAULT_LOAD_FACTOR)
{
	capacity = math::next_power_of_2(capacity);
	self.allocator = allocator;
	self.threshold = (int)(capacity * load_factor);
	self.load_factor = load_factor;
	self.table = alloc::new_array(allocator, LinkedEntry*, capacity);

	self.head = null;
	self.tail = null;
	return self;
}

<*
 @require capacity > 0 : "The capacity must be 1 or higher"
 @require load_factor > 0.0 && load_factor <= 1 : "The load factor must be higher than 0 and less or equal to 1"
 @require !self.is_initialized() : "Set was already initialized"
 @require capacity <= MAXIMUM_CAPACITY : "Capacity cannot exceed maximum"
*>
fn OrderedSet* OrderedSet.tinit(&self, sz capacity = DEFAULT_INITIAL_CAPACITY, float load_factor = DEFAULT_LOAD_FACTOR)
{
	return self.init(tmem, capacity, load_factor) @inline;
}

<*
 @param [&inout] allocator : "The allocator to use"
 @require capacity > 0 : "The capacity must be 1 or higher"
 @require load_factor > 0.0 && load_factor <= 1 : "The load factor must be higher than 0 and less or equal to 1"
 @require !self.is_initialized() : "Set was already initialized"
 @require capacity <= MAXIMUM_CAPACITY : "Capacity cannot exceed maximum"
*>
macro OrderedSet* OrderedSet.init_with_values(&self, Allocator allocator, ..., sz capacity = DEFAULT_INITIAL_CAPACITY, float load_factor = DEFAULT_LOAD_FACTOR)
{
	self.init(allocator, capacity, load_factor);
	$for var $i = 0; $i < $vaarg.len; $i++:
		self.add($vaarg[$i]);
	$endfor
	return self;
}

<*
 @require capacity > 0 : "The capacity must be 1 or higher"
 @require load_factor > 0.0 && load_factor <= 1 : "The load factor must be higher than 0 and less or equal to 1"
 @require !self.is_initialized() : "Set was already initialized"
 @require capacity <= MAXIMUM_CAPACITY : "Capacity cannot exceed maximum"
*>
macro OrderedSet* OrderedSet.tinit_with_values(&self, ..., sz capacity = DEFAULT_INITIAL_CAPACITY, float load_factor = DEFAULT_LOAD_FACTOR)
{
	return self.init_with_values(tmem, ...$vaarg, capacity: capacity, load_factor: load_factor);
}

<*
 @param [in] values : "The values for the OrderedSet"
 @param [&inout] allocator : "The allocator to use"
 @require capacity > 0 : "The capacity must be 1 or higher"
 @require load_factor > 0.0 && load_factor <= 1 : "The load factor must be higher than 0 and less or equal to 1"
 @require !self.is_initialized() : "Set was already initialized"
 @require capacity <= MAXIMUM_CAPACITY : "Capacity cannot exceed maximum"
*>
fn OrderedSet* OrderedSet.init_from_values(&self, Allocator allocator, Value[] values, sz capacity = DEFAULT_INITIAL_CAPACITY, float load_factor = DEFAULT_LOAD_FACTOR)
{
	self.init(allocator, capacity, load_factor);
	foreach (v : values) self.add(v);
	return self;
}

<*
 @param [in] values : "The values for the OrderedSet entries"
 @require capacity > 0 : "The capacity must be 1 or higher"
 @require load_factor > 0.0 && load_factor <= 1 : "The load factor must be higher than 0 and less or equal to 1"
 @require !self.is_initialized() : "Set was already initialized"
 @require capacity <= MAXIMUM_CAPACITY : "Capacity cannot exceed maximum"
*>
fn OrderedSet* OrderedSet.tinit_from_values(&self, Value[] values, sz capacity = DEFAULT_INITIAL_CAPACITY, float load_factor = DEFAULT_LOAD_FACTOR)
{
	return self.init_from_values(tmem, values, capacity, load_factor);
}

<*
 Has this linked hash set been initialized yet?

 @param [&in] set : "The linked hash set we are testing"
 @return "Returns true if it has been initialized, false otherwise"
*>
fn bool OrderedSet.is_initialized(&set)
{
	return set.allocator && set.allocator.ptr != &dummy;
}

<*
 @param [&inout] allocator : "The allocator to use"
 @param [&in] other_set : "The set to copy from."
 @require !self.is_initialized() : "Set was already initialized"
*>
fn OrderedSet* OrderedSet.init_from_set(&self, Allocator allocator, OrderedSet* other_set)
{
	self.init(allocator, other_set.table.len, other_set.load_factor);
	LinkedEntry* entry = other_set.head;
	while (entry) // Save insertion order
	{
		orderedset_put_for_create(self, entry.value);
		entry = entry.after;
	}
	return self;
}

<*
 @param [&in] other_set : "The set to copy from."
 @require !set.is_initialized() : "Set was already initialized"
*>
fn OrderedSet* OrderedSet.tinit_from_set(&set, OrderedSet* other_set)
{
	return set.init_from_set(tmem, other_set) @inline;
}

<*
 Check if the set is empty

 @return "true if it is empty"
 @pure
*>
fn bool OrderedSet.is_empty(&set) @inline
{
	return !set.count;
}

<*
 Add all elements in the slice to the set.

 @param [in] list
 @return "The number of new elements added"
 @ensure total <= list.len
*>
fn int OrderedSet.add_all(&set, Value[] list)
{
	int total;
	foreach (v : list)
	{
		if (set.add(v)) total++;
	}
	return total;
}

<*
 @param [&in] other
 @return "The number of new elements added"
 @ensure return <= other.count
*>
fn int OrderedSet.add_all_from(&set, OrderedSet* other)
{
	int total;
	other.@each(;Value value)
	{
		if (set.add(value)) total++;
	};
	return total;
}

<*
 @param value : "The value to add"
 @return "true if the value didn't exist in the set"
*>
fn bool OrderedSet.add(&set, Value value)
{
	// If the set isn't initialized, use the defaults to initialize it.
	switch (set.allocator.ptr)
	{
		case &dummy:
			set.init(mem);
		case null:
			set.tinit();
		default:
			break;
	}
	uint hash = rehash(value.hash());
	int index = index_for(hash, set.table.len);
	for (LinkedEntry *e = set.table[index]; e != null; e = e.next)
	{
		if (e.hash == hash && equals(value, e.value)) return false;
	}
	orderedset_add_entry(set, hash, value, index);
	return true;
}

<*
 Iterate over all the values in the set
*>
macro OrderedSet.@each(set; @body(value))
{
	if (!set.count) return;
    LinkedEntry* entry = set.head;
	while (entry)
	{
		@body(entry.value);
		entry = entry.after;
	}
}

<*
 Check if the set contains the given value.

 @param value : "The value to check"
 @return "true if it exists in the set"
*>
fn bool OrderedSet.contains(&set, Value value)
{
	if (!set.count) return false;
	uint hash = rehash(value.hash());
	for (LinkedEntry *e = set.table[index_for(hash, set.table.len)]; e != null; e = e.next)
	{
		if (e.hash == hash && equals(value, e.value)) return true;
	}
	return false;
}

<*
 Remove a single value from the set.

 @param value : "The value to remove"
 @return? NOT_FOUND : "If the entry is not found"
*>
fn void? OrderedSet.remove(&set, Value value) @maydiscard
{
	if (!orderedset_remove_entry_for_value(set, value)) return NOT_FOUND~;
}

fn int OrderedSet.remove_all(&set, Value[] values)
{
	int total;
	foreach (v : values)
	{
		if (orderedset_remove_entry_for_value(set, v)) total++;
	}
	return total;
}

<*
 @param [&in] other : "Other set"
*>
fn int OrderedSet.remove_all_from(&set, OrderedSet* other)
{
	int total;
	other.@each(;Value val)
    {
		if (orderedset_remove_entry_for_value(set, val)) total++;
    };
    return total;
}

<*
 Free all memory allocated by the hash set.
*>
fn void OrderedSet.free(&set)
{
	if (!set.is_initialized()) return;
	set.clear();
	orderedset_free_internal(set, set.table.ptr);
	set.table = {};
}

<*
 Clear all elements from the set while keeping the underlying storage

 @ensure set.count == 0
*>
fn void OrderedSet.clear(&set)
{
	if (!set.count) return;

	LinkedEntry* entry = set.head;
	while (entry)
	{
		LinkedEntry* next = entry.after;
		orderedset_free_entry(set, entry);
		entry = next;
	}

	foreach (LinkedEntry** &bucket : set.table)
	{
		*bucket = null;
	}

	set.count = 0;
	set.head = null;
	set.tail = null;
}

fn void OrderedSet.reserve(&set, int capacity)
{
	if (capacity > set.threshold)
	{
		orderedset_resize(set, math::next_power_of_2(capacity));
	}
}

// --- Set Operations ---

<*
 Returns the union of two sets (A | B)

 @param [&in] other : "The other set to union with"
 @param [&inout] allocator : "Allocator for the new set"
 @return "A new set containing the union of both sets"
*>
fn OrderedSet OrderedSet.set_union(&self, Allocator allocator, OrderedSet* other)
{
	int new_capacity = math::next_power_of_2(self.count + other.count);
	OrderedSet result;
	result.init(allocator, new_capacity, self.load_factor);
	result.add_all_from(self);
	result.add_all_from(other);
	return result;
}

fn OrderedSet OrderedSet.tset_union(&self, OrderedSet* other) => self.set_union(tmem, other) @inline;

<*
 Returns the intersection of the two sets (A & B)

 @param [&in] other : "The other set to intersect with"
 @param [&inout] allocator : "Allocator for the new set"
 @return "A new set containing the intersection of both sets"
*>
fn OrderedSet OrderedSet.intersection(&self, Allocator allocator, OrderedSet* other)
{
	OrderedSet result;
	result.init(allocator, (int)math::min(self.table.len, other.table.len), self.load_factor);

	// Iterate through the smaller set for efficiency
	OrderedSet* smaller = self.count <= other.count ? self : other;
	OrderedSet* larger = self.count > other.count ? self : other;

	smaller.@each(;Value value)
    {
		if (larger.contains(value)) result.add(value);
	};

	return result;
}

fn OrderedSet OrderedSet.tintersection(&self, OrderedSet* other) => self.intersection(tmem, other) @inline;

<*
 Return this set - other, so (A & ~B)

 @param [&in] other : "The other set to compare with"
 @param [&inout] allocator : "Allocator for the new set"
 @return "A new set containing elements in this set but not in the other"
*>
fn OrderedSet OrderedSet.difference(&self, Allocator allocator, OrderedSet* other)
{
	OrderedSet result;
	result.init(allocator, self.table.len, self.load_factor);
	self.@each(;Value value)
	{
		if (!other.contains(value))
		{
			result.add(value);
		}
	};
	return result;
}

fn OrderedSet OrderedSet.tdifference(&self, OrderedSet* other) => self.difference(tmem, other) @inline;

<*
 Return (A ^ B)

 @param [&in] other : "The other set to compare with"
 @param [&inout] allocator : "Allocator for the new set"
 @return "A new set containing elements in this set or the other, but not both"
*>
fn OrderedSet OrderedSet.symmetric_difference(&self, Allocator allocator, OrderedSet* other)
{
	OrderedSet result;
	result.init(allocator, self.table.len, self.load_factor);
	result.add_all_from(self);
	other.@each(;Value value)
	{
		if (!result.add(value))
		{
			result.remove(value);
		}
	};
	return result;
}

fn OrderedSet OrderedSet.tsymmetric_difference(&self, OrderedSet* other) => self.symmetric_difference(tmem, other) @inline;

<*
 Check if this hash set is a subset of another set.

 @param [&in] other : "The other set to check against"
 @return "True if all elements of this set are in the other set"
*>
fn bool OrderedSet.is_subset(&self, OrderedSet* other)
{
	if (self.count == 0) return true;
	if (self.count > other.count) return false;

	self.@each(; Value value) {
		if (!other.contains(value)) return false;
	};
	return true;
}

// --- private methods

fn void orderedset_add_entry(OrderedSet* set, uint hash, Value value, int bucket_index) @private
{
	LinkedEntry* entry = alloc::new(set.allocator, LinkedEntry, {
		.hash = hash,
		.value = value,
		.next = set.table[bucket_index],
		.before = set.tail,
		.after = null
	});

	// Update bucket chain
	set.table[bucket_index] = entry;

	// Update linked list
	if (set.tail)
	{
		set.tail.after = entry;
		entry.before = set.tail;
	}
	else
	{
		set.head = entry;
	}
	set.tail = entry;

	if (set.count++ >= set.threshold)
	{
		orderedset_resize(set, set.table.len * 2);
	}
}

fn void orderedset_resize(OrderedSet* set, int new_capacity) @private
{
	LinkedEntry*[] old_table = set.table;
	int old_capacity = old_table.len;

	if (old_capacity >= int::max / 2)
	{
		set.threshold = int::max;
		return;
	}

	LinkedEntry*[] new_table = alloc::new_array(set.allocator, LinkedEntry*, new_capacity);
	set.table = new_table;
	set.threshold = (int)(new_capacity * set.load_factor);

	// Rehash all entries - linked list order remains unchanged
	foreach (int i, LinkedEntry *e : old_table)
	{
		if (!e) continue;

		// Split the bucket chain into two chains based on new bit
		LinkedEntry* lo_head = null;
		LinkedEntry* lo_tail = null;
		LinkedEntry* hi_head = null;
		LinkedEntry* hi_tail = null;

		do
		{
			LinkedEntry* next = e.next;
			if ((e.hash & (uint)old_capacity) == 0)
			{
				if (!lo_tail)
				{
					lo_head = e;
				}
				else
				{
					lo_tail.next = e;
				}
				lo_tail = e;
			}
			else
			{
				if (!hi_tail)
				{
					hi_head = e;
				}
				else
				{
					hi_tail.next = e;
				}
				hi_tail = e;
			}
			e.next = null;
			e = next;
		}
		while (e);

		if (lo_tail)
		{
			lo_tail.next = null;
			new_table[i] = lo_head;
		}
		if (hi_tail)
		{
			hi_tail.next = null;
			new_table[i + old_capacity] = hi_head;
		}
	}

	orderedset_free_internal(set, old_table.ptr);
}

fn sz? OrderedSet.to_format(&self, Formatter* f) @dynamic
{
	sz len;
	len += f.print("{ ")!;
	self.@each(; Value value)
	{
		if (len > 2) len += f.print(", ")!;
		len += f.printf("%s", value)!;
	};
	return len + f.print(" }");
}


fn void orderedset_put_for_create(OrderedSet* set, Value value) @private
{
	uint hash = rehash(value.hash());
	int i = index_for(hash, set.table.len);
	for (LinkedEntry *e = set.table[i]; e != null; e = e.next)
	{
		if (e.hash == hash && equals(value, e.value))
		{
			// Value already exists, no need to do anything
			return;
		}
	}
	orderedset_create_entry(set, hash, value, i);
}

fn void orderedset_free_internal(OrderedSet* set, void* ptr) @inline @private
{
	alloc::free(set.allocator, ptr);
}

fn void orderedset_create_entry(OrderedSet* set, uint hash, Value value, int bucket_index) @private
{
	LinkedEntry* entry = alloc::new(set.allocator, LinkedEntry, {
		.hash = hash,
		.value = value,
		.next = set.table[bucket_index],
		.before = set.tail,
		.after = null
	});

	set.table[bucket_index] = entry;

	// Update linked list
	if (set.tail)
	{
		set.tail.after = entry;
		entry.before = set.tail;
	}
	else
	{
		set.head = entry;
	}
	set.tail = entry;

	set.count++;
}

fn bool orderedset_remove_entry_for_value(OrderedSet* set, Value value) @private
{
	if (!set.count) return false;

	uint hash = rehash(value.hash());
	int i = index_for(hash, set.table.len);
	LinkedEntry* prev = null;
	LinkedEntry* e = set.table[i];

	while (e)
	{
		if (e.hash == hash && equals(value, e.value))
		{
			if (prev)
			{
				prev.next = e.next;
			}
			else
			{
				set.table[i] = e.next;
			}

			if (e.before)
			{
				e.before.after = e.after;
			}
			else
			{
				set.head = e.after;
			}

			if (e.after)
			{
				e.after.before = e.before;
			}
			else
			{
				set.tail = e.before;
			}

			set.count--;
			orderedset_free_entry(set, e);
			return true;
		}
		prev = e;
		e = e.next;
	}
	return false;
}

fn void orderedset_free_entry(OrderedSet* set, LinkedEntry *entry) @private
{
	alloc::free(set.allocator, entry);
}

alias LinkedHashSetIterator @deprecated("Use OrderedSetIterator") = OrderedSetIterator;
struct OrderedSetIterator
{
	OrderedSet* set;
	LinkedEntry* current;
	bool started;
}

fn OrderedSetIterator OrderedSet.iter(&set) => { .set = set, .current = set.head, .started = false };

fn bool OrderedSetIterator.next(&self)
{
	if (!self.started)
	{
		self.current = self.set.head;
		self.started = true;
	}
	else if (self.current)
	{
		self.current = self.current.after;
	}
	return self.current != null;
}

fn Value*? OrderedSetIterator.get(&self)
{
	return self.current ? &self.current.value : NOT_FOUND~;
}

fn bool OrderedSetIterator.has_next(&self)
{
	if (!self.started) return self.set.head != null;
	return self.current && self.current.after != null;
}

fn sz OrderedSetIterator.len(&self) @operator(len)
{
    return self.set.count;
}

int dummy @local;
