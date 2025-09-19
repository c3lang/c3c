<*
 @require $defined((Value){}.hash()) : `No .hash function found on the value`
*>
module std::collections::set {Value};
import std::math;
import std::io @norecurse;

const LinkedHashSet LINKEDONHEAP = { .allocator = SET_HEAP_ALLOCATOR };

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

struct LinkedHashSet (Printable)
{
	LinkedEntry*[] table;
	Allocator allocator;
	<* Number of elements *>
	usz count;
	<* Resize limit *>
	usz threshold;
	float load_factor;
	<* Resize limit *>
	LinkedEntry* head;
	<* First inserted LinkedEntry *>
	LinkedEntry* tail;
}

fn int LinkedHashSet.len(&self) @operator(len) => (int) self.count;

<*
 @param [&inout] allocator : "The allocator to use"
 @require capacity > 0 : "The capacity must be 1 or higher"
 @require load_factor > 0.0 : "The load factor must be higher than 0"
 @require !self.is_initialized() : "Set was already initialized"
 @require capacity < MAXIMUM_CAPACITY : "Capacity cannot exceed maximum"
*>
fn LinkedHashSet* LinkedHashSet.init(&self, Allocator allocator, usz capacity = DEFAULT_INITIAL_CAPACITY, float load_factor = DEFAULT_LOAD_FACTOR)
{
	capacity = math::next_power_of_2(capacity);
	self.allocator = allocator;
	self.threshold = (usz)(capacity * load_factor);
	self.load_factor = load_factor;
	self.table = allocator::new_array(allocator, LinkedEntry*, capacity);

	self.head = null;
	self.tail = null;
	return self;
}

<*
 @require capacity > 0 : "The capacity must be 1 or higher"
 @require load_factor > 0.0 : "The load factor must be higher than 0"
 @require !self.is_initialized() : "Set was already initialized"
 @require capacity < MAXIMUM_CAPACITY : "Capacity cannot exceed maximum"
*>
fn LinkedHashSet* LinkedHashSet.tinit(&self, usz capacity = DEFAULT_INITIAL_CAPACITY, float load_factor = DEFAULT_LOAD_FACTOR)
{
	return self.init(tmem, capacity, load_factor) @inline;
}

<*
 @param [&inout] allocator : "The allocator to use"
 @require capacity > 0 : "The capacity must be 1 or higher"
 @require load_factor > 0.0 : "The load factor must be higher than 0"
 @require !self.is_initialized() : "Set was already initialized"
 @require capacity < MAXIMUM_CAPACITY : "Capacity cannot exceed maximum"
*>
macro LinkedHashSet* LinkedHashSet.init_with_values(&self, Allocator allocator, ..., uint capacity = DEFAULT_INITIAL_CAPACITY, float load_factor = DEFAULT_LOAD_FACTOR)
{
	self.init(allocator, capacity, load_factor);
	$for var $i = 0; $i < $vacount; $i++:
		self.add($vaarg[$i]);
	$endfor
	return self;
}

<*
 @require capacity > 0 : "The capacity must be 1 or higher"
 @require load_factor > 0.0 : "The load factor must be higher than 0"
 @require !self.is_initialized() : "Set was already initialized"
 @require capacity < MAXIMUM_CAPACITY : "Capacity cannot exceed maximum"
*>
macro LinkedHashSet* LinkedHashSet.tinit_with_values(&self, ..., uint capacity = DEFAULT_INITIAL_CAPACITY, float load_factor = DEFAULT_LOAD_FACTOR)
{
	return self.init_with_values(tmem, $vasplat, capacity: capacity, load_factor: load_factor);
}

<*
 @param [in] values : "The values for the LinkedHashSet"
 @param [&inout] allocator : "The allocator to use"
 @require capacity > 0 : "The capacity must be 1 or higher"
 @require load_factor > 0.0 : "The load factor must be higher than 0"
 @require !self.is_initialized() : "Set was already initialized"
 @require capacity < MAXIMUM_CAPACITY : "Capacity cannot exceed maximum"
*>
fn LinkedHashSet* LinkedHashSet.init_from_values(&self, Allocator allocator, Value[] values, uint capacity = DEFAULT_INITIAL_CAPACITY, float load_factor = DEFAULT_LOAD_FACTOR)
{
	self.init(allocator, capacity, load_factor);
	foreach (v : values) self.add(v);
	return self;
}

<*
 @param [in] values : "The values for the LinkedHashSet entries"
 @require capacity > 0 : "The capacity must be 1 or higher"
 @require load_factor > 0.0 : "The load factor must be higher than 0"
 @require !self.is_initialized() : "Set was already initialized"
 @require capacity < MAXIMUM_CAPACITY : "Capacity cannot exceed maximum"
*>
fn LinkedHashSet* LinkedHashSet.tinit_from_values(&self, Value[] values, uint capacity = DEFAULT_INITIAL_CAPACITY, float load_factor = DEFAULT_LOAD_FACTOR)
{
	return self.init_from_values(tmem, values, capacity, load_factor);
}

<*
 Has this linked hash set been initialized yet?

 @param [&in] set : "The linked hash set we are testing"
 @return "Returns true if it has been initialized, false otherwise"
*>
fn bool LinkedHashSet.is_initialized(&set)
{
	return set.allocator && set.allocator.ptr != &dummy;
}

<*
 @param [&inout] allocator : "The allocator to use"
 @param [&in] other_set : "The set to copy from."
 @require !self.is_initialized() : "Set was already initialized"
*>
fn LinkedHashSet* LinkedHashSet.init_from_set(&self, Allocator allocator, LinkedHashSet* other_set)
{
	self.init(allocator, other_set.table.len, other_set.load_factor);
	LinkedEntry* entry = other_set.head;
	while (entry) // Save insertion order
	{
		self.put_for_create(entry.value);
		entry = entry.after;
	}
	return self;
}

<*
 @param [&in] other_set : "The set to copy from."
 @require !set.is_initialized() : "Set was already initialized"
*>
fn LinkedHashSet* LinkedHashSet.tinit_from_set(&set, LinkedHashSet* other_set)
{
	return set.init_from_set(tmem, other_set) @inline;
}

<*
 Check if the set is empty

 @return "true if it is empty"
 @pure
*>
fn bool LinkedHashSet.is_empty(&set) @inline
{
	return !set.count;
}

<*
 Add all elements in the slice to the set.

 @param [in] list
 @return "The number of new elements added"
 @ensure total <= list.len
*>
fn usz LinkedHashSet.add_all(&set, Value[] list)
{
	usz total;
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
fn usz LinkedHashSet.add_all_from(&set, LinkedHashSet* other)
{
	usz total;
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
fn bool LinkedHashSet.add(&set, Value value)
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
	uint index = index_for(hash, set.table.len);
	for (LinkedEntry *e = set.table[index]; e != null; e = e.next)
	{
		if (e.hash == hash && equals(value, e.value)) return false;
	}
	set.add_entry(hash, value, index);
	return true;
}

<*
 Iterate over all the values in the set
*>
macro LinkedHashSet.@each(set; @body(value))
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
fn bool LinkedHashSet.contains(&set, Value value)
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
fn void? LinkedHashSet.remove(&set, Value value) @maydiscard
{
	if (!set.remove_entry_for_value(value)) return NOT_FOUND?;
}

fn usz LinkedHashSet.remove_all(&set, Value[] values)
{
	usz total;
	foreach (v : values)
	{
		if (set.remove_entry_for_value(v)) total++;
	}
	return total;
}

<*
 @param [&in] other : "Other set"
*>
fn usz LinkedHashSet.remove_all_from(&set, LinkedHashSet* other)
{
	usz total;
	other.@each(;Value val)
    {
		if (set.remove_entry_for_value(val)) total++;
    };
    return total;
}

<*
 Free all memory allocated by the hash set.
*>
fn void LinkedHashSet.free(&set)
{
	if (!set.is_initialized()) return;
	set.clear();
	set.free_internal(set.table.ptr);
	set.table = {};
}

<*
 Clear all elements from the set while keeping the underlying storage

 @ensure set.count == 0
*>
fn void LinkedHashSet.clear(&set)
{
	if (!set.count) return;

	LinkedEntry* entry = set.head;
	while (entry)
	{
		LinkedEntry* next = entry.after;
		set.free_entry(entry);
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

fn void LinkedHashSet.reserve(&set, usz capacity)
{
	if (capacity > set.threshold)
	{
		set.resize(math::next_power_of_2(capacity));
	}
}

// --- Set Operations ---

<*
 Returns the union of two sets (A | B)

 @param [&in] other : "The other set to union with"
 @param [&inout] allocator : "Allocator for the new set"
 @return "A new set containing the union of both sets"
*>
fn LinkedHashSet LinkedHashSet.set_union(&self, Allocator allocator, LinkedHashSet* other)
{
	usz new_capacity = math::next_power_of_2(self.count + other.count);
	LinkedHashSet result;
	result.init(allocator, new_capacity, self.load_factor);
	result.add_all_from(self);
	result.add_all_from(other);
	return result;
}

fn LinkedHashSet LinkedHashSet.tset_union(&self, LinkedHashSet* other) => self.set_union(tmem, other) @inline;

<*
 Returns the intersection of the two sets (A & B)

 @param [&in] other : "The other set to intersect with"
 @param [&inout] allocator : "Allocator for the new set"
 @return "A new set containing the intersection of both sets"
*>
fn LinkedHashSet LinkedHashSet.intersection(&self, Allocator allocator, LinkedHashSet* other)
{
	LinkedHashSet result;
	result.init(allocator, math::min(self.table.len, other.table.len), self.load_factor);

	// Iterate through the smaller set for efficiency
	LinkedHashSet* smaller = self.count <= other.count ? self : other;
	LinkedHashSet* larger = self.count > other.count ? self : other;

	smaller.@each(;Value value)
    {
		if (larger.contains(value)) result.add(value);
	};

	return result;
}

fn LinkedHashSet LinkedHashSet.tintersection(&self, LinkedHashSet* other) => self.intersection(tmem, other) @inline;

<*
 Return this set - other, so (A & ~B)

 @param [&in] other : "The other set to compare with"
 @param [&inout] allocator : "Allocator for the new set"
 @return "A new set containing elements in this set but not in the other"
*>
fn LinkedHashSet LinkedHashSet.difference(&self, Allocator allocator, LinkedHashSet* other)
{
	LinkedHashSet result;
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

fn LinkedHashSet LinkedHashSet.tdifference(&self, LinkedHashSet* other) => self.difference(tmem, other) @inline;

<*
 Return (A ^ B)

 @param [&in] other : "The other set to compare with"
 @param [&inout] allocator : "Allocator for the new set"
 @return "A new set containing elements in this set or the other, but not both"
*>
fn LinkedHashSet LinkedHashSet.symmetric_difference(&self, Allocator allocator, LinkedHashSet* other)
{
	LinkedHashSet result;
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

fn LinkedHashSet LinkedHashSet.tsymmetric_difference(&self, LinkedHashSet* other) => self.symmetric_difference(tmem, other) @inline;

<*
 Check if this hash set is a subset of another set.

 @param [&in] other : "The other set to check against"
 @return "True if all elements of this set are in the other set"
*>
fn bool LinkedHashSet.is_subset(&self, LinkedHashSet* other)
{
	if (self.count == 0) return true;
	if (self.count > other.count) return false;

	self.@each(; Value value) {
		if (!other.contains(value)) return false;
	};
	return true;
}

// --- private methods

fn void LinkedHashSet.add_entry(&set, uint hash, Value value, uint bucket_index) @private
{
	LinkedEntry* entry = allocator::new(set.allocator, LinkedEntry, {
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
		set.resize(set.table.len * 2);
	}
}

fn void LinkedHashSet.resize(&set, usz new_capacity) @private
{
	LinkedEntry*[] old_table = set.table;
	usz old_capacity = old_table.len;

	if (old_capacity == MAXIMUM_CAPACITY)
	{
		set.threshold = uint.max;
		return;
	}

	LinkedEntry*[] new_table = allocator::new_array(set.allocator, LinkedEntry*, new_capacity);
	set.table = new_table;
	set.threshold = (uint)(new_capacity * set.load_factor);

	// Rehash all entries - linked list order remains unchanged
	foreach (uint i, LinkedEntry *e : old_table)
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
			if ((e.hash & old_capacity) == 0)
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

	set.free_internal(old_table.ptr);
}

fn usz? LinkedHashSet.to_format(&self, Formatter* f) @dynamic
{
	usz len;
	len += f.print("{ ")!;
	self.@each(; Value value)
	{
		if (len > 2) len += f.print(", ")!;
		len += f.printf("%s", value)!;
	};
	return len + f.print(" }");
}

fn void LinkedHashSet.transfer(&set, LinkedEntry*[] new_table) @private
{
	LinkedEntry*[] src = set.table;
	uint new_capacity = new_table.len;
	foreach (uint j, LinkedEntry *e : src)
	{
		if (!e) continue;
		do
		{
			LinkedEntry* next = e.next;
			uint i = index_for(e.hash, new_capacity);
			e.next = new_table[i];
			new_table[i] = e;
			e = next;
		}
		while (e);
	}
}

fn void LinkedHashSet.put_for_create(&set, Value value) @private
{
	uint hash = rehash(value.hash());
	uint i = index_for(hash, set.table.len);
	for (LinkedEntry *e = set.table[i]; e != null; e = e.next)
	{
		if (e.hash == hash && equals(value, e.value))
		{
			// Value already exists, no need to do anything
			return;
		}
	}
	set.create_entry(hash, value, i);
}

fn void LinkedHashSet.free_internal(&set, void* ptr) @inline @private
{
	allocator::free(set.allocator, ptr);
}

fn void LinkedHashSet.create_entry(&set, uint hash, Value value, int bucket_index) @private
{
	LinkedEntry* entry = allocator::new(set.allocator, LinkedEntry, {
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

fn bool LinkedHashSet.remove_entry_for_value(&set, Value value) @private
{
	if (!set.count) return false;

	uint hash = rehash(value.hash());
	uint i = index_for(hash, set.table.len);
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
			set.free_entry(e);
			return true;
		}
		prev = e;
		e = e.next;
	}
	return false;
}

fn void LinkedHashSet.free_entry(&set, LinkedEntry *entry) @private
{
	allocator::free(set.allocator, entry);
}

struct LinkedHashSetIterator
{
	LinkedHashSet* set;
	LinkedEntry* current;
	bool started;
}

fn LinkedHashSetIterator LinkedHashSet.iter(&set) => { .set = set, .current = set.head, .started = false };

fn bool LinkedHashSetIterator.next(&self)
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

fn Value*? LinkedHashSetIterator.get(&self)
{
	return self.current ? &self.current.value : NOT_FOUND?;
}

fn bool LinkedHashSetIterator.has_next(&self)
{
	if (!self.started) return self.set.head != null;
	return self.current && self.current.after != null;
}

fn usz LinkedHashSetIterator.len(&self) @operator(len)
{
    return self.set.count;
}

int dummy @local;
