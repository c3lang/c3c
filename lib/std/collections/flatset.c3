<*
 @require types::has_equals(Value) : "Must have equality operator overloaded"
 @require $defined((Value) { }.hash()) : "Must be hashable"
*>
module std::collections::flatset <Value>;
import std::math;
import std::io;
import std::collections::pair;

alias HashType = uint;
const sz DEFAULT_CAPACITY @private       = 16;
const float DEFAULT_LOAD_FACTOR @private = 0.85f;
const HashType HASH_EMPTY @private       = 0;
const HashType HASH_DELETED @private     = 1;
const HashType HASH_FIRST_VALID @private = 2;

<*
 FlatSet is a hash set whose elements live together in one compact, contiguous
 region, unlike separate chaining where each element is a node heap-allocated
 individually and chained off its bucket. The flat storage is cache-friendly
 and memory-lean, so membership tests, inserts and full iteration stay quick
 under dense use, yet the data must be redistributed whenever the set outgrows
 its capacity. Chaining is more tolerant of skewed hashes and makes deletion
 cheap and predictable, but pays a per-element allocation and pointer-chasing
 overhead that penalizes sequential and hot-path workloads.
*>
struct FlatSet
{
	HashType* data;
	Allocator alloc;
	sz count;
	sz capacity;
	float load_factor;
}

<*
 @param [&inout] allocator : "The allocator to use"
 @require capacity > 0 : "The capacity must be 1 or higher"
 @require load_factor > 0.0 && load_factor <= 1 : "The load factor must be higher than 0 and less or equal to 1"
 @require !self.is_initialized() : "Set was already initialized"
*>
fn FlatSet* FlatSet.init(
	&self,
	Allocator allocator,
	sz capacity = DEFAULT_CAPACITY,
	float load_factor = DEFAULT_LOAD_FACTOR
)
{
	sz mem_req = calc_mem_req(math::next_power_of_2(capacity));
	self.data = alloc::malloc(allocator, mem_req);
	self.capacity = capacity;
	self.count = 0;
	self.alloc = allocator;
	self.load_factor = load_factor;
	reset_hashes(self.data[:self.capacity]);
	return self;
}

<*
 @require capacity > 0 : "The capacity must be 1 or higher"
 @require load_factor > 0.0 && load_factor <= 1 : "The load factor must be higher than 0 and less or equal to 1"
 @require !self.is_initialized() : "Set was already initialized"
*>
fn FlatSet* FlatSet.tinit(&self, sz capacity = DEFAULT_CAPACITY, float load_factor = DEFAULT_LOAD_FACTOR)
{
	return self.init(tmem, capacity, load_factor);
}

<*
 @param [in] values : "The values for the FlatSet entries"
 @param [&inout] allocator : "The allocator to use"
 @require capacity > 0 : "The capacity must be 1 or higher"
 @require load_factor > 0.0 && load_factor <= 1 : "The load factor must be higher than 0 and less or equal to 1"
 @require !self.is_initialized() : "Set was already initialized"
*>
fn FlatSet* FlatSet.init_from_values(
	&self,
	Allocator allocator,
	Value[] values,
	sz capacity = DEFAULT_CAPACITY,
	float load_factor = DEFAULT_LOAD_FACTOR
)
{
	self.init(allocator, capacity, load_factor);
	foreach (value : values)
	{
		self.add(value);
	}
	return self;
}

<*
 @param [in] values : "The values for the FlatSet entries"
 @require capacity > 0 : "The capacity must be 1 or higher"
 @require load_factor > 0.0 && load_factor <= 1 : "The load factor must be higher than 0 and less or equal to 1"
 @require !self.is_initialized() : "Set was already initialized"
*>
fn FlatSet* FlatSet.tinit_from_values(
	&self,
	Value[] values,
	sz capacity = DEFAULT_CAPACITY,
	float load_factor = DEFAULT_LOAD_FACTOR
)
{
	return self.init_from_values(tmem, values, capacity, load_factor);
}

<*
 Frees all underlying resources.
 Shouldn't be used after this operation without initialization.
*>
fn void FlatSet.free(&self)
{
	if (self.data)
	{
		alloc::free(self.alloc, self.data);
		*self = { };
	}
}

<*
 Resets underlying data, so flat set can be reused.
*>
fn void FlatSet.clear(&self)
{
	self.count = 0;
	HashType[] hashes = self.data[:self.capacity];
	reset_hashes(hashes);
}

<*
 Inserts a value into the flat set.
*>
fn bool FlatSet.add(&self, Value value)
{
	if (crossed_threshold(self)) grow(self);
	HashType hash = calc_hash(value);
	return add_inner(self.data, self.capacity, &self.count, hash, value);
}

fn bool add_inner(HashType* data, sz capacity, sz* count, HashType hash, Value value) @private @inline
{
	sz start_idx       =  get_idx(hash, capacity);
	sz curr_idx        =  start_idx;
	Value[] values     =  values_array(data, capacity);
	sz insert_slot_idx = -1;

	for (; curr_idx < capacity; ++curr_idx)
	{
		// We need to try to find closest slot possible, but we can't break from here.
		// We need to iterate over all probe sequence to scan for duplicates.
		if (insert_slot_idx == -1 && data[curr_idx] < HASH_FIRST_VALID)
		{
			insert_slot_idx = curr_idx;
			continue;
		}
		// Element was already in the set.
		if (hash == data[curr_idx] && values[curr_idx] == value) return false;
	}

	if (insert_slot_idx != -1)
	{
		data[insert_slot_idx] = hash;
		values[insert_slot_idx] = value;
		if (count) (*count)++;
		return true;
	}

	// Wrapped around, start from first entry.
	curr_idx = 0;

	for (; curr_idx < start_idx; ++curr_idx)
	{
		// We need to try to find closest slot possible, but we can't break from here.
		// We need to iterate over all probe sequence to scan for duplicates.
		if (insert_slot_idx == -1 && data[curr_idx] < HASH_FIRST_VALID)
		{
			insert_slot_idx = curr_idx;
			continue;
		}
		// Element was already in the set.
		if (hash == data[curr_idx] && values[curr_idx] == value) return false;
	}

	if (@unlikely(insert_slot_idx == -1))
	{
		unreachable("Failed to insert a value into a hashset");
	}

	data[insert_slot_idx] = hash;
	values[insert_slot_idx] = value;
	if (count) (*count)++;
	return true;
}

<*
 Copies all elements from the input set into the current set.

 @require self.is_initialized() : "Must be initialized set"
 @require other.is_initialized() : "Must be initialized set"
*>
fn void FlatSet.add_all_from(&self, FlatSet* other)
{
	if (@unlikely(other.is_empty())) return;

	HashType[] other_hashes = hashes_array(other);
	Value[] other_values    = values_array(other.data, other.capacity);
	foreach (idx, hash : other_hashes)
	{
		if (hash >= HASH_FIRST_VALID)
		{
			self.add(other_values[idx]);
		}
	}
}

<*
 Checks if flat set has the provided value.
*>
fn bool FlatSet.contains(&self, Value value)
{
	HashType search_hash = calc_hash(value);
	sz start_idx         = get_idx(search_hash, self.capacity);
	sz curr_idx          = start_idx;
	Value[] values       = values_array(self.data, self.capacity);

	for (; curr_idx != self.capacity; ++curr_idx)
	{
		if (self.data[curr_idx] == HASH_EMPTY) return false;
		if (self.data[curr_idx] == search_hash && values[curr_idx] == value)
		{
			return true;
		}
	}

	// if we searched from the start, no need to do wrap around search.
	if (start_idx == 0) return false;

	curr_idx = 0;

	for (; curr_idx < start_idx; ++curr_idx)
	{
		if (self.data[curr_idx] == HASH_EMPTY) return false;
		if (self.data[curr_idx] == search_hash && values[curr_idx] == value)
		{
			return true;
		}
	}

	return false;
}

<*
 Removes the value from a flat set if it exists.
*>
fn bool FlatSet.remove(&self, Value value)
{
	HashType search_hash = calc_hash(value);
	sz start_idx         = get_idx(search_hash, self.capacity);
	sz curr_idx          = start_idx;
	Value[] values       = values_array(self.data, self.capacity);

	for (; curr_idx < self.capacity; ++curr_idx)
	{
		if (self.data[curr_idx] == HASH_EMPTY) return false;
		if (self.data[curr_idx] == search_hash && values[curr_idx] == value)
		{
			self.data[curr_idx] = HASH_DELETED;
			self.count--;
			return true;
		}
	}

	// if we searched from the start, no need to do wrap around search.
	if (start_idx == 0) return false;

	curr_idx = 0;

	for (; curr_idx < start_idx; ++curr_idx)
	{
		if (self.data[curr_idx] == HASH_EMPTY) return false;
		if (self.data[curr_idx] == search_hash && values[curr_idx] == value)
		{
			self.data[curr_idx] = HASH_DELETED;
			self.count--;
			return true;
		}
	}

	return false;
}

// Set operations.

<*
 Returns the union of two sets (A | B)

 @param [&in] other : "The other set to union with"
 @param [&inout] allocator : "Allocator for the new set"
 @return "A new set containing the union of both sets"
*>
fn FlatSet FlatSet.set_union(&self, Allocator allocator, FlatSet* other)
{
	sz new_capacity = math::next_power_of_2(self.count + other.count);
	FlatSet result;
	result.init(allocator, self.capacity + other.capacity, self.load_factor);
	result.add_all_from(self);
	result.add_all_from(other);
	return result;
}

fn FlatSet FlatSet.tset_union(&self, FlatSet* other) => self.set_union(tmem, other);

<*
 Returns the intersection of the two sets (A & B)

 @param [&in] other : "The other set to intersect with"
 @param [&inout] allocator : "Allocator for the new set"
 @return "A new set containing the intersection of both sets"
*>
fn FlatSet FlatSet.intersection(&self, Allocator allocator, FlatSet* other)
{
	FlatSet result;
	result.init(allocator, math::min(self.capacity, other.capacity), self.load_factor);

	// Iterate through the smaller set for efficiency
	FlatSet* smaller = self.count <= other.count ? self : other;
	FlatSet* larger  = self.count > other.count ? self : other;

	smaller.@each(; Value value)
	{
		if (larger.contains(value)) result.add(value);
	};

	return result;
}

fn FlatSet FlatSet.tintersection(&self, FlatSet* other) => self.intersection(tmem, other);

<*
 Return this set - other, so (A & ~B)

 @param [&in] other : "The other set to compare with"
 @param [&inout] allocator : "Allocator for the new set"
 @return "A new set containing elements in this set but not in the other"
*>
fn FlatSet FlatSet.difference(&self, Allocator allocator, FlatSet* other)
{
	FlatSet result;
	result.init(allocator, self.capacity, self.load_factor);
	self.@each(; Value value)
	{
		if (!other.contains(value))
		{
			(void)result.add(value);
		}
	};
	return result;
}

fn FlatSet FlatSet.tdifference(&self, FlatSet* other) => self.difference(tmem, other) @inline;

<*
 Return (A ^ B)

 @param [&in] other : "The other set to compare with"
 @param [&inout] allocator : "Allocator for the new set"
 @return "A new set containing elements in this set or the other, but not both"
*>
fn FlatSet FlatSet.symmetric_difference(&self, Allocator allocator, FlatSet* other)
{
	FlatSet result;
	result.init(allocator, self.capacity, self.load_factor);
	result.add_all_from(self);
	other.@each(; Value value)
	{
		if (!result.add(value))
		{
			(void)result.remove(value);
		}
	};
	return result;
}

fn FlatSet FlatSet.tsymmetric_difference(&self, FlatSet* other) => self.symmetric_difference(tmem, other) @inline;

<*
 Check if this hash set is a subset of another set.

 @param [&in] other : "The other set to check against"
 @return "True if all elements of this set are in the other set"
*>
fn bool FlatSet.is_subset(&self, FlatSet* other)
{
	if (self.count == 0) return true;
	if (self.count > other.count) return false;

	self.@each(; Value value)
	{
		if (!other.contains(value)) return false;
	};
	return true;
}

<*
 Returns array of all values.
*>
fn Value[] FlatSet.values(&self, Allocator alloc)
{
	Value[] value_buff   = alloc::alloc_array(alloc, Value, self.count);
	sz value_buff_used   = 0;
	HashType[] hashes    = hashes_array(self);
	Value[] exist_values = values_array(self.data, self.capacity);

	foreach (idx, hash : hashes)
	{
		if (hash >= HASH_FIRST_VALID)
		{
			value_buff[value_buff_used++] = exist_values[idx];
		}
	}

	return value_buff;
}

<*
 Returns array of all values.
*>
fn Value[] FlatSet.tvalues(&self)
{
	return self.values(tmem);
}

<*
 Iterates over each inserted value in the set.

 @require self.is_initialized() : "Must be initialized set"
*>
macro void FlatSet.@each(&self; @body(Value value))
{
	if (@unlikely(self.is_empty())) return;

	HashType[] hashes = hashes_array(self);
	Value[] values    = values_array(self.data, self.capacity);

	foreach (idx, hash : hashes)
	{
		if (hash >= HASH_FIRST_VALID)
		{
			@body(values[idx]);
		}
	}
}

fn void grow(FlatSet* self) @private
{
	HashType* old_data = self.data;
	sz old_capacity    = self.capacity;
	sz new_capacity    = max(old_capacity, DEFAULT_CAPACITY) * 2;
	sz mem_req         = calc_mem_req(new_capacity);
	HashType* new_data = (HashType*)alloc::malloc(self.alloc, mem_req);
	reset_hashes(new_data[:new_capacity]);

	sz old_idx        = 0;
	Value* old_values = values_array(old_data, old_capacity);

	for (; old_idx < old_capacity; ++old_idx)
	{
		if (old_data[old_idx] < HASH_FIRST_VALID) continue;
		// Re-index and insert.
		Value old_val = old_values[old_idx];
		HashType hash = old_data[old_idx];
		add_inner(new_data, new_capacity, null, hash, old_val);
	}

	alloc::free(self.alloc, old_data);
	self.data = new_data;
	self.capacity = new_capacity;
}

fn bool FlatSet.is_initialized(&self) @inline
{
	return self.alloc != null;
}

fn bool FlatSet.is_empty(&self) @inline
{
	return self.count == 0;
}

fn void reset_hashes(HashType[] hashes) @private
{
	foreach (&hash : hashes)
	{
		*hash = HASH_EMPTY;
	}
}

fn bool crossed_threshold(FlatSet* self) @private @inline
{
	// Capacity is a power of 2; avoid float division on every insert.
	return self.count >= (sz)(self.capacity * self.load_factor);
}

fn sz calc_mem_req(sz want_items) @private @inline
{
	return HashType::size * want_items + Value::size * want_items + Value::alignment;
}

fn HashType[] hashes_array(FlatSet* self) @private
{
	return self.data[:self.capacity];
}

fn Value[] values_array(HashType* data, sz capacity) @private
{
	Value* ptr = mem::aligned_pointer((Value*)(data + capacity), Value::alignment);
	return ptr[:capacity];
}

fn HashType calc_hash(Value val) @inline
{
	HashType hash = val.hash();
	return max(hash, HASH_FIRST_VALID);
}

<*
 @require math::is_power_of_2(capacity) : "Must be a power of 2"
*>
fn sz get_idx(HashType hash, sz capacity) @inline
{
	HashType cap_u = (HashType)capacity;
	return max((sz)(hash & (cap_u - 1u)) - (sz)HASH_FIRST_VALID, (sz)0);
}
