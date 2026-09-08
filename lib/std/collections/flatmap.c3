<*
 @require types::has_equals(Key) : "Must have equality operator overloaded"
 @require $defined((Key) { }.hash()) : "Must be hashable"
*>
module std::collections::flatmap <Key, Value>;
import std::io;
import std::math;
import std::collections::pair;

alias HashType = uint;
const sz DEFAULT_CAPACITY @private       = 16;
const float DEFAULT_LOAD_FACTOR @private = 0.85f;
const HashType HASH_EMPTY @private       = 0;
const HashType HASH_DELETED @private     = 1;
const HashType HASH_FIRST_VALID @private = 2;

<*
 FlatMap is an open-addressing map that keeps every entry in one contiguous,
 cache-friendly block, unlike separate chaining where each key is a separate
 heap node linked from its bucket. The flat design is memory-efficient and
 fast to iterate and probe, but fills under load: the table periodically
 rebuilds into a larger block, and collisions or delete/re-add churn degrade
 probing performance.
*>
struct FlatMap
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
 @require !self.is_initialized() : "Map was already initialized"
*>
fn FlatMap* FlatMap.init(
	&self,
	Allocator allocator,
	sz capacity = DEFAULT_CAPACITY,
	float load_factor = DEFAULT_LOAD_FACTOR
)
{
	capacity = math::next_power_of_2(capacity);
	sz mem_req = calc_mem_req(capacity);
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
 @require !self.is_initialized() : "Map was already initialized"
*>
fn FlatMap* FlatMap.tinit(&self, sz capacity = DEFAULT_CAPACITY, float load_factor = DEFAULT_LOAD_FACTOR)
{
	return self.init(tmem, capacity, load_factor);
}

<*
 @param [in] keys : "The keys for the FlatMap entries"
 @param [in] values : "The values for the FlatMap entries"
 @param [&inout] allocator : "The allocator to use"
 @require keys.len == values.len : "Both keys and values arrays must be the same length"
 @require capacity > 0 : "The capacity must be 1 or higher"
 @require load_factor > 0.0 && load_factor <= 1 : "The load factor must be higher than 0 and less or equal to 1"
 @require !self.is_initialized() : "Map was already initialized"
*>
fn FlatMap* FlatMap.init_from_keys_and_values(
	&self,
	Allocator allocator,
	Key[] keys,
	Value[] values,
	sz capacity = DEFAULT_CAPACITY,
	float load_factor = DEFAULT_LOAD_FACTOR
)
{
	self.init(allocator, capacity, load_factor);
	foreach (idx, key : keys)
	{
		self.set(key, values[idx]);
	}
	return self;
}

<*
 @param [in] keys : "The keys for the FlatMap entries"
 @param [in] values : "The values for the FlatMap entries"
 @require keys.len == values.len : "Both keys and values arrays must be the same length"
 @require capacity > 0 : "The capacity must be 1 or higher"
 @require load_factor > 0.0 && load_factor <= 1 : "The load factor must be higher than 0 and less or equal to 1"
 @require !self.is_initialized() : "Map was already initialized"
 @require keys.len == values.len : "Arrays must have equal length"
*>
fn FlatMap* FlatMap.tinit_from_keys_and_values(
	&self,
	Key[] keys,
	Value[] values,
	sz capacity = DEFAULT_CAPACITY,
	float load_factor = DEFAULT_LOAD_FACTOR
)
{
	return self.init_from_keys_and_values(tmem, keys, values, capacity, load_factor);
}

<*
 Frees all underlying resources.
 Shouldn't be used after this operation without initialization.
*>
fn void FlatMap.free(&self)
{
	if (self.data)
	{
		alloc::free(self.alloc, self.data);
		*self = { };
	}
}

<*
 Resets underlying data, so flat map can be reused.
*>
fn void FlatMap.clear(&self)
{
	self.count = 0;
	HashType[] hashes = self.data[:self.capacity];
	reset_hashes(hashes);
}

<*
 Inserts key-value pair into the flat map.
*>
fn void FlatMap.set(&self, Key key, Value val)
{
	if (crossed_threshold(self)) grow(self);
	HashType hash = calc_hash(key);
	set_inner(self.data, self.capacity, &self.count, hash, key, val);
}

fn void set_inner(HashType* data, sz capacity, sz* count, HashType hash, Key key, Value val) @private @inline
{
	sz start_idx            = get_idx(hash, capacity);
	sz curr_idx             = start_idx;
	Pair{Key[], Value[]} kv = keys_values_array(data, capacity);
	Key[] keys              = kv.first;
	Value[] values          = kv.second;

	for (; curr_idx < capacity; ++curr_idx)
	{
		if (data[curr_idx] < HASH_FIRST_VALID)
		{
			data[curr_idx] = hash;
			keys[curr_idx] = key;
			values[curr_idx] = val;
			if (count) (*count)++;
			return;
		}
		else if (hash == data[curr_idx] && keys[curr_idx] == key)
		{
			data[curr_idx] = hash;
			keys[curr_idx] = key;
			values[curr_idx] = val;
			return;
		}
	}

	// Wrapped around, start from first entry.
	curr_idx = 0;

	for (; curr_idx < start_idx; ++curr_idx)
	{
		if (data[curr_idx] < HASH_FIRST_VALID)
		{
			data[curr_idx] = hash;
			keys[curr_idx] = key;
			values[curr_idx] = val;
			if (count) (*count)++;
			return;
		}
		else if (hash == data[curr_idx] && keys[curr_idx] == key)
		{
			data[curr_idx] = hash;
			keys[curr_idx] = key;
			values[curr_idx] = val;
			return;
		}
	}

	unreachable("Failed to insert a value into a hashmap");
}

<*
 Get reference to the value associated with the provided key.
*>
fn Value*? FlatMap.get_ref(&self, Key key)
{
	HashType search_hash    = calc_hash(key);
	sz start_idx            = get_idx(search_hash, self.capacity);
	sz curr_idx             = start_idx;
	Pair{Key[], Value[]} kv = keys_values_array(self.data, self.capacity);
	Key[] keys              = kv.first;
	Value[] values          = kv.second;

	for (; curr_idx != self.capacity; ++curr_idx)
	{
		if (self.data[curr_idx] == HASH_EMPTY) return NOT_FOUND~;
		if (self.data[curr_idx] == search_hash && keys[curr_idx] == key)
		{
			return &values[curr_idx];
		}
	}

	// if we searched from the start, no need to do wrap around search.
	if (start_idx == 0) return NOT_FOUND~;

	curr_idx = 0;

	for (; curr_idx < start_idx; ++curr_idx)
	{
		if (self.data[curr_idx] == HASH_EMPTY) return NOT_FOUND~;
		if (self.data[curr_idx] == search_hash && keys[curr_idx] == key)
		{
			return &values[curr_idx];
		}
	}

	return NOT_FOUND~;
}

<*
 Get value associated with the provided key.
*>
fn Value? FlatMap.get(&self, Key key)
{
	if (try value_ref = self.get_ref(key))
	{
		return *value_ref;
	}
	return NOT_FOUND~;
}

<*
 Checks if flat map has a value associated with the provided key.
*>
fn bool FlatMap.has(&self, Key key)
{
	if (catch self.get(key)) return false;
	return true;
}

<*
 Removes key-value pair from a flat map if it exists.
*>
fn void? FlatMap.remove(&self, Key key) @maydiscard
{
	HashType search_hash    = calc_hash(key);
	sz start_idx            = get_idx(search_hash, self.capacity);
	sz curr_idx             = start_idx;
	Pair{Key[], Value[]} kv = keys_values_array(self.data, self.capacity);
	Key[] keys              = kv.first;
	Value[] values          = kv.second;

	for (; curr_idx < self.capacity; ++curr_idx)
	{
		if (self.data[curr_idx] == HASH_EMPTY) return NOT_FOUND~;
		if (self.data[curr_idx] == search_hash && keys[curr_idx] == key)
		{
			self.data[curr_idx] = HASH_DELETED;
			self.count--;
			return;
		}
	}

	// if we searched from the start, no need to do wrap around search.
	if (start_idx == 0) return NOT_FOUND~;

	curr_idx = 0;

	for (; curr_idx < start_idx; ++curr_idx)
	{
		if (self.data[curr_idx] == HASH_EMPTY) return NOT_FOUND~;
		if (self.data[curr_idx] == search_hash && keys[curr_idx] == key)
		{
			self.data[curr_idx] = HASH_DELETED;
			self.count--;
			return;
		}
	}

	return NOT_FOUND~;
}

<*
 Iterates over each key-value pair in the map.

 @require self.is_initialized() : "Must be initialized set"
*>
macro void FlatMap.@each_pair(&self; @body(Key key, Value value))
{
	if (@unlikely(self.is_empty())) return;

	HashType[] hashes = hashes_array(self);
	Key[] keys        = keys_array(self.data, self.capacity);
	Value[] values    = values_array(self.data, self.capacity);

	foreach (idx, hash : hashes)
	{
		if (hash >= HASH_FIRST_VALID)
		{
			@body(keys[idx], values[idx]);
		}
	}
}

<*
 Iterates over each key in the map.

 @require self.is_initialized() : "Must be initialized set"
*>
macro void FlatMap.@each_key(&self; @body(Key key))
{
	if (@unlikely(self.is_empty())) return;

	HashType[] hashes = hashes_array(self);
	Key[] keys        = keys_array(self.data, self.capacity);

	foreach (idx, hash : hashes)
	{
		if (hash >= HASH_FIRST_VALID)
		{
			@body(keys[idx]);
		}
	}
}

<*
 Iterates over each value in the map.

 @require self.is_initialized() : "Must be initialized set"
*>
macro void FlatMap.@each_value(&self; @body(Value value))
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

fn void grow(FlatMap* self) @private
{
	HashType* old_data = self.data;
	sz old_capacity    = self.capacity;
	sz new_capacity    = max(old_capacity, DEFAULT_CAPACITY) * 2;
	sz mem_req         = calc_mem_req(new_capacity);
	HashType* new_data = (HashType*)alloc::malloc(self.alloc, mem_req);
	reset_hashes(new_data[:new_capacity]);

	sz old_idx               = 0;
	Pair{Key[], Value[]} kvs = keys_values_array(old_data, old_capacity);
	Key[] old_keys           = kvs.first;
	Value[] old_values       = kvs.second;

	for (; old_idx < old_capacity; ++old_idx)
	{
		if (old_data[old_idx] < HASH_FIRST_VALID) continue;
		// Re-index and insert.
		Key old_key   = old_keys[old_idx];
		Value old_val = old_values[old_idx];
		HashType hash = old_data[old_idx];
		set_inner(new_data, new_capacity, null, hash, old_key, old_val);
	}

	alloc::free(self.alloc, old_data);
	self.data = new_data;
	self.capacity = new_capacity;
}

fn bool FlatMap.is_initialized(&self) @inline
{
	return self.alloc != null;
}

fn bool FlatMap.is_empty(&self) @inline
{
	return self.count == 0;
}

<*
 Returns array of all keys.
*>
fn Key[] FlatMap.keys(&self, Allocator alloc)
{
	Key[] key_buff    = alloc::alloc_array(alloc, Key, self.count);
	sz key_buff_used  = 0;
	Key[] exist_keys  = keys_array(self.data, self.capacity);
	HashType[] hashes = hashes_array(self);

	foreach (idx, hash : hashes)
	{
		if (hash >= HASH_FIRST_VALID)
		{
			key_buff[key_buff_used++] = exist_keys[idx];
		}
	}

	return key_buff;
}

<*
 Returns array of all keys.
*>
fn Key[] FlatMap.tkeys(&self)
{
	return self.keys(tmem);
}

<*
 Returns array of all values.
*>
fn Value[] FlatMap.values(&self, Allocator alloc)
{
	Value[] value_buff  = alloc::alloc_array(alloc, Value, self.count);
	sz value_buff_used  = 0;
	Value* exist_values = values_array(self.data, self.capacity);
	HashType[] hashes   = hashes_array(self);

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
fn Value[] FlatMap.tvalues(&self)
{
	return self.values(tmem);
}

fn void reset_hashes(HashType[] hashes) @private
{
	foreach (&hash : hashes)
	{
		*hash = HASH_EMPTY;
	}
}

fn bool crossed_threshold(FlatMap* self) @inline @private
{
	// Capacity is a power of 2; avoid float division on every insert.
	return self.count >= (sz)(self.capacity * self.load_factor);
}

fn sz calc_mem_req(sz want_items) @private @inline
{
	return HashType::size * want_items + Key::size * want_items + Value::size * want_items + Key::alignment
		+ Value::alignment;
}

fn Pair{Key[], Value[]} keys_values_array(HashType* data, sz capacity) @private
{
	Pair{Key[], Value[]} res @noinit;
	res.first = ((Key*)mem::aligned_pointer((Key*)(data + capacity), Key::alignment))[:capacity];
	res.second = ((Value*)mem::aligned_pointer((Value*)(res.first.ptr + capacity), Value::alignment))[:capacity];
	return res;
}

fn HashType[] hashes_array(FlatMap* self) @private @inline
{
	return self.data[:self.capacity];
}

fn Key[] keys_array(HashType* data, sz capacity) @private
{
	return ((Key*)mem::aligned_pointer((Key*)(data + capacity), Key::alignment))[:capacity];
}

fn Value[] values_array(HashType* data, sz capacity) @private
{
	Key* keys = keys_array(data, capacity);
	return ((Value*)mem::aligned_pointer((Value*)(keys + capacity), Value::alignment))[:capacity];
}

fn HashType calc_hash(Key key) @inline
{
	HashType hash = key.hash();
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
