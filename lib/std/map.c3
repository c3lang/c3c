module std::map<Key, Value>;
import std::math;

const uint DEFAULT_INITIAL_CAPACITY = 16;
const uint MAXIMUM_CAPACITY = 1u << 31;
const float DEFAULT_LOAD_FACTOR = 0.75;

private struct Entry
{
	uint hash;
	Key key;
	Value value;
	Entry* next;
}
struct HashMap
{
	Entry*[] table;
	Allocator* allocator;
	uint count; // Number of elements
	uint threshold; // Resize limit
	float load_factor;
}

/**
 * @require capacity > 0
 * @require load_factor > 0.0 && load_factor < 1.0
 * @require map.table.len == 0
 * @require capacity < MAXIMUM_CAPACITY
 * @require allocator != null
 **/
fn void HashMap.init(HashMap* map, uint capacity = DEFAULT_INITIAL_CAPACITY, float load_factor = DEFAULT_LOAD_FACTOR, Allocator* allocator = mem::current_allocator())
{
	capacity = math::next_power_of_2(capacity);
	map.allocator = allocator;
	map.load_factor = load_factor;
	map.threshold = (uint)(capacity * load_factor);
	map.table = array::make(Entry*, capacity, allocator);
}

fn void HashMap.init_from_map(HashMap* map, HashMap* other_map, Allocator* allocator = mem::current_allocator())
{
	map.init(other_map.table.len, other_map.load_factor, allocator);
	map.put_all_for_create(other_map);
}

fn bool HashMap.is_empty(HashMap* map) @inline
{
	return !map.count;
}

fn Value*! HashMap.get_ref(HashMap* map, Key key)
{
	uint hash = rehash(key.hash());
	for (Entry *e = map.table[index_for(hash, map.table.len)]; e != null; e = e.next)
	{
		if (e.hash == hash && equals(key, e.key)) return &e.value;
	}
	return SearchResult.MISSING!;
}

fn Value! HashMap.get(HashMap* map, Key key)
{
	return *map.get_ref(key) @inline;
}

fn bool HashMap.has_key(HashMap* map, Key key)
{

	return try(map.get_ref(key));
}

fn bool HashMap.set(HashMap* map, Key key, Value value)
{
	uint hash = rehash(key.hash());
	uint index = index_for(hash, map.table.len);
	for (Entry *e = map.table[index]; e != null; e = e.next)
	{
		if (e.hash == hash && equals(key, e.key))
		{
			e.value = value;
			return true;
		}
	}
	map.add_entry(hash, key, value, index);
	return false;
}

fn void! HashMap.remove(HashMap* map, Key key) @maydiscard
{
	if (!map.remove_entry_for_key(key)) return SearchResult.MISSING!;
}

fn void HashMap.clear(HashMap* map)
{
	if (!map.count) return;
	foreach (Entry** &entry_ref : map.table)
	{
		Entry* entry = *entry_ref;
		if (!entry) continue;
		map.free(entry);
		*entry_ref = null;
	}
	map.count = 0;
}

fn void HashMap.destroy(HashMap* map)
{
	map.clear();
	map.free(map.table.ptr);
	map.table = Entry*[] {};
}
fn Key[] HashMap.key_tlist(HashMap* map)
{
	return map.key_list(mem::temp_allocator());
}

fn Key[] HashMap.key_list(HashMap* map, Allocator* allocator = null)
{
	if (!map.count) return Key[] {};

	Key[] list = array::make(Key, map.count, allocator ?: map.allocator);
	usize index = 0;
	foreach (Entry* entry : map.table)
	{
		while (entry)
		{
			list[index++] = entry.key;
			entry = entry.next;
		}
	}
	return list;
}

fn Value[] HashMap.value_tlist(HashMap* map)
{
	return map.value_list(mem::temp_allocator());
}

fn Value[] HashMap.value_list(HashMap* map, Allocator* allocator = null)
{
	if (!map.count) return Value[] {};
	Value[] list = array::make(Value, map.count, allocator ?: map.allocator);
	usize index = 0;
	foreach (Entry* entry : map.table)
	{
		while (entry)
		{
			list[index++] = entry.value;
			entry = entry.next;
		}
	}
	return list;
}

$if (types::is_equatable(Value)):
fn bool HashMap.has_value(HashMap* map, Value v)
{
	if (!map.count) return false;
	foreach (Entry* entry : map.table)
	{
		while (entry)
		{
			if (equals(v, entry.value)) return true;
			entry = entry.next;
		}
	}
	return false;
}
$endif;

// --- private methods

private fn void HashMap.add_entry(HashMap* map, uint hash, Key key, Value value, uint bucket_index)
{
	Entry* entry = map.allocator.alloc(Entry.sizeof)!!;
	*entry = { .hash = hash, .key = key, .value = value, .next = map.table[bucket_index] };
	map.table[bucket_index] = entry;
	if (map.count++ >= map.threshold)
	{
		map.resize(map.table.len * 2);
    }
}

private fn void HashMap.resize(HashMap* map, uint new_capacity)
{
	Entry*[] old_table = map.table;
	uint old_capacity = old_table.len;
	if (old_capacity == MAXIMUM_CAPACITY)
	{
		map.threshold = uint.max;
		return;
	}
	Entry*[] new_table = array::make(Entry*, new_capacity, map.allocator);
	map.transfer(new_table);
	map.table = new_table;
	map.free(old_table.ptr);
	map.threshold = (uint)(new_capacity * map.load_factor);
}

private fn uint rehash(uint hash) @inline
{
    hash ^= (hash >> 20) ^ (hash >> 12);
    return hash ^ ((hash >> 7) ^ (hash >> 4));
}

private macro uint index_for(uint hash, uint capacity)
{
	return hash & (capacity - 1);
}

private fn void HashMap.transfer(HashMap* map, Entry*[] new_table)
{
	Entry*[] src = map.table;
    uint new_capacity = new_table.len;
    foreach (uint j, Entry *e : src)
    {
        if (!e) continue;
        do
        {
            Entry* next = e.next;
            uint i = index_for(e.hash, new_capacity);
            e.next = new_table[i];
            new_table[i] = e;
            e = next;
        }
        while (e);
    }
}

private fn void HashMap.put_all_for_create(HashMap* map, HashMap* other_map)
{
	if (!other_map.count) return;
    foreach (Entry *e : other_map.table)
    {
        if (!e) continue;
        map.put_for_create(e.key, e.value);
    }
}

private fn void HashMap.put_for_create(HashMap* map, Key key, Value value)
{
    uint hash = rehash(key.hash());
    uint i = index_for(hash, map.table.len);
	for (Entry *e = map.table[i]; e != null; e = e.next)
	{
		if (e.hash == hash && equals(key, e.key))
		{
			e.value = value;
			return;
		}
	}
	map.create_entry(hash, key, value, i);
}

private fn void HashMap.free(HashMap* map, void* ptr)
{
	map.free(ptr);
}

private fn bool HashMap.remove_entry_for_key(HashMap* map, Key key)
{
	uint hash = rehash(key.hash());
	uint i = index_for(hash, map.table.len);
	Entry* prev = map.table[i];
	Entry* e = prev;
	while (e)
	{
		Entry *next = e.next;
		if (e.hash == hash && equals(key, e.key))
		{
			map.count--;
			if (prev == e)
			{
				map.table[i] = next;
			}
			else
			{
				prev.next = next;
			}
			map.free(e);
			return true;
		}
		prev = e;
		e = next;
	}
	return false;
}

private fn void HashMap.create_entry(HashMap* map, uint hash, Key key, Value value, int bucket_index)
{
	Entry *e = map.table[bucket_index];
	Entry* entry = map.allocator.alloc(Entry.sizeof)!!;
	*entry = { .hash = hash, .key = key, .value = value, .next = map.table[bucket_index] };
	map.table[bucket_index] = entry;
	map.count++;
}