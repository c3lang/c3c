module linked_map_test @test;
import std::collections::list;
import std::collections::map;
import std::sort;
import std::io;

alias TestLinkedHashMap = LinkedHashMap{String, usz};

struct MapTest
{
	String key;
	usz value;
}
alias ListMap = List{MapTest};

fn void linked_map_basic()
{
	TestLinkedHashMap m;
	assert(!m.is_initialized());
	m.tinit();
	assert(m.is_initialized());
	assert(m.is_empty());
	assert(m.len() == 0);

	m.set("a", 1);
	assert(!m.is_empty());
	assert(m.len() == 1);
	m.remove("a");
	assert(m.is_empty());

	MapTest[] tcases = { {"key1", 0}, {"key2", 1}, {"key3", 2} };
	foreach (tc : tcases)
	{
		m.set(tc.key, tc.value);
	}
	assert(m.len() == tcases.len);
	foreach (tc : tcases)
	{
		usz v = m.get(tc.key)!!;
		assert(tc.value == v);
	}
}

fn void linked_map_insertion_order()
{
	TestLinkedHashMap m;
	m.tinit();
	
	String[] keys = { "first", "second", "third", "fourth" };
	foreach (i, key : keys)
	{
		m.set(key, i);
	}
	
	usz index = 0;
	m.@each(; String key, usz value)
	{
		assert(key == keys[index]);
		assert(value == index);
		index++;
	};
	
	m.remove("second");
	m.set("second", 1);
	
	String[] new_order = { "first", "third", "fourth", "second" };
	index = 0;
	m.@each(; String key, usz value)
	{
		assert(key == new_order[index]);
		index++;
	};
}

fn void linked_map_init_with_values()
{
	TestLinkedHashMap m;
	m.tinit_with_key_values("a", 1, "b", 2, "c", 3);
	
	assert(m.len() == 3);
	assert(m.get("a")!! == 1);
	assert(m.get("b")!! == 2);
	assert(m.get("c")!! == 3);
	
	// Verify order
	String[] expected_order = { "a", "b", "c" };
	usz index = 0;
	m.@each(; String key, usz value)
	{
		assert(key == expected_order[index]);
		index++;
	};
}

fn void linked_map_remove()
{
	TestLinkedHashMap m;
	assert(!@ok(m.remove("A")));
	m.tinit();
	assert(!@ok(m.remove("A")));
	m.set("A", 0);
	assert(@ok(m.remove("A")));
	
	m.set("a", 1);
	m.set("b", 2);
	m.set("c", 3);
	m.remove("b");
	
	String[] expected_order = { "a", "c" };
	usz index = 0;
	m.@each(; String key, usz value)
	{
		assert(key == expected_order[index]);
		index++;
	};
}

fn void linked_map_copy()
{
	TestLinkedHashMap hash_map;
	hash_map.tinit();

	hash_map.set("aa", 1);
	hash_map.set("b", 2);
	hash_map.set("bb", 1);

	TestLinkedHashMap hash_map_copy;
	hash_map_copy.tinit_from_map(&hash_map);

	assert(hash_map_copy.len() == hash_map.len());
	
	String[] expected_order = { "aa", "b", "bb" };
	usz index = 0;
	hash_map_copy.@each(; String key, usz value)
	{
		assert(key == expected_order[index]);
		index++;
	};
	assert(index == 3); // check if @each worked
}

fn void linked_map_iterators()
{
	TestLinkedHashMap m;
	m.tinit_with_key_values("a", 1, "b", 2, "c", 3);
	
	usz count = 0;
	LinkedHashMapIterator{String, ulong} iter = m.iter();
	while (iter.next())
	{
		count++;
		LinkedEntry {String, ulong} * current = iter.get()!!;
		assert(current.key.len > 0);
		assert(current.value > 0);
	}
	assert(count == 3);
	
	count = 0;

	LinkedHashMapKeyIterator{String, ulong} key_iter = m.key_iter(); 
	while (key_iter.next())
	{
		count++;
		assert(key_iter.get()!!.len > 0);
	}
	assert(count == 3);
	
	count = 0;
	usz sum = 0;
	LinkedHashMapValueIterator{String, ulong} value_iter = m.value_iter(); 
	
	while (value_iter.next())
	{
		count++;
		sum += *(value_iter.get()!!);
	}
	assert(count == 3);
	assert(sum == 6);
}

alias FooLinkedMap = LinkedHashMap{char, Foobar};
enum Foobar : inline char
{
	FOO,
	BAR,
	BAZ
}

constdef Foobar2 : inline int
{
	ABC = 3,
	DEF = 5,
}

fn void linked_map_inline_enum()
{
	FooLinkedMap x;
	x.tinit();
	x[Foobar.BAZ] = FOO;
	x[Foobar2.ABC] = BAR;
	test::eq(string::tformat("%s", x), "{ 2: FOO, 3: BAR }");
	x.free();
}

fn void linked_map_clear()
{
	TestLinkedHashMap m;
	m.tinit_with_key_values("a", 1, "b", 2, "c", 3);
	
	assert(m.len() == 3);
	m.clear();
	assert(m.len() == 0);
	assert(m.is_empty());
	
	m.set("x", 10);
	assert(m.len() == 1);
	assert(@ok(m.get("x")));
	assert((m.get("x")??0) == 10);
}
