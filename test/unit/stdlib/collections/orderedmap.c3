module orderedmap_test;
import std::collections::orderedmap;

fn bool int_cmp_desc(int a, int b) => a > b;

module orderedmap_test @test;
import std::collections::orderedmap;

alias IntStringMap     = OrderedMap{int, String};
alias FloatIntMap      = OrderedMap{float, int};
alias LongBoolMap      = OrderedMap{long, bool};
alias CharDoubleMap    = OrderedMap{char, double};

fn void basic_insert_and_get()
{
	IntStringMap m;
	m.tinit();
	defer m.free();

	m.set(2, "banana");
	m.set(1, "apple");
	m.set(3, "cherry");

	assert(m.get(1)!! == "apple");
	assert(m.get(2)!! == "banana");
	assert(m.get(3)!! == "cherry");
	assert(!@ok(m.get(99)));
}

fn void update()
{
	IntStringMap m;
	m.tinit();
	defer m.free();

	m.set(1, "one");
	m.set(1, "ONE");
	assert(m.get(1)!! == "ONE");
	assert(m.len() == 1);
}

fn void has_and_len()
{
	IntStringMap m;
	m.tinit();
	defer m.free();

	assert(m.is_empty());
	m.set(1, "a");
	m.set(2, "b");
	assert(m.len() == 2);
	assert(m.has_key(1));
	assert(!m.has_key(9));
	assert(!m.is_empty());
}

fn void remove()
{
	IntStringMap m;
	m.tinit();
	defer m.free();

	m.set(1, "one");
	m.set(2, "two");
	assert(m.remove(1));
	assert(!m.remove(1));
	assert(m.len() == 1);
	assert(!m.has_key(1));
	assert(m.get(2)!! == "two");
}

fn void clear()
{
	IntStringMap m;
	m.tinit();
	defer m.free();

	m.set(1, "one");
	m.set(2, "two");
	m.clear();
	assert(m.len() == 0);
	assert(m.is_empty());
	m.set(3, "three");
	assert(m.len() == 1);
}

fn void sorted_iteration()
{
	IntStringMap m;
	m.tinit();
	defer m.free();

	m.set(50, "fifty");
	m.set(10, "ten");
	m.set(30, "thirty");
	m.set(20, "twenty");

	int[4] expected_keys = { 10, 20, 30, 50 };
	int i = 0;
	foreach (entry : m.iter())
	{
		assert(entry.key == expected_keys[i++]);
	}
	assert(i == 4);
}

fn void first_last()
{
	IntStringMap m;
	m.tinit();
	defer m.free();

	m.set(30, "thirty");
	m.set(10, "ten");
	m.set(20, "twenty");

	assert(m.first_key()!! == 10);
	assert(m.last_key()!! == 30);
}

fn void float_keys()
{
	FloatIntMap m;
	m.tinit();
	defer m.free();

	m.set(3.14f, 1);
	m.set(1.0f, 2);
	m.set(2.71f, 3);

	float[3] expected = { 1.0f, 2.71f, 3.14f };
	int i = 0;
	foreach (entry : m.iter())
	{
		assert(entry.key == expected[i++]);
	}
	assert(i == 3);
}

fn void long_bool_keys()
{
	LongBoolMap m;
	m.tinit();
	defer m.free();

	m.set(1000L, true);
	m.set(500L, false);
	m.set(2000L, true);

	long[3] expected_keys = { 500L, 1000L, 2000L };
	bool[3] expected_vals = { false, true, true };
	int i = 0;
	foreach (entry : m.iter())
	{
		assert(entry.key == expected_keys[i]);
		assert(entry.value == expected_vals[i]);
		i++;
	}
	assert(i == 3);
}


fn void char_double_keys()
{
	CharDoubleMap m;
	m.tinit();
	defer m.free();

	m.set('z', 26.0);
	m.set('a', 1.0);
	m.set('m', 13.0);

	char[3] expected_keys = { 'a', 'm', 'z' };
	double[3] expected_vals = { 1.0, 13.0, 26.0 };
	int i = 0;
	foreach (entry : m.iter())
	{
		assert(entry.key == expected_keys[i]);
		assert(entry.value == expected_vals[i]);
		i++;
	}
	assert(i == 3);
}

fn void floor_ceiling()
{
	IntStringMap m;
	m.tinit();
	defer m.free();

	m.set(10, "ten");
	m.set(20, "twenty");
	m.set(30, "thirty");
	m.set(40, "forty");

	assert(m.floor_key(25)!! == 20);
	assert(m.floor_key(20)!! == 20);
	assert(m.floor_key(10)!! == 10);
	assert(!@ok(m.floor_key(5)));

	assert(m.ceiling_key(25)!! == 30);
	assert(m.ceiling_key(20)!! == 20);
	assert(!@ok(m.ceiling_key(45)));

	assert(m.lower_key(25)!! == 20);
	assert(m.lower_key(20)!! == 10);
	assert(!@ok(m.lower_key(10)));

	assert(m.higher_key(25)!! == 30);
	assert(m.higher_key(30)!! == 40);
	assert(!@ok(m.higher_key(40)));
}

fn void pop()
{
	IntStringMap m;
	m.tinit();
	defer m.free();

	m.set(10, "ten");
	m.set(20, "twenty");
	m.set(30, "thirty");

	var first @safeinfer = m.pop_first()!!;
	assert(first.key == 10);
	assert(first.value == "ten");
	assert(m.len() == 2);

	var last @safeinfer = m.pop_last()!!;
	assert(last.key == 30);
	assert(last.value == "thirty");
	assert(m.len() == 1);
}

fn void get_ref()
{
	IntStringMap m;
	m.tinit();
	defer m.free();

	m.set(1, "hello");
	String* ref = m.get_ref(1)!!;
	assert(*ref == "hello");
	*ref = "world";
	assert(m.get(1)!! == "world");
	assert(!@ok(m.get_ref(99)));
}

fn void get_or_set()
{
	IntStringMap m;
	m.tinit();
	defer m.free();

	String v1 = m.@get_or_set(1, "default");
	assert(v1 == "default");
	assert(m.len() == 1);

	m.set(2, "existing");
	String v2 = m.@get_or_set(2, "default");
	assert(v2 == "existing");
	assert(m.len() == 2);
}

fn void clone()
{
	IntStringMap m;
	m.tinit();
	defer m.free();

	m.set(1, "one");
	m.set(2, "two");
	m.set(3, "three");

	var c @safeinfer = m.tclone();
	defer c.free();

	m.set(1, "ONE");
	assert(m.get(1)!! == "ONE");
	assert(c.get(1)!! == "one");
	assert(c.len() == 3);
}

fn void merge()
{
	IntStringMap a;
	a.tinit();
	defer a.free();

	IntStringMap b;
	b.tinit();
	defer b.free();

	a.set(1, "one");
	a.set(2, "two");
	b.set(2, "TWO");
	b.set(3, "three");

	a.merge(&b);

	assert(a.get(1)!! == "one");
	assert(a.get(2)!! == "TWO");
	assert(a.get(3)!! == "three");
	assert(a.len() == 3);
}

fn void iter_from()
{
	IntStringMap m;
	m.tinit();
	defer m.free();

	m.set(10, "ten");
	m.set(20, "twenty");
	m.set(30, "thirty");
	m.set(40, "forty");

	int[3] expected_from20 = { 20, 30, 40 };
	int i = 0;
	foreach (entry : m.iter_from(20))
	{
		assert(entry.key == expected_from20[i++]);
	}
	assert(i == 3);

	int[2] expected_from25 = { 30, 40 };
	i = 0;
	foreach (entry : m.iter_from(25))
	{
		assert(entry.key == expected_from25[i++]);
	}
	assert(i == 2);
}

fn void iter_range()
{
	IntStringMap m;
	m.tinit();
	defer m.free();

	m.set(10, "ten");
	m.set(20, "twenty");
	m.set(30, "thirty");
	m.set(40, "forty");

	int[2] expected_15_35 = { 20, 30 };
	int i = 0;
	foreach (entry : m.iter_range(15, 35))
	{
		assert(entry.key == expected_15_35[i++]);
	}
	assert(i == 2);

	int[2] expected_10_30 = { 10, 20 };
	i = 0;
	foreach (entry : m.iter_range(10, 30))
	{
		assert(entry.key == expected_10_30[i++]);
	}
	assert(i == 2);
}

fn void custom_cmp()
{
	OrderedMap{int, String} m;
	m.tinit_with_cmp(&int_cmp_desc);
	defer m.free();

	m.set(10, "ten");
	m.set(50, "fifty");
	m.set(30, "thirty");

	int[3] expected = { 50, 30, 10 };
	int i = 0;
	foreach (entry : m.iter())
	{
		assert(entry.key == expected[i++]);
	}
	assert(i == 3);
}

fn void operators()
{
	IntStringMap m;
	m.tinit();
	defer m.free();

	assert(!(m[1] = "one"));      // new insert -> false
	assert((m[1] = "ONE"));       // update -> true

	assert(m[1]!! == "ONE");
	assert(!@ok(m[99]));

	String* ref = (&m[1])!!;
	*ref = "world";
	assert(m[1]!! == "world");
	assert(!@ok(&m[99]));
}

fn void auto_tinit()
{
	IntStringMap m;
	assert(!m.is_initialized());
	m.set(1, "one");
	assert(m.is_initialized());
	assert(m.get(1)!! == "one");
	m.free();
}

fn void empty_map_edge_cases()
{
	IntStringMap m;
	m.tinit();
	defer m.free();

	assert(!@ok(m.first_key()));
	assert(!@ok(m.last_key()));
	assert(!@ok(m.pop_first()));
	assert(!@ok(m.pop_last()));
}

fn void iter_from_past_end()
{
	IntStringMap m;
	m.tinit();
	defer m.free();

	m.set(10, "ten");
	m.set(20, "twenty");

	int i = 0;
	foreach (entry : m.iter_from(99))
	{
		i++;
	}
	assert(i == 0);
}

fn void iter_range_empty()
{
	IntStringMap m;
	m.tinit();
	defer m.free();

	m.set(10, "ten");
	m.set(20, "twenty");
	m.set(30, "thirty");

	int i = 0;
	foreach (entry : m.iter_range(20, 20))
	{
		i++;
	}
	assert(i == 0);

	i = 0;
	foreach (entry : m.iter_range(25, 15))
	{
		i++;
	}
	assert(i == 0);
}

fn void onheap_auto_init()
{
	IntStringMap m = orderedmap::ONHEAP;
	assert(!m.is_initialized());
	m.set(1, "one");
	assert(m.is_initialized());
	assert(m.get(1)!! == "one");
	m.free();
}

fn void printable()
{
	IntStringMap m;
	m.tinit();
	defer m.free();

	assert(string::tformat("%s", m) == "{  }");

	m.set(2, "two");
	m.set(1, "one");
	m.set(3, "three");
	assert(string::tformat("%s", m) == "{ 1: one, 2: two, 3: three }");
}
