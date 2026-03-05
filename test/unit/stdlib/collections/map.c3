module map_test @test;
import std::collections::list;
import std::collections::map;
import std::sort;
import std::io;

alias TestHashMap = HashMap{String, usz};

struct MapTest
{
    String key;
    usz value;
}
alias ListMap = List{MapTest};

fn void map()
{
    TestHashMap m;
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

    ListMap list;
    list.tinit();
    m.@each(;String key, usz value)
    {
        list.push({key, value});
    };
    assert(list.len() == tcases.len);
    quicksort(&list, fn int (MapTest a, MapTest b) => (int)(a.value - b.value));
    foreach (i, tc : tcases)
    {
        assert(tc.key == list[i].key);
        assert(tc.value == list[i].value);
    }
}

alias FooMap = HashMap{char, Foobar};
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

fn void map_inline_enum()
{
	FooMap x;
	x[Foobar.BAZ] = FOO;
	x[Foobar2.ABC] = BAR;
	test::eq(string::tformat("%s", x), "{ 2: FOO, 3: BAR }");
	x.free();
}

fn void map_remove()
{
	TestHashMap m;
	assert(!@ok(m.remove("A")));
	m.tinit();
	assert(!@ok(m.remove("A")));
	m.set("A", 0);
	assert(@ok(m.remove("A")));
}

fn void map_copy()
{
	TestHashMap hash_map;
	hash_map.tinit();

	hash_map.set("aa", 1);
	hash_map.set("b", 2);
	hash_map.set("bb", 1);

	TestHashMap hash_map_copy;
	hash_map_copy.tinit_from_map(&hash_map);

	assert(hash_map_copy.len() == hash_map.len());

}

alias Test = HashMap{String, HashMap {String, String}};
fn void test_ref()
{
    Test t;
    t.init(tmem);
    (&t["a"]).init(tmem);

    t["a"]["b"] = "ab";
    test::eq("ab", t["a"]["b"]!!);
}


