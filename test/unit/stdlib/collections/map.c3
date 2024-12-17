module map_test @test;
import std::collections::list;
import std::collections::map;
import std::sort;
import std::io;

def TestHashMap = HashMap(<String, usz>);
def TestMap = Map(<String, usz>);

struct MapTest
{
    String key;
    usz value;
}
def List = List(<MapTest>);

fn void map()
{
    TestHashMap m;
    assert(!m.is_initialized());
    m.temp_init();
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

    List list;
    list.temp_init();
    m.@each(;String key, usz value)
    {
        list.push({key, value});
    };
    assert(list.len() == tcases.len);
    quicksort(list, fn int (MapTest a, MapTest b) => (int)(a.value - b.value));
    foreach (i, tc : tcases)
    {
        assert(tc.key == list[i].key);
        assert(tc.value == list[i].value);
    }
}

fn void map_remove()
{
	TestHashMap m;
	assert(!@ok(m.remove("A")));
	m.temp_init();
	assert(!@ok(m.remove("A")));
	m.set("A", 0);
	assert(@ok(m.remove("A")));
}

fn void map_copy()
{
	TestHashMap hash_map;
	hash_map.temp_init();

	hash_map.set("aa", 1);
	hash_map.set("b", 2);
	hash_map.set("bb", 1);

	TestHashMap hash_map_copy;
	hash_map_copy.temp_init_from_map(&hash_map);

	assert(hash_map_copy.len() == hash_map.len());

	TestMap map = map::temp(<String, usz>)();

	map.set("aa", 1);
	map.set("b", 2);
	map.set("bb", 1);

	TestMap map_copy = map::temp_from_map(<String, usz>)(map);

	assert(map_copy.len() == map.len());
}
