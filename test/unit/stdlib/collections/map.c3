module map_test @test;
import std::collections::list;
import std::collections::map;
import std::sort;
import std::io;

def Map = HashMap(<String, usz>);

struct MapTest
{
    String key;
    usz value;
}
def List = List(<MapTest>);

fn void map()
{
    Map m;
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