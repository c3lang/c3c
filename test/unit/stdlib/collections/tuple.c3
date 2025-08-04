module tuple_test;

struct TestStruct
{
    String a;
    int b;
}

fn bool TestStruct.eq(self, TestStruct other) @operator(==)
    => self.a == other.a && self.b == other.b;


module tuple_test @test;

import std::collections::pair;
import std::collections::triple;


fn void pair_make_and_unpack()
{
    Pair { ulong, String } x = { 0x8034534, "some string" };

    assert(x.first == 0x8034534);
    assert(x.second == "some string");

    ulong a;
    String b;
    x.unpack(&a, &b);

    assert(a == 0x8034534);
    assert(b == "some string");
}

fn void pair_compare()
{
    Pair { TestStruct, String } x = { {"left", -123}, "right" };
    Pair { TestStruct, String } y = { {"left", -123}, "right" };
    Pair { TestStruct, String } z = { {"left", 4096}, "right" };

    assert(x == y);
    assert(x != z);
}

fn void triple_make_and_unpack()
{
    int myval = 7;
    Triple { ulong, String, int* } x = { 0xA_DEAD_C0DE, "yet another string", &myval };

    assert(x.first == 0xA_DEAD_C0DE);
    assert(x.second == "yet another string");
    assert(x.third == &myval);

    ulong a;
    String b;
    int* c;
    x.unpack(&a, &b, &c);

    assert(a == 0xA_DEAD_C0DE);
    assert(b == "yet another string");
    assert(c == &myval);
}

fn void triple_compare()
{
    Triple { TestStruct, String, String } x = { {"in", 42}, "left", "right" };
    Triple { TestStruct, String, String } y = { {"in", 42}, "left", "right" };
    Triple { TestStruct, String, String } z = { {"in", 42}, "up", "down" };

    assert(x == y);
    assert(x != z);
}
