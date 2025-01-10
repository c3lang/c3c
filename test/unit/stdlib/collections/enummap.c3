module enummap_test @test;
import std::collections::enummap;

enum FooEnum
{
    ONE,
    TWO,
    THREE,
}

def FooEnumMap = EnumMap(<FooEnum, uint>);

fn void enums()
{
    FooEnumMap nm;
    nm.set(ONE, 1);
    nm.set(TWO, 2);
    nm.set(THREE, 3);

    assert(nm[FooEnum.ONE] == 1);
    assert(nm[FooEnum.TWO] == 2);
    assert(nm[FooEnum.THREE] == 3);

    nm.init(0);

    assert(nm[FooEnum.ONE] == 0);
    assert(nm[FooEnum.TWO] == 0);
    assert(nm[FooEnum.THREE] == 0);
}
