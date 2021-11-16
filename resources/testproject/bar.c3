module bar;

import baz::foo;
import testbaz::zab;

fn void test()
{
    Foo x;
    baz::foo::test();
    foo::test();
    baz::foo::Foo y;
    foo::Foo d;
    Zab z;
}
