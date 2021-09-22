module foo;
import bar;
func void run()
{
    @bar::test();
}

module bar;
import baz;
macro test() { baz::test(); }

module baz;
func void test() {}