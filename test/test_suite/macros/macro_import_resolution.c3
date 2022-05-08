module foo;
import bar;
fn void run()
{
    bar::test();
}

module bar;
import baz;
macro test() { baz::test(); }

module baz;
fn void test() {}