module foo;
import bar;
fn void run()
{
    bar::test();
}

fn void run2()
{
    bar::test2();
}

fn void tester() @private {}

module bar;
macro test()
{
    tester(); // #error: function 'tester' is '@private' and not visible
}

macro test2()
{
    foo::tester(); // #error: 'foo::tester' is '@private' and not visible
}
