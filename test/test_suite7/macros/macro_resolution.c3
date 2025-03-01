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
    tester(); // #error: 'tester' could not be found, did you spell it right
}

macro test2()
{
    foo::tester(); // #error: 'foo::tester' could not be found, did you spell
}
