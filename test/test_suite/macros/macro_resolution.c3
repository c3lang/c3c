module foo;
import bar;
func void run()
{
    @bar::test();
}

func void run2()
{
    @bar::test2();
}

func void tester() {}

module bar;
macro test()
{
    tester(); // #error: 'tester' could not be found, did you spell it right
}

macro test2()
{
    foo::tester(); // #error: Unknown module 'foo', did you forget to import it?
}
