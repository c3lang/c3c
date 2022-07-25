module foo;
import bar;
import bar::abc;

fn void a()
{
    int x = Bazy.sizeof; // #error: 'Bazy' could not be found, did you spell it right
}

fn void b()
{
    int x = $evaltype("Bazz").sizeof; // #error: 'Bazz' could not be found, did you spell it rig
}


fn void e()
{
    int x = bar::Baze.sizeof; // #error: 'bar::Baze' could not be found, did you spell it right
}

fn void f()
{
    int x = $evaltype("bar::Bazy").sizeof; // #error: 'bar::Bazy' could not be found
}

fn void g()
{
    int x = $evaltype("bar::").sizeof; // #error: A valid name was expected here.
}

fn void k()
{
    int v = $evaltype("int").x.sizeof; // #error: 'int' does not have a property 'x'.
}

fn void l()
{
    int v = $sizeof(int[].len); // #error:  'int[]' does not have a property 'len'
}

fn void m()
{
    int v = $sizeof(int[4].len);
}

fn void n()
{
    int v = Baz.x1.sizeof; // #error: No method or inner struct/union 'Baz.x1' found.
}



module bar;

struct Baz
{
    int x;
    char[60] y;
}

module bar::abc;

struct Foo
{
    char x;
}

