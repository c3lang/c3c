module foo;
import bar;

fn void a()
{
    int x = Bazy.sizeof; // #error: 'Bazy' could not be found, did you spell it right
}

fn void b()
{
    int x = $typefrom("Bazz").sizeof; // #error: 'Bazz' could not be found, did you spell it rig
}


fn void e()
{
    int x = bar::Baze.sizeof; // #error: 'bar::Baze' could not be found, did you spell it right
}

fn void f()
{
    int x = $typefrom("bar::Bazy").sizeof; // #error: 'bar::Bazy' could not be found
}

fn void g()
{
    int x = $typefrom("bar::").sizeof; // #error: could not be resolved to a valid symbol
}

fn void k()
{
    int v = $typefrom("int").x.sizeof; // #error: 'x'.
}

fn void l()
{
    int v = $sizeof(int[].len); // #error: 'len'
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

