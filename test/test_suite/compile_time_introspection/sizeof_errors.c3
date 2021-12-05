module foo;
import bar;
import bar::abc;

fn void a()
{
    int x = $sizeof(Bazy); // #error: 'Bazy' could not be found, did you spell it right
}

fn void b()
{
    int x = $sizeof("Bazz"); // #error: 'Bazz' could not be found, did you misspell it
}

fn void c()
{
    int x = $sizeof("Baz#"); // #error: The path to an existing member was expected after 'Baz', did you make a mistake?
}

fn void c2()
{
    int x = $sizeof("#Baz"); // #error: A valid identifier was expected here, did you want to take the length of a string literal? If so use '.len'.
}

fn void d()
{
    int x = $sizeof("Baz."); // #error: The path to an existing member was expected after 'Baz', did you make a mistake?
}

fn void e()
{
    int x = $sizeof(bar::Baze); // #error:
}

fn void f()
{
    int x = $sizeof("bar::Bazy"); // #error: 'Bazy' could not be found, did you spell it right
}

fn void g()
{
    int x = $sizeof("bar::"); // #error: A valid identifier was expected here, did you want to take the length of a string literal? If so use '.len'.
}

fn void k()
{
    int v = $sizeof("int.x"); // #error: 'int' has no members
}

fn void l()
{
    int v = $sizeof("int[].len"); // #error:  'int[]' has no members
}

fn void m()
{
    int v = $sizeof("int[4].len"); // #error: 'int[4]' has no members
}

fn void n()
{
    int v = $sizeof("Baz.x1"); // #error: The path to an existing member was expected after
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

