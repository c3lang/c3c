module foo;
import bar;
import bar::abc;

func void a()
{
    int x = $sizeof(Bazy); // #error: 'Bazy' could not be found, did you spell it right
}

func void b()
{
    int x = $sizeof("Bazz"); // #error: 'Bazz' could not be found, did you misspell it
}

func void c()
{
    int x = $sizeof("Baz#"); // #error: The path to an existing member was expected after 'Baz', did you make a mistake?
}

func void c2()
{
    int x = $sizeof("#Baz"); // #error: '#Baz' is not a valid identifier, did you misspell it?
}

func void d()
{
    int x = $sizeof("Baz."); // #error: The path to an existing member was expected after 'Baz', did you make a mistake?
}

func void e()
{
    int x = $sizeof(bar::Baze); // #error: 'Baze' could not be found, did you spell it right
}

func void f()
{
    int x = $sizeof("bar::Bazy"); // #error: 'Bazy' could not be found, did you spell it right
}

func void g()
{
    int x = $sizeof("bar::"); // #error: 'bar::' is not a valid identifier, did you misspell it?
}

func void k()
{
    int v = $sizeof("int.x"); // #error: 'int' has no members
}

func void l()
{
    int v = $sizeof("int[].len"); // #error:  'int[]' has no members
}

func void m()
{
    int v = $sizeof("int[4].len"); // #error: 'int[4]' has no members
}

func void n()
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

