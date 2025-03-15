module testing;
import std::io;
import foo;

fn void main()
{
    FooInt foo;
}

typedef FooInt = inline Foo;

struct Bar
{
    FooInt list;
}

fn void Bar.set(&self, int x)
{
    self.list.set(x);
}

module foo;

struct Foo
{
    int x;
}

fn void Foo.set(&self, int x)
{
    self.x = x;
}