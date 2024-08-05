module methodsof;

interface IBar
{
    fn void fnA();
    fn void fnB();
}
struct Bar (IBar)
{
    int a;
}
fn void Bar.fnB(&self) @dynamic {}
fn void Bar.fnA(&self) @dynamic {}

struct Foo
{
    int i;
    bool b;
}

fn void Foo.foo(&self) {}
fn int Foo.bar(&self, int x) { return x * self.i; }
fn bool Foo.xyz(&self) { return self.b; }

struct NoMethods
{
    int a;
}

module methodsof @test;

import std::io;

fn void methodsof() 
{
    assert(Foo.methodsof.len == 3);
    assert(Bar.methodsof.len == 2);
    assert(NoMethods.methodsof.len == 0);

    assert(Foo.methodsof[0] == "foo");
    assert(Foo.methodsof[1] == "bar");
    assert(Foo.methodsof[2] == "xyz");
    assert(Bar.methodsof[0] == "fnB");
    assert(Bar.methodsof[1] == "fnA");

    Foo foo = { .i = 4, .b = true };
    assert(Foo.$eval(Foo.methodsof[2])(&foo) == true); // Foo.xyz
    assert(Foo.$eval(Foo.methodsof[1])(&foo, 2) == 8); // Foo.bar

    Foo foo2 = { .i = 2, .b = false };
    assert(foo2.$eval(Foo.methodsof[2])() == false); // Foo.xyz
    assert(foo2.$eval(Foo.methodsof[1])(10) == 20); // Foo.bar
}
