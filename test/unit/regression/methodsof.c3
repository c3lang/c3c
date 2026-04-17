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

bitstruct BazBits : char
{
    int a : 0..2;
    int b : 4..6;
    bool c : 7;
}

fn void BazBits.fn1(&self) {}
fn void BazBits.fn2(&self) {}

union AUnion {
    int y;
    double z;
}

fn void AUnion.a(&self) {}
fn void AUnion.b(&self) {}


module methodsof @test;

import std::io;

fn void methodsof() 
{
    assert(Foo::methods.len == 3);
    assert(Bar::methods.len == 2);
    assert(NoMethods::methods.len == 0);

    assert(Foo::methods[0] == "foo");
    assert(Foo::methods[1] == "bar");
    assert(Foo::methods[2] == "xyz");
    assert(Bar::methods[0] == "fnB");
    assert(Bar::methods[1] == "fnA");

    assert(BazBits::methods[0] == "fn1");
    assert(BazBits::methods[1] == "fn2");

    assert(AUnion::methods[0] == "a");
    assert(AUnion::methods[1] == "b");

    Foo foo = { .i = 4, .b = true };
    assert(Foo.$eval(Foo::methods[2])(&foo) == true); // Foo.xyz
    assert(Foo.$eval(Foo::methods[1])(&foo, 2) == 8); // Foo.bar
    assert(Foo.$eval(Foo::methods[2])(&foo) == true); // Foo.xyz
    assert(Foo.$eval(Foo::methods[1])(&foo, 2) == 8); // Foo.bar

    Foo foo2 = { .i = 2, .b = false };
    assert(foo2.$eval(Foo::methods[2])() == false); // Foo.xyz
    assert(foo2.$eval(Foo::methods[1])(10) == 20); // Foo.bar
}
