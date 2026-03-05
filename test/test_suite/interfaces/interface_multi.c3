import std;

interface IFoo { fn void a(); }
interface IBar { fn void b(); }

interface IFooBar: IFoo, IBar {}

struct Foo (IFooBar) { int _a; }

fn void Foo.a(&s) @dynamic {}
fn void Foo.b(&s) @dynamic {}

fn void foo(IFoo o) => o.a();
fn void bar(IBar o) => o.b();

fn void foobar(IFooBar o)
{
	o.a();
	o.b();
}

fn void main()
{
    Foo f;

    foo(&f);
    bar(&f);
    foobar(&f);
}
