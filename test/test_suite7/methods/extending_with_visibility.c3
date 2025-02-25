module test;
import abc;

struct Foo
{
	int x;
}

fn int Foo.get1(Foo* f) @private => f.x;
fn int Foo.get2(Foo* f) => f.x;
fn int Foo.get3(Foo* f) @local => f.x;

fn int Bar.get1(Bar* f) @private => f.x;
fn int Bar.get2(Bar* f) => f.x;
fn int Bar.get3(Bar* f) @local => f.x;

fn int main()
{
	Foo x = { 1 };
	x.get1();
	x.get2();
	x.get3();
	Bar y = { 1 };
	y.get1();
	y.get2();
	y.get3();
	return 1;
}

module abc;

struct Bar
{
	int x;
}
