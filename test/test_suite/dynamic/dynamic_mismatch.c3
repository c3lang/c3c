protocol TestProto
{
	fn int test(int ag);
}


struct Foo (TestProto) { int a; }

fn int! Foo.test(Foo* f) @dynamic { return 1; } // #error: The prototype method has a return type 'int'

struct Foo1 (TestProto) { int a; }

fn int Foo1.test(Foo1* f, int a) @dynamic { return 1; }

struct Foo2 (TestProto) { int a; }

fn int Foo2.test(Foo2* f) @dynamic { return 1; } // #error: This function is missing parameters, 2

struct Foo3 (TestProto) { int a; }

fn int Foo3.test(Foo3* f, double a) @dynamic { return 1; } // #error: The prototype argument has type 'int'

struct Foo4 (TestProto) { int a; }

fn int Foo4.test(Foo4* f, double a, int x) @dynamic { return 1; } // #error: This function has too many parameters
