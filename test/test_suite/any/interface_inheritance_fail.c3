import std;

interface IFoo { fn void a(); }
interface IBar { fn void b(); }

interface IFooBar: IFoo, IBar {}

struct Foo (IFooBar) { int _a; } // #error: was not fully implemented
struct Foo2 (IFoo, IBar) { int _a; }

fn void Foo2.a(&s) @dynamic {}
fn void Foo2.b(&s) @dynamic {}

