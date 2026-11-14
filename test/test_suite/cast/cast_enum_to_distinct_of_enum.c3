module test;

// A distinct type over an enum is converted to with an explicit cast, like a distinct type over an
// integer: `typedef Foo = int;` follows the rules of `struct Foo { int val; }`.
enum Foo { A, B }
typedef MyFoo = Foo;
typedef InFoo = inline Foo;

fn void runtime_value(Foo f) { MyFoo m = (MyFoo)f; }
fn void runtime_value_inline(Foo f) { InFoo m = (InFoo)f; }
fn void constant_value() { MyFoo m = (MyFoo)Foo.B; }
fn void integer_value(int i) { MyFoo m = (MyFoo)i; }

// The way back, and the other enum rules, already work.
fn void back(MyFoo m) { Foo f = (Foo)m; }
fn void inline_back(InFoo m) { Foo f = m; }
fn void ordinal(MyFoo m) { int i = (int)(Foo)m; }

// It is not implicit.
fn void implicit(Foo f) { MyFoo m = f; } // #error: 'MyFoo'
fn void to_float(Foo f) { MyFoo m = (MyFoo)(float)f; } // #error: float
