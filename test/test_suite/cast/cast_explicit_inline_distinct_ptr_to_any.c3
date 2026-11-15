module test;

// A pointer wrapped in an inline distinct type converts implicitly to `any` and to an interface
// (see cast_inline_distinct_ptr_to_any.c3), so an explicit cast must work as well.
interface Foo
{
	fn void bar();
}

struct Impl (Foo)
{
	int x;
}

fn void Impl.bar(&self) @dynamic {}

typedef IntPtr = inline int*;
typedef ImplPtr = inline Impl*;
typedef OuterPtr = inline IntPtr;

fn void inline_ptr_to_any(IntPtr p) { any a = (any)p; }
fn void inline_ptr_to_interface(ImplPtr p) { Foo f = (Foo)p; }
fn void nested_inline_ptr_to_any(OuterPtr p) { any a = (any)p; }

// A distinct type that is not inline is like a struct wrapping the pointer: it is not the pointer,
// so it does not convert to `any` or to an interface (`(any)&p` is probably what was meant).
typedef MyPtr = int*;
typedef MyImplPtr = Impl*;
typedef InsideMyPtr = inline MyPtr;

fn void distinct_ptr_to_any(MyPtr p) { any a = (any)p; } // #error: 'any'
fn void distinct_ptr_to_interface(MyImplPtr p) { Foo f = (Foo)p; } // #error: 'Foo'
fn void inline_of_distinct_ptr_to_any(InsideMyPtr p) { any a = (any)p; } // #error: 'any'
fn void distinct_ptr_to_any_implicit(MyPtr p) { any a = p; } // #error: 'any'
