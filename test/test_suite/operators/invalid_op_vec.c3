struct Foo { int x; }

fn void Foo.test(Foo f) @operator(intvec) {} // #error: @operator(intvec) can only be used with macros.
fn void Foo.test(Foo f) @operator(floatvec) {} // #error: @operator(floatvec) can only be used with macros.
macro void Foo.test(Foo f) @operator(intvec) {} // #error: @operator(intvec) cannot be used with methods
macro void Foo.test(Foo f) @operator(floatvec) {} // #error: @operator(floatvec) cannot be used with methods

