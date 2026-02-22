module test;
fn void foo() @inline @noinline // #error: cannot be combined
{}

fn void bar() @used @unused // #error: cannot be combined
{}

fn bool baz() @nodiscard @noreturn // #error: cannot be combined
{}
