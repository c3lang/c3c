module test;
fn VoidFn foo()
{
  return fn void() {};
}

fn void main()
{
  foo() @inline ();
  foo() @inline (foo()); // #error: This argument would exceed the number of parameters
}
