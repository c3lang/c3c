import std::io;

fn void main()
{
  $eval("foo()");  // #error: with $eval
}
fn void foo()
{
  io::printfn("foo");
}