import std::io;

fn void main()
{
  $eval("foo()");  // #error: could not be resolved to a valid
}
fn void foo()
{
  io::printfn("foo");
}