import std;

def IList = List(<int>);
fn IList IList.foo() @operator(construct)
{
  IList l;
  l.new_init($vasplat); // #error: can only be used inside
  return l;
}


fn void main()
{
  IList a = IList.foo(123, 123, 123, 123, 123, 123, 123, 134);
  a.push(567);

  io::printfn("%s", a[0]);
}
