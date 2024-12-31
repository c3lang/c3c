module test;

def Bob = Test(<any>);
fn void Bob.crash(&self) {}

fn int main()
{
  Test(<any>) foo;
  return 0;
}
<*
 @require $defined(String.hash)
*>
module test::generic(<Some_Type>);

enum Test
{
	ABC
}