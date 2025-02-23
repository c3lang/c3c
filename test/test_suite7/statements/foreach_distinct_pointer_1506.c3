distinct Type = char*;
fn usz Type.len(str)
{
	return 0;
}
fn int main(String[] args)
{
  Type x = "AAAAA";  // Or ZString
  foreach(y : x) // #error: It's not possible to enumerate an expression
  {

  }
  return 0;
}