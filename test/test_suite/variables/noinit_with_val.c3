fn int main()
{
  test();
  return 0;
}

macro test()
{
  var x @noinit = 0; // #error: '@noinit' variables may not have initializers
}
