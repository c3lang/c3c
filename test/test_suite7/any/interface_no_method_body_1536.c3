interface Foo
{
  fn int bar()
  {                 // #error: An interface method
    return 0;
  }
}