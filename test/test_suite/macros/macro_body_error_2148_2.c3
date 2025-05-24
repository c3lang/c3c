fn int main()
{
  @foo(;int x = 2) // #error: Macro body parameters should
  {

  };
  return 0;
}

macro @foo(; @body(int x))
{

}
