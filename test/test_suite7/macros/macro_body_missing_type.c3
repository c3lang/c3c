enum Foo
{
  A,
}

fn void main()
{
  @test(;$f) // #error: should be explicitly
  {

  };
}

macro @test(; @body(Foo $f))
{
  @body(A);
}