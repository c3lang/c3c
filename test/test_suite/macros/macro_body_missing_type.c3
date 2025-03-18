enum Foo
{
  A,
}

fn void main()
{
  @test(;$f)
  {
	Foo x = $f;
	int xd = $f; // #error: It is not possible to cast 'Foo' to 'int'
  };
}

macro @test(; @body(Foo $f))
{
  @body(A);
}