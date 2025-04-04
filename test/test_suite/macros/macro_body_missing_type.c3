enum Foo
{
  A,
}

fn void main()
{
  @test(;$f)
  {
	Foo x = $f;
	int xd = $f; // #error: you can use .ordinal
  };
}

macro @test(; @body(Foo $f))
{
  @body(A);
}