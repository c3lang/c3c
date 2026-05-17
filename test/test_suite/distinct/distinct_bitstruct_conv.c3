typedef Foo = int;

bitstruct Bar : Foo
{
  bool a;
}

fn int main()
{
	Bar b;
	(Foo)b;
	(int)b;
	($Typefrom(Bar::inner))b;
	return 0;
}