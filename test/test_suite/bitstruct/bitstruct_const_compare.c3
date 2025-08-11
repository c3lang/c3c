module test;

bitstruct Foo : int
{
  bool foo;
}

bitstruct Foo2 : int
{
	bool a : 1;
	int b : 7..9;
}

fn void main()
{
	var $foo = (Foo) { true };
	var $bar = (Foo) { };
	bool $baz = $foo == $bar;
	$assert($baz == false);
	$baz = $foo == $foo;
	$assert($baz == true);
	$assert(true == types::has_equals(Foo));
	Foo2 $a1 = { true, 2 };
	Foo2 $a2 = {};
	Foo2 $a3 = { true, 3 };
	Foo2 $a4 = { false, 0 };
	$assert(!($a1 == $a2));
	$assert($a1 != $a2);
	$assert($a2 == $a2);
	$assert(!($a2 != $a2));
	$assert($a1 != $a3);
	$assert(!($a1 == $a3));
	$assert($a4 == $a2);
	$assert(!($a4 != $a2));

}