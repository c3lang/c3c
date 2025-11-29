module test;

macro foo(int $a, int... $i)
{
	$foreach $ab : $i:
		$echo $ab;
	$endforeach
}
fn int main()
{
	int a;
	foo(3, 1, 2, a); // #error: All vaargs must be contant values
	int[3] x = { 1, 2, 3 };
	foo(3, ...x); // #error: The splat must be a compile time value
	return 0;
}

