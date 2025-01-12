module test;

macro void @foo(#a)
{
	#a;
}

macro void @bar(#a)
{
	$typeof(#a) x;
}
fn int main()
{
	int $i;
	var $Type;
	@foo($Type = int); // #error: only be modified
	@foo($i = 2); // #error: only be modified
	@foo($i += 1); // #error: only be modified
	@bar($Type = int); // #error: only be modified
	@bar($i = 2); // #error: only be modified
	@bar($i += 1); // #error: only be modified
	return 0;
}