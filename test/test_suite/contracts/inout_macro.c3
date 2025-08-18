import std;
<*
 @param [&out] x
 @require $defined(*x) : "x must be a pointer"
*>
macro foo(x) => *x = 1;

fn int main()
{
	foo(0); // #error: x must be
	return 0;
}