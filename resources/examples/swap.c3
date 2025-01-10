module test;
import std::io;

<*
 @require $defined(#a = #b, #b = #a)
*>
macro void @swap(#a, #b)
{
    var temp = #a;
    #a = #b;
    #b = temp;
}

fn void main()
{
	int x = 123;
	int y = 456;
	@swap(x, y);
	io::printfn("x: %d y: %d", x, y);
}