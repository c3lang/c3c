module testi;

macro @foo(#x)
{
	#x *= 2; // #error: You cannot assign to a constant expression
}

attrdef @On_Start(x) = @tag("on_start", @foo(x));

fn void some() @On_Start(fn void(int a) { ; })
{
}

fn int main() => main2();

macro main2()
{
	some.tagof("on_start")(1);
	some();
	return 0;
}