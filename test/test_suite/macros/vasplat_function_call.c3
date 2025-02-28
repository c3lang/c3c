import std;

def IList = List(<int>);
fn IList new()
{
	IList l;
	l.init(mem, $vasplat); // #error: can only be used inside
	return l;
}


fn void main()
{
	IList a = new(123, 123, 123, 123, 123, 123, 123, 134);
	a.push(567);

	io::printfn("%s", a[0]);
}