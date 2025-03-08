import std;

def IList = List{int};
fn IList IList.newAbc(&self)
{
	IList l;
	l.init(mem, $vasplat); // #error: can only be used inside
	return l;
}


fn void main()
{
	IList x;
	IList a = x.newAbc(123, 123, 123, 123, 123, 123, 123, 134);
	a.push(567);

	io::printfn("%s", a[0]);
}
