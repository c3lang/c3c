import std;
alias Foo = fn void();

fn int main()
{
	int x;
	io::printn($defined($nameof("hello")));
	io::printn($defined($nameof(x)));
	return 0;
}