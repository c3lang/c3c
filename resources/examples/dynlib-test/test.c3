import std;
extern fn int adder(int a, int b);

fn int main()
{
	io::printn(adder(1, 4));
	return 0;
}