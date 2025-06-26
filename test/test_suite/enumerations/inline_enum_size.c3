/*
enum Bar : inline usz
{
  FOO,
  BAR,
}

enum Bar2 : usz (inline int x)
{
  FOO = 1,
  BAR = 4,
}

int[Bar.BAR] x;

fn int main()
{
	char[Bar2.BAR] a;
	return 0;
}
*/