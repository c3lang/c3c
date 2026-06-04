alias Xy = int;
typedef Xz = inline int;
fn int main()
{
	int b, c;
	long a = (Xy)(b + c);
	a = a + (int)(b + c);
	a = a + (Xz)(b + c);
	return 0;
}
