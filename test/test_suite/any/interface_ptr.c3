interface Abc {}

fn void main()
{
	Abc y;
	typeid z = y.type;
	void* x = y.ptr;
}
