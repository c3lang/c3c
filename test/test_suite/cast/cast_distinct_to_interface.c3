module test;
typedef DString2 = inline char*;
interface Collectable
{
	fn void append(void* element);
}

fn Collectable foo(Collectable c) { return c; }

fn int main()
{
	foo((Collectable) (DString2) {}); // #error: You can only convert pointers to an interface like
	foo((Collectable) (DString) {}); // #error: You can only convert pointers to an interface like
	return 0;
}