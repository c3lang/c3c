import std;
fn int main()
{
	void* a;
	void* b;
	int z = a - b; // #error: A pointer diff cannot implicitly
	return 0;
}