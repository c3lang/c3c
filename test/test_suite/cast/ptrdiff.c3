import std;
fn int main()
{
	void* a;
	void* b;
	int z = a - b; // #error: A pointer diff has the type
	return 0;
}