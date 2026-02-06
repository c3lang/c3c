import std;
fn void a()
{
	usz{} i = Z; // #error: A user-defined generic type was expected here, but the type was 'usz' (ulong)
}
fn int main()
{
	return 0;
}