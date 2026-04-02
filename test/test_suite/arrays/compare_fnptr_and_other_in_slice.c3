alias Bob = fn void();
extern fn int a(Bob[] );
struct Aa
{
	int x;
}
fn int Aa.a(b)
{
	a(""); // #error: It is not possible to cast 'String' to 'Bob[]' (fn void()[])
}