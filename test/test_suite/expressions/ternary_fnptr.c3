alias Func = fn void();
enum Cmd : uint (Func handler)
{
	A { true ? &f : null }
}
fn void f() {}
fn void main () {}