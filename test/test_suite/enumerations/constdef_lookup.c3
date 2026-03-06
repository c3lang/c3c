import std::io;

constdef MyEnum : inline CInt
{
	FOO = 1,
	BAR = 5,
	BAZ = 9,
}
fn void main()
{
	io::printn(MyEnum.lookup(2)); // #error: No method or inner struct/union 'MyEnum.lookup' found
}