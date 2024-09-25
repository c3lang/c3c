import std::io^;
fn void main()
{
	(void)file::open("test", "r"); // #error: std::io::file::open
}