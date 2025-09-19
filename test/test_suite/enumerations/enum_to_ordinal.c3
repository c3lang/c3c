import std::io;

enum Foo
{
	A,
	B,
}

fn void Foo.hello(&self)
{
	io::printf("%d\n", self.ordinal);
}

fn void main()
{
	Foo x = B;
	x.hello();
}