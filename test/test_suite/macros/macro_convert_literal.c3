module foo;
import std::io;

macro foo(y)
{
    return y * y;
}

func void main()
{
    io::printf("%d\n", @foo(10));
}