module foo;
import std::io;

macro foo(y)
{
    return y * y;
}

fn void main()
{
    libc::printf("%d\n", foo(10));
}