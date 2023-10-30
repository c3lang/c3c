module testing;
import std::io;

macro char[] read(src, Allocator* allocator, n)
{
    char* data = allocator.alloc_checked(n)!; // #error: Rethrow is only allowed in macros
    io::read_all(src, data[:n])!;
}

fn void main()
{
    ByteReader br;
    read(&br, mem::temp(), 10);
}