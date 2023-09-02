module testing;
import std::io;

macro char[] read(src, allocator, n)
{
    char* data = allocator.alloc(n)!; // #error: Rethrow is only allowed in macros
    src.read_all(data[:n])!;
}

fn void main()
{
    ByteReader br;
    read(&br, mem::temp(), 10);
}