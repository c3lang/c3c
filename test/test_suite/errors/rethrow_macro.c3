module testing;
import std::io;

macro void read(src, Allocator allocator, n)
{
    char* data = allocator::malloc_try(allocator, n)!; // #error: Rethrow is only allowed in macros
    (void)io::read_all(src, data[:n]);
}

fn void main()
{
    ByteReader br;
    read(&br, tmem, 10)!!;
}