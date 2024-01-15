// #target: macos-x64
module test;
import std::io;
import libc;
enum Foo
{
	ABC
}

fn void print_pages()
{
	allocator::temp().print_pages(io::stdout())!!;
}

fn void setstring(char* dst, String str)
{
	foreach (char c : str)
	{
		dst++[0] = c;
	}
	dst[0] = 0;
}

fn void testAllocator(Allocator* a, int val)
{
	io::printn("Test");
	void* data = a.malloc_aligned(val, 128, 16)!!;
	io::printf("Aligned with offset %p, align 16: %s offset align 128: %s\n", data, mem::ptr_is_aligned(data, 16), mem::ptr_is_aligned(data + 16, 128));
	data = a.calloc_aligned(val, 128, 16)!!;
	io::printf("Aligned with offset %p, align 16: %s offset align 128: %s\n", data, mem::ptr_is_aligned(data, 16), mem::ptr_is_aligned(data + 16, 128));
	data = a.realloc_aligned(data, (usz)val + 1, 128, 16)!!;
	io::printf("Aligned with offset %p, align 16: %s offset align 128: %s\n", data, mem::ptr_is_aligned(data, 16), mem::ptr_is_aligned(data + 16, 128));
	data = a.realloc_aligned(data, (usz)val + 1, 128, 0)!!;
	io::printf("No offset %p, align 16: %s offset align 128: %s\n", data, mem::ptr_is_aligned(data, 16), mem::ptr_is_aligned(data + 16, 128));
	io::printfn("Freeing %p", data);
	a.free_aligned(data);
}
fn void main()
{
	char* small = tmalloc(128);
	setstring(small, "small");
	libc::printf("Small1: %p %s\n", small, small);
	print_pages();
	small = trealloc(small, 129, 1024 * 16);
	libc::printf("Small2: %p %s\n", small, small);
	print_pages();
	small = trealloc(small, 12933);
	libc::printf("Small3: %p %s\n", small, small);
	print_pages();
	char* first_big = tmalloc(9512);
	void *big = tmalloc(4095);
	io::printf("Big: %p\n", big);
	io::printf("Small: %p\n", tmalloc(13));
	print_pages();
	@pool() {
	big = trealloc(big, 5067);
	print_pages();
	void* hidden = tmalloc(4096);
	io::printf("Hidden: %p\n", hidden);
	io::printf("Big: %p\n", big);
	big = trealloc(big, 4096, 256);
	io::printf("Big: %p\n", big);
	io::printf("First big: %p\n", first_big);
	print_pages();
	};
	mem::@scoped(allocator::temp())
	{
		io::printf("Malloc: %p\n", (void*)malloc(23));
		io::printf("Malloc: %p\n", (void*)malloc(23));
	};
	io::printf("Malloc: %p\n", (void*)malloc(23));
	@pool()
	{
		io::printf("Talloc: %p\n", (void*)tmalloc(22));
	};
	testAllocator(allocator::temp(), 126);
	testAllocator(allocator::temp(), 12600);
	ArenaAllocator aa;
	aa.init(&&char[1024] {});
	testAllocator(&aa, 126);
	io::printn("Test dynamic arena");
	DynamicArenaAllocator dynamic_arena;
	dynamic_arena.init(1024, allocator::heap());
    testAllocator(&dynamic_arena, 112);
    testAllocator(&dynamic_arena, 712);
	first_big[3] = 123;
}