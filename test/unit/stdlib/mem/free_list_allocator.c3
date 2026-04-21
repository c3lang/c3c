module free_list_alloc_test @test;
import free_list_alloc;
import std::io;
import std::collections::list;
import std::collections::map;

fn void sequential_alloc_free()
{
	FreeListAlloc fl;
	fl.init(mem, 4096, BEST_FIT);
	defer fl.free();

	Allocator a = (Allocator)&fl;

	DString s1;
	s1.init(a, 8);
	char[] bytes1 = "hello_world hello_world hello_world hello_world!";
	s1.append_bytes(bytes1);
	test::eq(s1.str_view(), bytes1);
	s1.free();

	DString s2;
	char[] bytes2 = "this is the SECOND phrase!";
	s2.init(a, 256);
	s2.append_bytes(bytes2);
	test::eq(s2.str_view(), bytes2);
	s2.free();

	DString s3;
	char[] bytes3 = "this is the THIRD phrase!";
	s3.init(a, 412);
	s3.append_bytes(bytes3);
	test::eq(s3.str_view(), bytes3);
	s3.free();
}

fn void mem_zero()
{
	FreeListAlloc fl;
	fl.init(mem);
	defer fl.free();
	Allocator a = (Allocator)&fl;

	char[] a1 = alloc::new_array(a, char, 256);
	char[] a2 = alloc::new_array(a, char, 256);
	char[] a3 = alloc::new_array(a, char, 256);

	foreach (el : a1)
	{
		test::eq(el, 0);
	}
	foreach (el : a2)
	{
		test::eq(el, 0);
	}
	foreach (el : a3)
	{
		test::eq(el, 0);
	}

	alloc::free(a, a1);
	alloc::free(a, a2);
	alloc::free(a, a3);
}

fn void test_list()
{
	FreeListAlloc fl;
	fl.init(mem);
	defer fl.free();
	Allocator a = (Allocator)&fl;

	char[100] bytes @noinit;
	fill_random_ascii(bytes[..]);

	List{char} l;
	l.init(a, 64);
	defer l.free();

	char[] a1 = alloc::alloc_array(a, char, 100);
	l.push_all(a1);
	test::eq(l.entries[:100], a1);
	
	char[] a2 = alloc::alloc_array(a, char, 100);
	l.push_all(a2);
	test::eq(l.entries[100:100], a2);

	char[] a3 = alloc::alloc_array(a, char, 1000);
	l.push_all(a3);
	test::eq(l.entries[1000:200], a3[800:200]);
}

fn void test_map()
{
	FreeListAlloc fl;
	fl.init(mem);
	defer fl.free();
	Allocator a = (Allocator)&fl;

	HashMap{String, int} m;
	m.init(a, 8);
	defer m.free();

	m.set("hello", 13);
	m.set("world", 476);
	m.set("key", 1000);
	m.set("val", 777);

	test::eq(m.get("hello")!!, 13);
	test::eq(m.get("world")!!, 476);
	test::eq(m.get("key")!!, 1000);
	test::eq(m.get("val")!!, 777);

	m.remove("hello")!!;
	m.remove("world")!!;
	m.remove("key")!!;
	m.remove("val")!!;
}

fn void test_dstring()
{
	FreeListAlloc fl;
	fl.init(mem);
	defer fl.free();
	Allocator a = (Allocator)&fl;

	DString s1;
	s1.init(a, 512);
	char[*] bytes1 = "hello_world hello_world hello_world hello_world!";
	s1.append_bytes(bytes1[..]);
	test::eq(s1.str_view()[:bytes1.len], bytes1[..]);

	DString s2;
	s2.init(a, 256);
	char[*] bytes2 = "this is the SECOND phrase!";
	s2.append_bytes(bytes2[..]);
	test::eq(s2.str_view()[:bytes2.len], bytes2[..]);

	DString s3;
	s3.init(a, 412);
	char[*] bytes3 = "this is the THIRD phrase!";
	s3.append_bytes(bytes3[..]);
	test::eq(s3.str_view()[:bytes3.len], bytes3[..]);

	DString s4;
	s4.init(a, 1024);
	char[*] bytes4 = "this is the FOURTH phrase!";
	s4.append_bytes(bytes4[..]);
	test::eq(s4.str_view()[:bytes4.len], bytes4[..]);
}

fn void more_allocs()
{
	FreeListAlloc fl;
	fl.init(mem, 2048);
	defer fl.free();
	Allocator a = (Allocator)&fl;

	char[100] bytes @noinit;
	fill_random_ascii(bytes[..]);

	// 256b
	char[] a1 = alloc::alloc_array(a, char, 240);
	char[] a2 = alloc::alloc_array(a, char, 240);
	char[] a3 = alloc::alloc_array(a, char, 240);
	char[] a4 = alloc::alloc_array(a, char, 240);
	char[] a5 = alloc::alloc_array(a, char, 240);
	char[] a6 = alloc::alloc_array(a, char, 240);
	char[] a7 = alloc::alloc_array(a, char, 240);

	a1[:100] = bytes[..];
	test::eq(a1[:100], bytes[..]);
	a3[100:100] = bytes[..];
	test::eq(a3[100:100], bytes[..]);
	a6[:100] = bytes[..];
	test::eq(a6[:100], bytes[..]);

	// Explicit double free edge case.
	alloc::free(a, a4);
	alloc::free(a, a4);

	alloc::free(a, a5);
	alloc::free(a, a6);
	alloc::free(a, a7);
	
	// 512b
	char[] a8 = alloc::alloc_array(a, char, 496);
	char[] a9 = alloc::alloc_array(a, char, 496);

	alloc::free(a, a1);
	alloc::free(a, a2);
	alloc::free(a, a3);
	alloc::free(a, a8);
	alloc::free(a, a9);

	// 2048b
	char[] a10 = alloc::alloc_array(a, char, 2000);
	a10[1900:100] = bytes[..];
	test::eq(a10[1900:100], bytes[..]);
	alloc::free(a, a10);
}

fn void zeroing_memory()
{
	FreeListAlloc fl;
	fl.init(mem, 1024);
	defer fl.free();
	Allocator a = (Allocator)&fl;

	// 256b
	char[] a1 = alloc::new_array(a, char, 256);

	foreach (v : a1)
	{
		test::eq(v, 0);
	}

	alloc::free(a, a1);
}

fn void test_int128()
{
	FreeListAlloc fl;
	fl.init(mem, 2048, BEST_FIT);
	defer fl.free();
	Allocator a = (Allocator)&fl;

	int128[] a1 = alloc::alloc_array(a, int128, 8);
	int128[] a2 = alloc::alloc_array(a, int128, 14);
	int128[] a3 = alloc::alloc_array(a, int128, 120);
	int128[] a4 = alloc::alloc_array(a, int128, 64);

	int128[10] buf @noinit;
	foreach (i, &el : buf[..])
	{
		*el = i;
	}

	a1[..] = buf[:8];
	test::eq(a1[..], buf[:8]);

	a2[:10] = buf[:10];
	test::eq(a2[:10], buf[:10]);

	a3[100:10] = buf[:10];
	test::eq(a3[100:10], buf[:10]);

	alloc::free(a, a1);
	alloc::free(a, a2);
	alloc::free(a, a3);
	alloc::free(a, a4);
}

fn void single_alloc_filling_entire_buf()
{
	FreeListAlloc fl;
	fl.init(mem, 2048, BEST_FIT);
	defer fl.free();
	Allocator a = (Allocator)&fl;

	char[100] bytes @noinit;
	fill_random_ascii(bytes[..]);

	char[] a1 = alloc::alloc_array(a, char, 2000);
	a1[1900:100] = bytes[..];
	test::eq(a1[1900:100], bytes[..]);

	alloc::free(a, a1);
}

// Helpers.

module free_list_alloc_test;
import std::math::random;

fn void fill_random_ascii(char[] buf)
{
	foreach (&c : buf)
	{
		*c = (char)(random::rand('z' - 'a') + 'a');
	}
}
