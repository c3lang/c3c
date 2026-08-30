module std::core::mem_test @test;
import std;

struct MemTestPair
{
	int x;
	int y;
}

struct MemTestOveraligned @align(32)
{
	int a;
}

struct MemTestPacked @packed
{
	char pad;
	int value;
}

fn void test_equals_slice_equal_and_not_equal()
{
	uint[*] a = { 1, 2 };
	uint[*] b = { 1, 2 };
	uint[*] c = { 1, 3 };

	assert(mem::equals(a[..], b[..]));
	assert(!mem::equals(a[..], c[..]));
}

fn void test_equals_slice_different_length()
{
	uint[*] a = { 1, 2, 3 };
	uint[*] b = { 1, 2 };

	assert(!mem::equals(a[..], b[..]));
}

fn void test_equals_handles_byte_tails()
{
	char[*] a = { 1, 2, 3, 4, 5 };
	char[*] b = { 1, 2, 3, 4, 5 };
	char[*] c = { 1, 2, 3, 4, 6 };

	assert(mem::equals(a[..], b[..]));
	assert(!mem::equals(a[..], c[..]));
}

fn void test_equals_pointer_mode()
{
	char[*] a = { 1, 2, 3, 4, 5, 6, 7, 8, 9 };
	char[*] b = { 1, 2, 3, 4, 5, 6, 7, 8, 9 };

	assert(mem::equals(&a, &b, 9));
	b[8] = 10;
	assert(!mem::equals(&a, &b, 9));
}

fn void test_equals_zero_length()
{
	char[*] a = { 1 };
	char[*] b = { 2 };

	assert(mem::equals(&a, &b, 0));
}

fn void test_equals_explicit_align_overaligned()
{
	char[*] a = { 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17 };
	char[*] b = { 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17 };

	assert(mem::equals(&a, &b, 17, $align: 32));
	b[16] = 0;
	assert(!mem::equals(&a, &b, 17, $align: 32));
}

fn void test_aligned_offset()
{
	test::eq(mem::aligned_offset(0, 16), (sz)0);
	test::eq(mem::aligned_offset(1, 16), (sz)16);
	test::eq(mem::aligned_offset(16, 16), (sz)16);
	test::eq(mem::aligned_offset(17, 16), (sz)32);
}

fn void test_aligned_pointer()
{
	void* p = (void*)(uptr)17;
	void* aligned = mem::aligned_pointer(p, 16);
	test::eq((uptr)aligned, (uptr)32);
}

fn void test_ptr_is_aligned()
{
	test::@check(mem::ptr_is_aligned((void*)(uptr)32, 16));
	test::@check(!mem::ptr_is_aligned((void*)(uptr)33, 16));
}

fn void test_ptr_is_page_aligned()
{
	sz page = mem::os_pagesize();
	test::@check(mem::ptr_is_page_aligned((void*)(uptr)0));
	test::@check(mem::ptr_is_page_aligned((void*)(uptr)page));
	test::@check(!mem::ptr_is_page_aligned((void*)(uptr)(page + 1)));
}

fn void test_os_pagesize()
{
	sz page = mem::os_pagesize();
	test::@check(page > 0);
	test::@check(math::is_power_of_2(page));
}

fn void test_clear()
{
	char[*] a = { 1, 2, 3, 4 };
	mem::clear(&a, a.len);
	foreach (v : a) test::eq(v, (char)0);
}

fn void test_clear_inline()
{
	char[*] a = { 1, 2, 3, 4 };
	mem::clear_inline(&a, 4);
	foreach (v : a) test::eq(v, (char)0);
}

fn void test_zero_volatile()
{
	char[*] a = { 1, 2, 3, 4 };
	mem::zero_volatile(a[..]);
	foreach (v : a) test::eq(v, (char)0);
}

fn void test_copy()
{
	char[*] src = { 1, 2, 3, 4 };
	char[4] dst;
	mem::copy(&dst, &src, 4);
	test::@check(mem::equals(&dst, &src, 4));
}

fn void test_copy_inline()
{
	char[*] src = { 1, 2, 3, 4 };
	char[4] dst;
	mem::copy_inline(&dst, &src, 4);
	test::@check(mem::equals(&dst, &src, 4));
}

fn void test_move_forward_overlap()
{
	char[*] buf = { 1, 2, 3, 4, 5 };
	mem::move(&buf[0], &buf[1], 4);
	char[*] expected = { 2, 3, 4, 5, 5 };
	test::@check(mem::equals(&buf, &expected, 5));
}

fn void test_move_backward_overlap()
{
	char[*] buf = { 1, 2, 3, 4, 5 };
	mem::move(&buf[1], &buf[0], 4);
	char[*] expected = { 1, 1, 2, 3, 4 };
	test::@check(mem::equals(&buf, &expected, 5));
}

fn void test_set()
{
	char[4] buf;
	mem::set(&buf, 0x7F, 4);
	foreach (v : buf) test::eq(v, (char)0x7F);
}

fn void test_set_inline()
{
	char[4] buf;
	mem::set_inline(&buf, 0x7F, 4);
	foreach (v : buf) test::eq(v, (char)0x7F);
}

fn void test_malloc_calloc_realloc_free()
{
	char* zeroed = mem::calloc(8);
	for (int i = 0; i < 8; i++) test::eq(zeroed[i], (char)0);

	char* buf = mem::malloc(4);
	buf[0] = 1;
	buf[1] = 2;
	buf[2] = 3;
	buf[3] = 4;
	buf = mem::realloc(buf, 8);
	test::eq(buf[0], (char)1);
	test::eq(buf[3], (char)4);

	mem::free(zeroed);
	mem::free(buf);
}

fn void test_realloc_and_free_accept_null()
{
	void* p = mem::realloc(null, 8);
	test::@check(p != null);
	mem::free(p);
	mem::free(null);
}

fn void test_malloc_aligned_calloc_aligned_free_aligned()
{
	void* a = mem::malloc_aligned(64, 64);
	test::@check(mem::ptr_is_aligned(a, 64));
	mem::free_aligned(a);

	void* b = mem::calloc_aligned(64, 64);
	test::@check(mem::ptr_is_aligned(b, 64));
	char* bytes = b;
	for (int i = 0; i < 64; i++) test::eq(bytes[i], (char)0);
	mem::free_aligned(b);
}

fn void test_realloc_aligned()
{
	void* a = mem::malloc_aligned(64, 64);
	a = mem::realloc_aligned(a, 128, 64);
	test::@check(mem::ptr_is_aligned(a, 64));
	mem::free_aligned(a);
}

fn void test_realloc_aligned_and_free_aligned_accept_null()
{
	void* p = mem::realloc_aligned(null, 64, 64);
	test::@check(p != null);
	mem::free_aligned(p);
	mem::free_aligned(null);
}

fn void test_new_is_zeroed_alloc_is_not_required_to_be()
{
	MemTestPair* zeroed = mem::new(MemTestPair);
	test::eq(zeroed.x, 0);
	test::eq(zeroed.y, 0);
	mem::free(zeroed);

	MemTestPair* initialized = mem::new(MemTestPair, { 3, 4 });
	test::eq(initialized.x, 3);
	test::eq(initialized.y, 4);
	mem::free(initialized);

	MemTestPair* p = mem::alloc(MemTestPair);
	p.x = 5;
	p.y = 6;
	test::eq(p.x, 5);
	test::eq(p.y, 6);
	mem::free(p);
}

fn void test_new_with_padding_and_alloc_with_padding()
{
	MemTestPair* p = mem::new_with_padding(MemTestPair, 16);
	test::eq(p.x, 0);
	mem::set((void*)p + MemTestPair::size, 0x11, 16);
	mem::free(p);

	MemTestPair* q = mem::alloc_with_padding(MemTestPair, 16);
	q.x = 1;
	q.y = 2;
	test::eq(q.x, 1);
	test::eq(q.y, 2);
	mem::free(q);
}

fn void test_new_aligned_and_alloc_aligned()
{
	MemTestOveraligned* a = mem::new_aligned(MemTestOveraligned);
	test::eq(a.a, 0);
	test::@check(mem::ptr_is_aligned(a, MemTestOveraligned::alignment));
	mem::free_aligned(a);

	MemTestOveraligned* b = mem::alloc_aligned(MemTestOveraligned);
	b.a = 7;
	test::eq(b.a, 7);
	test::@check(mem::ptr_is_aligned(b, MemTestOveraligned::alignment));
	mem::free_aligned(b);
}

fn void test_new_array_and_alloc_array()
{
	int[] zeroed = mem::new_array(int, 4);
	foreach (v : zeroed) test::eq(v, 0);
	mem::free(zeroed.ptr);

	int[] arr = mem::alloc_array(int, 4);
	for (int i = 0; i < 4; i++) arr[i] = i;
	for (int i = 0; i < 4; i++) test::eq(arr[i], i);
	mem::free(arr.ptr);
}

fn void test_new_array_aligned_and_alloc_array_aligned()
{
	MemTestOveraligned[] zeroed = mem::new_array_aligned(MemTestOveraligned, 4);
	foreach (v : zeroed) test::eq(v.a, 0);
	test::@check(mem::ptr_is_aligned(zeroed.ptr, MemTestOveraligned::alignment));
	mem::free_aligned(zeroed.ptr);

	MemTestOveraligned[] arr = mem::alloc_array_aligned(MemTestOveraligned, 4);
	test::@check(mem::ptr_is_aligned(arr.ptr, MemTestOveraligned::alignment));
	mem::free_aligned(arr.ptr);
}

fn void test_tmalloc_tcalloc_trealloc()
{
	@pool()
	{
		char* zeroed = mem::tcalloc(8);
		for (int i = 0; i < 8; i++) test::eq(zeroed[i], (char)0);

		char* buf = mem::tmalloc(4);
		buf[0] = 9;
		buf = mem::trealloc(buf, 8);
		test::eq(buf[0], (char)9);

		void* p = mem::trealloc(null, 8);
		test::@check(p != null);
	};
}

fn void test_tnew_talloc_and_padding_variants()
{
	@pool()
	{
		MemTestPair* zeroed = mem::tnew(MemTestPair);
		test::eq(zeroed.x, 0);

		MemTestPair* initialized = mem::tnew(MemTestPair, { 1, 2 });
		test::eq(initialized.x, 1);
		test::eq(initialized.y, 2);

		MemTestPair* p = mem::talloc(MemTestPair);
		p.x = 3;
		test::eq(p.x, 3);

		MemTestPair* padded = mem::temp_with_padding(MemTestPair, 16, { 4, 5 });
		test::eq(padded.x, 4);
		test::eq(padded.y, 5);

		MemTestPair* padded2 = mem::talloc_with_padding(MemTestPair, 16);
		padded2.x = 6;
		test::eq(padded2.x, 6);
	};
}

fn void test_talloc_array_and_temp_array()
{
	@pool()
	{
		int[] zeroed = mem::temp_array(int, 4);
		foreach (v : zeroed) test::eq(v, 0);

		int[] arr = mem::talloc_array(int, 4);
		for (int i = 0; i < 4; i++) arr[i] = i + 1;
		for (int i = 0; i < 4; i++) test::eq(arr[i], i + 1);
	};
}

fn void test_clone_and_clone_slice()
{
	MemTestPair original = { 1, 2 };
	MemTestPair* cloned = mem::@clone(original);
	test::eq(cloned.x, 1);
	test::eq(cloned.y, 2);
	original.x = 99;
	test::eq(cloned.x, 1);
	mem::free(cloned);

	int[*] arr = { 1, 2, 3 };
	int[] cloned_slice = mem::@clone_slice(arr[..]);
	test::eq(cloned_slice.len, 3);
	arr[0] = 99;
	test::eq(cloned_slice[0], 1);
	mem::free(cloned_slice.ptr);
}

fn void test_clone_aligned()
{
	MemTestOveraligned original = { 7 };
	MemTestOveraligned* cloned = mem::@clone_aligned(original);
	test::eq(cloned.a, 7);
	test::@check(mem::ptr_is_aligned(cloned, MemTestOveraligned::alignment));
	mem::free_aligned(cloned);
}

fn void test_tclone_and_tclone_slice()
{
	@pool()
	{
		MemTestPair original = { 3, 4 };
		MemTestPair* cloned = mem::@tclone(original);
		test::eq(cloned.x, 3);
		test::eq(cloned.y, 4);

		int[*] arr = { 5, 6, 7 };
		int[] cloned_slice = mem::@tclone_slice(arr[..]);
		test::eq(cloned_slice.len, 3);
		test::eq(cloned_slice[1], 6);
	};
}

fn void test_scoped_swaps_and_restores_allocator()
{
	TrackingAllocator tracker;
	tracker.init(mem);

	Allocator before = mem;
	mem::@scoped(&tracker)
	{
		void* p = mem::malloc(16);
		test::@check(tracker.allocated() >= 16);
		mem::free(p);
	};
	test::@check(mem.ptr == before.ptr);
	tracker.free();
}

fn void test_stack_mem()
{
	mem::@stack_mem(256; Allocator alloc)
	{
		char* p = alloc::malloc(alloc, 32);
		mem::set(p, 0x22, 32);
		test::eq(p[0], (char)0x22);
	};
}

fn void test_stack_pool()
{
	mem::@stack_pool(256)
	{
		char* p = mem::malloc(32);
		mem::set(p, 0x33, 32);
		test::eq(p[0], (char)0x33);
	};
}

fn void test_temp_push_and_pop()
{
	PoolState state = mem::temp_push();
	char* p = mem::tmalloc(16);
	mem::set(p, 0x44, 16);
	test::eq(p[0], (char)0x44);
	mem::temp_pop(state);
}

fn void test_assert_leak_no_leak_does_not_abort()
{
	@assert_leak()
	{
		void* p = mem::malloc(8);
		mem::free(p);
	};
}

fn void test_type_alloc_must_be_aligned()
{
	test::@check(!mem::type_alloc_must_be_aligned(int));
	test::@check(mem::type_alloc_must_be_aligned(MemTestOveraligned));
}

fn void test_volatile_get_set()
{
	int x = 1;
	mem::volatile::Volatile{int}* v = (mem::volatile::Volatile{int}*)&x;
	v.set(42);
	test::eq(v.get(), 42);
	test::eq(x, 42);
}

fn void test_volatile_load_store_macros()
{
	int x = 1;
	mem::@volatile_store(x, 7);
	test::eq(mem::@volatile_load(x), 7);
	test::eq(x, 7);
}

fn void test_unaligned_ref_get_set()
{
	char* buf = mem::malloc(64);
	defer mem::free(buf);
	mem::alignment::UnalignedRef{int, 1} ref = (mem::alignment::UnalignedRef{int, 1})(buf + 1);
	ref.set(12345);
	test::eq(ref.get(), 12345);
}

fn void test_unaligned_addr()
{
	MemTestPacked p = { 0, 999 };
	var addr @safeinfer = mem::@unaligned_addr(p.value);
	addr.set(42);
	test::eq(addr.get(), 42);
	test::eq(p.value, 42);
}

fn void test_load_store_macros()
{
	int x = 5;
	mem::store(&x, 9, 4);
	test::eq(mem::load(&x, 4), 9);
}

fn void test_compare_exchange()
{
	int x = 1;
	int witnessed = mem::compare_exchange(&x, 1, 2);
	test::eq(witnessed, 1);
	test::eq(x, 2);

	int failed_witness = mem::compare_exchange(&x, 1, 3);
	test::eq(failed_witness, 2);
	test::eq(x, 2);
}

fn void test_compare_exchange_volatile()
{
	int x = 1;
	int witnessed = mem::compare_exchange_volatile(&x, 1, 5);
	test::eq(witnessed, 1);
	test::eq(x, 5);
}

fn void test_atomic_load_store()
{
	int x = 1;
	mem::@atomic_store(x, 8);
	test::eq(mem::@atomic_load(x), 8);
	test::eq(x, 8);
}
