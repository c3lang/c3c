module atomic_rt_test @test;
import compiler_rt;

const CInt SEQ_CST = 6;

fn void is_lock_free()
{
	assert(compiler_rt::__atomic_is_lock_free(1, null));
	assert(compiler_rt::__atomic_is_lock_free(2, null));
	assert(compiler_rt::__atomic_is_lock_free(4, null));
	assert(!compiler_rt::__atomic_is_lock_free(16, null));
	assert(!compiler_rt::__atomic_is_lock_free(3, null));
}

fn void load_store_char()
{
	char val, result;
	char src = 0xAB;
	compiler_rt::__atomic_store(1, &val, &src, SEQ_CST);
	compiler_rt::__atomic_load(1, &val, &result, SEQ_CST);
	test::eq(result, 0xAB);
}

fn void load_store_short()
{
	short val, result;
	short src = 0x1234;
	compiler_rt::__atomic_store(2, &val, &src, SEQ_CST);
	compiler_rt::__atomic_load(2, &val, &result, SEQ_CST);
	test::eq(result, 0x1234);
}

fn void load_store_int()
{
	int val, result;
	int src = 12345;
	compiler_rt::__atomic_store(4, &val, &src, SEQ_CST);
	compiler_rt::__atomic_load(4, &val, &result, SEQ_CST);
	test::eq(result, 12345);
}

fn void load_store_long()
{
	$if iptr::size >= 8:
		long val, result;
		long src = 0x123456789ABCDEF0;
		compiler_rt::__atomic_store(8, &val, &src, SEQ_CST);
		compiler_rt::__atomic_load(8, &val, &result, SEQ_CST);
		assert(result == 0x123456789ABCDEF0);
	$endif
}

fn void load_store_large()
{
	// 16-byte value exercises the lock-based fallback path.
	int[4] val;
	int[4] src = { 10, 20, 30, 40 };
	int[4] result;
	compiler_rt::__atomic_store(16, &val, &src, SEQ_CST);
	compiler_rt::__atomic_load(16, &val, &result, SEQ_CST);
	test::eq(result, (int[4]){ 10, 20, 30, 40});
}

fn void exchange_int()
{
	int val = 100;
	int new_val = 200;
	int old;
	compiler_rt::__atomic_exchange(4, &val, &new_val, &old, SEQ_CST);
	assert(old == 100, "expected old 100, got %d", old);
	int loaded;
	compiler_rt::__atomic_load(4, &val, &loaded, SEQ_CST);
	assert(loaded == 200, "expected 200, got %d", loaded);
}

fn void exchange_large()
{
	// Lock-based fallback path.
	int[4] val = { 1, 2, 3, 4 };
	int[4] new_val = { 5, 6, 7, 8 };
	int[4] old;
	compiler_rt::__atomic_exchange(16, &val, &new_val, &old, SEQ_CST);
	test::eq(old,  (int[4]){ 1, 2, 3, 4 });
	int[4] loaded;
	compiler_rt::__atomic_load(16, &val, &loaded, SEQ_CST);
	test::eq(loaded, (int[4]){ 5, 6, 7, 8 });
}

fn void compare_exchange_success()
{
	int val = 42;
	int expected = 42;
	int desired = 99;
	CInt result = compiler_rt::__atomic_compare_exchange(4, &val, &expected, &desired, 0, SEQ_CST, SEQ_CST);
	assert(result == 1, "CAS should have succeeded");
	int loaded;
	compiler_rt::__atomic_load(4, &val, &loaded, SEQ_CST);
	assert(loaded == 99, "expected 99, got %d", loaded);
}

fn void compare_exchange_failure()
{
	int val = 42;
	int expected = 10; // Wrong expected → should fail
	int desired = 99;
	CInt result = compiler_rt::__atomic_compare_exchange(4, &val, &expected, &desired, 0, SEQ_CST, SEQ_CST);
	test::eq(result, 0);
	test::eq(expected, 42);
	int loaded;
	compiler_rt::__atomic_load(4, &val, &loaded, SEQ_CST);
	test::eq(loaded, 42);
}

fn void compare_exchange_large_success()
{
	// Lock fallback path.
	int[4] val = { 1, 2, 3, 4 };
	int[4] expected = { 1, 2, 3, 4 };
	int[4] desired = { 5, 6, 7, 8 };
	CInt result = compiler_rt::__atomic_compare_exchange(16, &val, &expected, &desired, 0, SEQ_CST, SEQ_CST);
	assert(result == 1, "CAS should have succeeded");
	int[4] loaded;
	compiler_rt::__atomic_load(16, &val, &loaded, SEQ_CST);
	test::eq(loaded, (int[4]){ 5, 6, 7, 8 });
}

fn void compare_exchange_large_failure()
{
	// Lock fallback path, mismatch.
	int[4] val = { 1, 2, 3, 4 };
	int[4] expected = { 1, 2, 99, 4 }; // Mismatch at [2]
	int[4] desired = { 5, 6, 7, 8 };
	CInt result = compiler_rt::__atomic_compare_exchange(16, &val, &expected, &desired, 0, SEQ_CST, SEQ_CST);
	assert(result == 0, "CAS should have failed");
	// expected should now contain the actual value
	test::eq(expected, (int[4]){ 1, 2, 3, 4 });
}
