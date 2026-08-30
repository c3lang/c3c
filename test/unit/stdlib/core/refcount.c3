module std::core::mem::refcount_test @test;
import std;
import std::core::mem::ref;
import std::core::mem::rc;

struct RefTestPair
{
	int x;
	int y;
}

struct RefTestOveraligned @align(32)
{
	int a;
}

int ref_dealloc_calls = 0;

struct RefTestWithDealloc
{
	int x;
}
fn void RefTestWithDealloc.dealloc(&self)
{
	ref_dealloc_calls++;
}

fn void test_ref_wrap_and_release_frees_immediately()
{
	TrackingAllocator tracker;
	tracker.init(mem);

	RefTestPair* p = alloc::new(&tracker, RefTestPair, { 1, 2 });
	ref::Ref{RefTestPair} r = ref::wrap(p, &tracker);
	test::@check(tracker.allocated() > 0);

	r.release();
	test::eq(tracker.allocated(), 0);
	tracker.free();
}

fn void test_ref_new_zero_initialized()
{
	TrackingAllocator tracker;
	tracker.init(mem);

	ref::Ref{RefTestPair} r = ref::new{RefTestPair}(&tracker);
	test::eq(r.ptr.x, 0);
	test::eq(r.ptr.y, 0);
	r.release();
	test::eq(tracker.allocated(), 0);
	tracker.free();
}

fn void test_ref_new_with_init()
{
	TrackingAllocator tracker;
	tracker.init(mem);

	ref::Ref{RefTestPair} r = ref::new{RefTestPair}(&tracker, { 3, 4 });
	test::eq(r.ptr.x, 3);
	test::eq(r.ptr.y, 4);
	r.release();
	tracker.free();
}

fn void test_ref_retain_release_balance()
{
	TrackingAllocator tracker;
	tracker.init(mem);

	ref::Ref{RefTestPair} r = ref::new{RefTestPair}(&tracker, { 1, 1 });
	r.retain();
	r.release();
	test::@check(tracker.allocated() > 0, "Expected the value to still be alive after one release balances the retain");
	r.release();
	test::eq(tracker.allocated(), 0);
	tracker.free();
}

fn void test_ref_dealloc_hook_runs_before_free()
{
	int before = ref_dealloc_calls;
	RefTestWithDealloc* p = mem::new(RefTestWithDealloc);
	ref::Ref{RefTestWithDealloc} r = ref::wrap(p);
	r.release();
	test::eq(ref_dealloc_calls, before + 1);
}

fn void test_ref_overaligned_round_trip()
{
	TrackingAllocator tracker;
	tracker.init(mem);

	ref::Ref{RefTestOveraligned} r = ref::new{RefTestOveraligned}(&tracker, { 7 });
	test::eq(r.ptr.a, 7);
	test::@check(mem::ptr_is_aligned(r.ptr, RefTestOveraligned::alignment));
	r.release();
	test::eq(tracker.allocated(), 0);
	tracker.free();
}

struct RCTestNeither
{
	inline RefCounted rc;
	int x;
}

fn void test_rc_default_free_used_when_no_hooks()
{
	TrackingAllocator tracker;
	tracker.init(mem);
	mem::@scoped(&tracker)
	{
		RCTestNeither* p = mem::new(RCTestNeither);
		p.x = 42;
		rc::release(p);
		test::eq(tracker.allocated(), 0);
	};
	tracker.free();
}

int rc_dealloc_calls = 0;

struct RCTestDeallocOnly
{
	inline RefCounted rc;
	int x;
}
fn void RCTestDeallocOnly.dealloc(&self)
{
	rc_dealloc_calls++;
}

fn void test_rc_dealloc_hook_runs_then_default_free_reclaims_memory()
{
	int before = rc_dealloc_calls;
	TrackingAllocator tracker;
	tracker.init(mem);
	mem::@scoped(&tracker)
	{
		RCTestDeallocOnly* p = mem::new(RCTestDeallocOnly);
		rc::release(p);
		test::eq(tracker.allocated(), 0);
	};
	test::eq(rc_dealloc_calls, before + 1);
	tracker.free();
}

int rc_free_calls = 0;

struct RCTestFreeOnly
{
	inline RefCounted rc;
	int x;
}
fn void RCTestFreeOnly.free(&self)
{
	rc_free_calls++;
	mem::free(self);
}

fn void test_rc_custom_free_used_instead_of_default()
{
	int before = rc_free_calls;
	RCTestFreeOnly* p = mem::new(RCTestFreeOnly);
	rc::release(p);
	test::eq(rc_free_calls, before + 1);
}

int rc_both_dealloc_calls = 0;
int rc_both_free_calls = 0;

struct RCTestBoth
{
	inline RefCounted rc;
	int x;
}
fn void RCTestBoth.dealloc(&self)
{
	rc_both_dealloc_calls++;
}
fn void RCTestBoth.free(&self)
{
	rc_both_free_calls++;
	mem::free(self);
}

fn void test_rc_dealloc_and_custom_free_both_called()
{
	int dealloc_before = rc_both_dealloc_calls;
	int free_before = rc_both_free_calls;
	RCTestBoth* p = mem::new(RCTestBoth);
	rc::release(p);
	test::eq(rc_both_dealloc_calls, dealloc_before + 1);
	test::eq(rc_both_free_calls, free_before + 1);
}

fn void test_rc_retain_prevents_release_from_freeing()
{
	TrackingAllocator tracker;
	tracker.init(mem);
	mem::@scoped(&tracker)
	{
		RCTestNeither* p = mem::new(RCTestNeither);
		rc::retain(p);
		rc::release(p);
		test::@check(tracker.allocated() > 0, "Expected the value to still be alive after one release balances the retain");
		rc::release(p);
		test::eq(tracker.allocated(), 0);
	};
	tracker.free();
}

fn void test_rc_null_retain_and_release_are_noops()
{
	RCTestNeither* p = null;
	test::eq(rc::retain(p), p);
	rc::release(p);
}
