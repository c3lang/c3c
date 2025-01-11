<*
Unit test module

This module provides a toolset of macros for running unit test checks 

Example:
```c3
module sample::m;
import std::io;

fault MathError
{
    DIVISION_BY_ZERO
}

fn double! divide(int a, int b)
{
    if (b == 0) return MathError.DIVISION_BY_ZERO?;
    return (double)(a) / (double)(b);
}

fn void! test_div() @test {
    test::equal(2, divide(6, 3)!);
    test::not_equal(1, 2);
    test::almost_equal(m::divide(1, 3)!, 0.333, places: 3);
    test::greater_equal(3, 3);
    test::greater(2, divide(3, 3)!);
    test::less(2, 3);
    test::less_equal(2, 3);
    test::@check(2 == 2, "divide: %d", divide(6, 3)!);
    test::@error(m::divide(3, 0), MathError.DIVISION_BY_ZERO);
}

```
*>
// Copyright (c) 2025 Alex Veden <i@alexveden.com>. All rights reserved.
// Use of this source code is governed by the MIT license
// a copy of which can be found in the LICENSE_STDLIB file.
module std::core::test;
import std::math;
import std::core::builtin;
import std::io;
import libc;

def TestFn = fn void!() @if($$OLD_TEST);
def TestFn = fn void() @if(!$$OLD_TEST);


struct TestContext @private
{
	JmpBuf buf;
	// Allows filtering test cased or modules by substring, e.g. 'foo::', 'foo::test_add'  
	String test_filter;
	// Triggers debugger breakpoint when assert or test:: checks failed
	bool breakpoint_on_assert;

	// internal state
	bool assert_print_backtrace;
	bool has_ansi_codes;
	bool is_in_panic;
	String current_test_name;
	TestFn setup_fn;
	TestFn teardown_fn;

			
	char* error_buffer;
	usz error_buffer_capacity;
	File fake_stdout;
	File orig_stdout;
	File orig_stderr;
}

TestContext* test_context @private;

<*
Initializes test case context.

@param setup_fn `initializer function for test case`
@param teardown_fn `cleanup function for test context (may be null)`

@require setup_fn != null, "setup_fn must always be set"
@require test_context != null, "allowed only in @test functions"
*>
macro setup(TestFn setup_fn, TestFn teardown_fn = null) {
	test_context.setup_fn = setup_fn;
	test_context.teardown_fn = teardown_fn;
	test_context.setup_fn();
}

<*
Checks condition and fails assertion if not true 

@param #condition `any boolean condition, will be expanded by text`
@param format `printf compatible format`
@param args `vargs for format`
@require test_context != null, "allowed only in @test functions"
*>
macro @check(#condition, format = "", args...) { 
    if(!#condition) {
		@stack_mem(512; Allocator allocator)
		{
			DString s;
			s.new_init(allocator: allocator);
			s.appendf("check `%s` failed. ", $stringify(#condition));
			s.appendf(format, ...args);
    		test_context.assert_print_backtrace = false;
	    	builtin::panicf(s.str_view(), $$FILE, $$FUNC, $$LINE);
		};
    }
}

<*
Check if function returns specific error

@param #funcresult `result of function execution`
@param error_expected `expected error of function execution`
@require test_context != null, "allowed only in @test functions"
*>
macro @error(#funcresult, error_expected) {
    if(catch err = #funcresult) {
    	if (err != error_expected){
    		test_context.assert_print_backtrace = false;
	    	builtin::panicf("`%s` expected to return error [%s], got [%s]",
	    					$$FILE, $$FUNC, $$LINE, 
	    					$stringify(#funcresult), error_expected, err);
    	}
    } else {
    	test_context.assert_print_backtrace = false;
	    builtin::panicf("`%s` error [%s] was not returned.", 
	    				$$FILE, $$FUNC, $$LINE,
	    				$stringify(#funcresult), error_expected);
    }
}

<*
Check if left == right

@param left `left argument of any comparable type`
@param left `right argument of any comparable type`
@require test_context != null, "allowed only in @test functions"
*>
macro equal(left, right) { 
    if(!builtin::equals(left, right)) {
    	test_context.assert_print_backtrace = false;
	    builtin::panicf("`%s` != `%s`", $$FILE, $$FUNC, $$LINE, left, right);
    }
}

<*
Check left floating point value is approximately equals to right value

@param left `left argument of any comparable type`
@param left `right argument of any comparable type`
@param places `number of decimal places to compare (default: 7)`
@param delta `minimal allowed difference (overrides places parameter)`
@param equal_nan `allows comparing nan values, if left and right both nans result is ok`

@require places > 0, places <= 20, "too many decimal places"
@require delta >= 0, delta <= 1, "delta must be a small number"
@require test_context != null, "allowed only in @test functions"
*>
macro almost_equal(left, right, uint places=7, double delta=0, bool equal_nan = true) { 
	double diff = (double)left - (double)right;
	double eps = delta;
	if (eps == 0){
		eps = 1.0 / math::pow(10.0, places);
	} 

    if(!math::is_approx(left, right, eps)){
    	if(equal_nan && math::is_nan(left) && math::is_nan(right)) {
    		return;
    	}
    	test_context.assert_print_backtrace = false;
	    builtin::panicf("Not almost equal: `%s` !~~ `%s` delta=%e diff: %e", $$FILE, $$FUNC, $$LINE, left, right, eps, diff);
    }
}

<*
Check if left != right

@param left `left argument of any comparable type`
@param left `right argument of any comparable type`
@require test_context != null, "allowed only in @test functions"
*>
macro not_equal(left, right) { 
    if(builtin::equals(left, right)) {
    	test_context.assert_print_backtrace = false;
	    builtin::panicf("`%s` == `%s`", $$FILE, $$FUNC, $$LINE, left, right);
    }
}

<*
Check if left > right

@param left `left argument of any comparable type`
@param left `right argument of any comparable type`
@require test_context != null, "allowed only in @test functions"
*>
macro greater(left, right) { 
    if(!builtin::greater(left, right)) {
    	test_context.assert_print_backtrace = false;
	    builtin::panicf("`%s` <= `%s`", $$FILE, $$FUNC, $$LINE, left, right);
    }
}

<*
Check if left >= right

@param left `left argument of any comparable type`
@param left `right argument of any comparable type`
@require test_context != null, "allowed only in @test functions"
*>
macro greater_equal(left, right) { 
    if(!builtin::greater_eq(left, right)) {
    	test_context.assert_print_backtrace = false;
	    builtin::panicf("`%s` < `%s`", $$FILE, $$FUNC, $$LINE, left, right);
    }
}

<*
Check if left < right

@param left `left argument of any comparable type`
@param left `right argument of any comparable type`
@require test_context != null, "allowed only in @test functions"
*>
macro less(left, right) { 
    if(!builtin::less(left, right)) {
    	test_context.assert_print_backtrace = false;
	    builtin::panicf("`%s` >= `%s`", $$FILE, $$FUNC, $$LINE, left, right);
    }
}

<*
Check if left <= right

@param left `left argument of any comparable type`
@param left `right argument of any comparable type`
@require test_context != null, "allowed only in @test functions"
*>
macro less_equal(left, right) { 
    if(!builtin::less_eq(left, right)) {
    	test_context.assert_print_backtrace = false;
	    builtin::panicf("`%s` > `%s`", $$FILE, $$FUNC, $$LINE, left, right);
    }
}

