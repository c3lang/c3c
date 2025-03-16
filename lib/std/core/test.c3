<*
Unit test module

This module provides a toolset of macros for running unit test checks 

Example:
```c3
module sample::m;
import std::io;

faultdef DIVISION_BY_ZERO;

fn double? divide(int a, int b)
{
	if (b == 0) return MathError.DIVISION_BY_ZERO?;
	return (double)(a) / (double)(b);
}

fn void? test_div() @test
{
	test::eq(2, divide(6, 3)!);
	test::ne(1, 2);
	test::ge(3, 3);
	test::gt(2, divide(3, 3)!);
	test::lt(2, 3);
	test::le(2, 3);
	test::eq_approx(m::divide(1, 3)!, 0.333, places: 3);
	test::@check(2 == 2, "divide: %d", divide(6, 3)!);
	test::@error(m::divide(3, 0), MathError.DIVISION_BY_ZERO);
}

```
*>
// Copyright (c) 2025 Alex Veden <i@alexveden.com>. All rights reserved.
// Use of this source code is governed by the MIT license
// a copy of which can be found in the LICENSE_STDLIB file.
module std::core::test;
import std::core::runtime @public;
import std::math, std::io, libc;

<*
 Initializes test case context.

 @param setup_fn : `initializer function for test case`
 @param teardown_fn : `cleanup function for test context (may be null)`

 @require runtime::test_context != null : "Only allowed in @test functions"
 @require setup_fn != null : "setup_fn must always be set"
*>
macro @setup(TestFn setup_fn, TestFn teardown_fn = null)
{
	runtime::test_context.setup_fn = setup_fn;
	runtime::test_context.teardown_fn = teardown_fn;
	runtime::test_context.setup_fn();
}

<*
 Checks condition and fails assertion if not true

 @param #condition : `any boolean condition, will be expanded by text`
 @param format : `printf compatible format`
 @param args : `vargs for format`
 @require runtime::test_context != null : "Only allowed in @test functions"
*>
macro @check(#condition, String format = "", args...)
{
	if (!#condition)
	{
		@stack_mem(512; Allocator allocator)
		{
			DString s;
			s.init(allocator);
			s.appendf("check `%s` failed. ", $stringify(#condition));
			s.appendf(format, ...args);
			print_panicf(s.str_view());
		};
	}
}

<*
 Check if function returns specific error

 @param #funcresult : `result of function execution`
 @param error_expected : `expected error of function execution`
 @require runtime::test_context != null : "Only allowed in @test functions"
*>
macro @error(#funcresult, fault error_expected)
{
	if (catch err = #funcresult)
	{
		if (err != error_expected)
		{
			print_panicf("`%s` expected to return error [%s], got [%s]",
				$stringify(#funcresult), error_expected, err);
		}
		return;
	}
	print_panicf("`%s` error [%s] was not returned.", $stringify(#funcresult), error_expected);
}

<*
 Check if left == right

 @param left : `left argument of any comparable type`
 @param right : `right argument of any comparable type`
 @require runtime::test_context != null : "Only allowed in @test functions"
*>
macro eq(left, right)
{
	if (!equals(left, right))
	{
		print_panicf("`%s` != `%s`", left, right);
	}
}

<*
 Check left floating point value is approximately equals to right value

 @param places : `number of decimal places to compare (default: 7)`
 @param delta : `minimal allowed difference (overrides places parameter)`
 @param equal_nan : `allows comparing nan values, if left and right both nans result is ok`

 @require places > 0, places <= 20 : "too many decimal places"
 @require delta >= 0, delta <= 1 : "delta must be a small number"
 @require runtime::test_context != null : "Only allowed in @test functions"
*>
macro void eq_approx(double left, double right, uint places = 7, double delta = 0, bool equal_nan = true)
{
	double diff = left - right;
	double eps = delta;
	if (eps == 0) eps = 1.0 / math::pow(10.0, places);

	if (!math::is_approx(left, right, eps))
	{
		if (equal_nan && math::is_nan(left) && math::is_nan(right)) return;
		print_panicf("Not almost equal: `%s` !~~ `%s` delta=%e diff: %e", left, right, eps, diff);
	}
}

<*
 Check if left != right

 @param left : `left argument of any comparable type`
 @param right : `right argument of any comparable type`
 @require runtime::test_context != null : "Only allowed in @test functions"
*>
macro void ne(left, right)
{
	if (equals(left, right))
	{
		print_panicf("`%s` == `%s`", left, right);
	}
}

<*
 Check if left > right

 @param left : `left argument of any comparable type`
 @param right : `right argument of any comparable type`
 @require runtime::test_context != null : "Only allowed in @test functions"
*>
macro gt(left, right)
{
	if (!builtin::greater(left, right))
	{
		print_panicf("`%s` <= `%s`", left, right);
	}
}

<*
 Check if left >= right

 @param left : `left argument of any comparable type`
 @param right : `right argument of any comparable type`
 @require runtime::test_context != null : "Only allowed in @test functions"
*>
macro ge(left, right)
{
	if (!builtin::greater_eq(left, right))
	{
		print_panicf("`%s` < `%s`", left, right);
	}
}

<*
 Check if left < right

 @param left : `left argument of any comparable type`
 @param right : `right argument of any comparable type`
 @require runtime::test_context != null : "Only allowed in @test functions"
*>
macro lt(left, right)
{
	if (!builtin::less(left, right))
	{
		print_panicf("`%s` >= `%s`", left, right);
	}
}

<*
 Check if left <= right

 @param left : `left argument of any comparable type`
 @param right : `right argument of any comparable type`
 @require runtime::test_context != null : "Only allowed in @test functions"
*>
macro le(left, right)
{
	if (!builtin::less_eq(left, right))
	{
		print_panicf("`%s` > `%s`", left, right);
	}
}

macro void print_panicf(format, ...) @local
{
	runtime::test_context.assert_print_backtrace = false;
	builtin::panicf(format, $$FILE, $$FUNC, $$LINE, $vasplat);
}

