// Regression tests for optional scalar type fault propagation
// Bug: Functions returning scalar optionals (char?, int?, bool?) don't propagate faults
// The fault is ignored and default/zero values are used instead
// EXTENDED: Also tests pointer optionals (Foo*?) for fault propagation
module optional_scalar_test;
import std::io;

faultdef TEST_ERROR;

// ===== Test struct for pointer optionals =====

struct TestStruct
{
	int value;
}

// ===== Helper functions (NOT test functions) =====

fn char? failing_char()
{
	return TEST_ERROR?;
}

fn int? failing_int()
{
	return TEST_ERROR?;
}

fn bool? failing_bool()
{
	return TEST_ERROR?;
}

fn char? success_char()
{
	return 'A';
}

fn int? success_int()
{
	return 42;
}

fn bool? success_bool()
{
	return true;
}

fn TestStruct*? failing_ptr()
{
	return TEST_ERROR?;
}

fn TestStruct*? success_ptr()
{
	TestStruct* ptr = mem::new(TestStruct);
	ptr.value = 123;
	return ptr;
}

// ===== Try Unwrap Tests =====

fn void test_try_unwrap_char_propagates_fault() @test
{
	char result = 0;

	// This function should return the fault, not continue
	if (try c = failing_char())
	{
		// Should NOT reach here
		assert(false, "BUG: try unwrap should have propagated fault");
	}
	else
	{
		// Should reach here - try failed, so we're in else block
		result = 'X';
	}

	assert(result == 'X', "Expected to be in else block");
}

fn void test_try_unwrap_int_propagates_fault() @test
{
	int result = 0;

	if (try i = failing_int())
	{
		assert(false, "BUG: try unwrap should have propagated fault");
	}
	else
	{
		result = 999;
	}

	assert(result == 999, "Expected to be in else block");
}

fn void test_try_unwrap_success_char() @test
{
	char result = 0;

	if (try c = success_char())
	{
		result = c;
	}
	else
	{
		assert(false, "Should have succeeded");
	}

	assert(result == 'A', "Expected success value");
}

// ===== Rethrow Tests =====

fn char? helper_rethrow_char()
{
	char c = failing_char()!;
	// Should NOT reach here
	unreachable("BUG: rethrow should have propagated fault");
}

fn void test_rethrow_char_propagates_fault() @test
{
	bool caught = false;

	if (catch err = helper_rethrow_char())
	{
		// When there IS a fault, catch enters the if block
		caught = true;
		assert(err == TEST_ERROR, "Expected TEST_ERROR fault");
	}
	else
	{
		// When there is NO fault, catch enters the else block
		assert(false, "Should have caught fault");
	}

	assert(caught, "Expected to catch fault");
}

fn int? helper_rethrow_int()
{
	int i = failing_int()!;
	unreachable("BUG: rethrow should have propagated fault");
}

fn void test_rethrow_int_propagates_fault() @test
{
	bool caught = false;

	if (catch err = helper_rethrow_int())
	{
		// When there IS a fault, catch enters the if block
		caught = true;
	}
	else
	{
		// When there is NO fault, catch enters the else block
		assert(false, "Should have caught fault");
	}

	assert(caught, "Expected to catch fault");
}

fn char? helper_rethrow_with_defer(bool* defer_ran)
{
	*defer_ran = false;

	defer *defer_ran = true;

	char c = failing_char()!;

	// Should not reach here
	unreachable("BUG: rethrow should have propagated fault");
}

fn void test_rethrow_executes_defers() @test
{
	bool defer_ran = false;

	if (catch err = helper_rethrow_with_defer(&defer_ran))
	{
		// When there IS a fault, catch enters the if block
		// Defer should have executed
		assert(defer_ran, "Defer should have executed");
	}
	else
	{
		// When there is NO fault, catch enters the else block
		assert(false, "Should have caught fault");
	}
}

// ===== Catch Tests =====

fn void test_catch_char_captures_fault() @test
{
	char result = 0;
	bool fault_caught = false;

	if (catch err = failing_char())
	{
		// When there IS a fault, catch enters the if block
		assert(err == TEST_ERROR, "Expected to capture TEST_ERROR");
		result = 'X';
		fault_caught = true;
	}
	else
	{
		// When there is NO fault, catch enters the else block
		assert(false, "Should have caught fault");
	}

	assert(fault_caught, "Expected to catch fault");
	assert(result == 'X', "Expected to be in error branch");
}

fn void test_catch_int_captures_fault() @test
{
	int result = 0;
	bool fault_caught = false;

	if (catch err = failing_int())
	{
		// When there IS a fault, catch enters the if block
		assert(err == TEST_ERROR, "Expected to capture TEST_ERROR");
		result = 999;
		fault_caught = true;
	}
	else
	{
		// When there is NO fault, catch enters the else block
		assert(false, "Should have caught fault");
	}

	assert(fault_caught, "Expected to catch fault");
	assert(result == 999, "Expected to be in error branch");
}

fn void test_catch_success_char() @test
{
	char result = 0;

	if (try c = success_char())
	{
		result = c;
	}
	else
	{
		assert(false, "Should have succeeded");
	}

	assert(result == 'A', "Expected success value");
}

// ===== Nested Optional Tests =====

fn int? inner_optional()
{
	return failing_int();
}

fn int? outer_optional()
{
	// Should propagate fault from inner
	int val = inner_optional()!;
	return val;
}

fn void test_nested_optional_propagation() @test
{
	bool caught = false;

	if (catch err = outer_optional())
	{
		// When there IS a fault, catch enters the if block
		caught = true;
		assert(err == TEST_ERROR, "Expected TEST_ERROR");
	}
	else
	{
		// When there is NO fault, catch enters the else block
		assert(false, "Should have caught fault");
	}

	assert(caught, "Expected to catch fault from nested call");
}

// ===== Mixed Success/Failure Tests =====

fn char? sometimes_fails_helper(bool should_fail)
{
	if (should_fail)
	{
		return TEST_ERROR?;
	}
	return 'Z';
}

fn void test_conditional_fault() @test
{
	// Test failure case
	if (catch err = sometimes_fails_helper(true))
	{
		// When there IS a fault, catch enters the if block
		assert(err == TEST_ERROR, "Expected TEST_ERROR");
	}
	else
	{
		// When there is NO fault, catch enters the else block
		assert(false, "Should fail when should_fail=true");
	}

	// Test success case
	if (try c = sometimes_fails_helper(false))
	{
		assert(c == 'Z', "Expected success value");
	}
	else
	{
		assert(false, "Should succeed when should_fail=false");
	}
}

// ===== Pointer Optional Tests =====

fn void test_try_unwrap_ptr_propagates_fault() @test
{
	TestStruct* result = null;

	if (try ptr = failing_ptr())
	{
		assert(false, "BUG: try unwrap should have propagated fault");
	}
	else
	{
		result = (TestStruct*)0xDEADBEEF;
	}

	assert(result == (TestStruct*)0xDEADBEEF, "Expected to be in else block");
}

fn void test_try_unwrap_success_ptr() @test
{
	TestStruct* result = null;
	int saved_value = 0;

	if (try ptr = success_ptr())
	{
		result = ptr;
		saved_value = ptr.value;
		free(ptr);
	}
	else
	{
		assert(false, "Should have succeeded");
	}

	assert(result != null, "Expected success value");
	assert(saved_value == 123, "Expected correct value");
}

fn TestStruct*? helper_rethrow_ptr()
{
	TestStruct* ptr = failing_ptr()!;
	unreachable("BUG: rethrow should have propagated fault");
}

fn void test_rethrow_ptr_propagates_fault() @test
{
	bool caught = false;

	if (catch err = helper_rethrow_ptr())
	{
		// When there IS a fault, catch enters the if block
		caught = true;
		assert(err == TEST_ERROR, "Expected TEST_ERROR fault");
	}
	else
	{
		// When there is NO fault, catch enters the else block
		assert(false, "Should have caught fault");
	}

	assert(caught, "Expected to catch fault");
}

fn void test_catch_ptr_captures_fault() @test
{
	TestStruct* result = null;
	bool fault_caught = false;

	if (catch err = failing_ptr())
	{
		// When there IS a fault, catch enters the if block
		assert(err == TEST_ERROR, "Expected to capture TEST_ERROR");
		result = (TestStruct*)0xBADF00D;
		fault_caught = true;
	}
	else
	{
		// When there is NO fault, catch enters the else block
		assert(false, "Should have caught fault");
	}

	assert(fault_caught, "Expected to catch fault");
	assert(result == (TestStruct*)0xBADF00D, "Expected to be in error branch");
}

fn void test_catch_success_ptr() @test
{
	TestStruct* result = null;
	int saved_value = 0;

	if (try ptr = success_ptr())
	{
		result = ptr;
		saved_value = ptr.value;
		free(ptr);
	}
	else
	{
		assert(false, "Should have succeeded");
	}

	assert(result != null, "Expected success value");
	assert(saved_value == 123, "Expected correct value");
}

fn TestStruct*? inner_ptr_optional()
{
	return failing_ptr();
}

fn TestStruct*? outer_ptr_optional()
{
	TestStruct* ptr = inner_ptr_optional()!;
	return ptr;
}

fn void test_nested_ptr_optional_propagation() @test
{
	bool caught = false;

	if (catch err = outer_ptr_optional())
	{
		// When there IS a fault, catch enters the if block
		caught = true;
		assert(err == TEST_ERROR, "Expected TEST_ERROR");
	}
	else
	{
		// When there is NO fault, catch enters the else block
		assert(false, "Should have caught fault");
	}

	assert(caught, "Expected to catch fault from nested call");
}
