module maybetest @test;

import std::collections::maybe;
import std::core::types;

alias maybeint = maybe::value{int};

struct Unequatable { int value; }

alias MaybeUnequatable = maybe::Maybe{Unequatable};

fn void test_equals() @test
{
	$assert(!types::is_equatable_type(MaybeUnequatable));
	assert(maybeint(1) == maybeint(1));
	assert(maybeint(0) == maybeint(0));
	assert(maybeint(0) != maybeint(1));
	assert(maybeint(1) != maybeint(0));
	assert(maybeint(0) != maybe::EMPTY{int});
	assert(maybe::EMPTY{int} != maybeint(0));
}
