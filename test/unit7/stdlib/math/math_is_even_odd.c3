module math_is_even_odd_tests;
import std::math;
import std::io;

macro test(start, ...)
{
    $for (var $i = 0; $i < $vacount; $i++)
        for ($vatype[$i] i = ($vatype[$i])start; i < 5; i+=2)
        {
	    assert(math::is_even(i));
	    assert(i.is_even());

	    assert(!math::is_odd(i));
	    assert(!i.is_odd());
        }
    $endfor;
}

fn void tests() @test
{
    test(0, char, ushort, uint, ulong, uptr, usz, uint128);
    test(-4, ichar, short, int, long, iptr, isz, int128);
}
