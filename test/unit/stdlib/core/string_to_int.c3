module string_to_int;
import std::io, std::math;

const N = 2560;
macro String[] m() @const
{
    var $strings = {};
    $foreach $i : math::iota(int[N]):
        $strings +++= @sprintf("%s", $i);
    $endforeach
    return $strings;
}

macro String[] m2() @const
{
    var $strings = {};
    $foreach $i : math::iota(int[N]):
        $strings +++= @sprintf("%s", -$i);
    $endforeach
    return $strings;
}

fn void string_to_char() @test
{
	int valid = 0;
    foreach (str : m())
    {
        char? x = str.to_integer(char);
        if (try x) valid++;
    }
    assert(valid == 256);
}
fn void string_to_ichar() @test
{
	int valid = 0;
    foreach (str : m2())
    {
        ichar? x = str.to_integer(ichar);
        if (try x) valid++;
    }
    assert(valid == 129);
}