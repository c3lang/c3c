module string_to_int;
import std::io, std::math;

const N = 2560;
macro String[] m() @const
{
    String[N] $strings = {};
    $foreach $i : math::iota(int[N]):
        $strings[$i] = @sprintf("%s", $i);
    $endforeach
    return $strings[..];
}

macro String[] m2() @const
{
    String[N] $strings = {};
    $foreach $i : math::iota(int[N]):
        $strings[$i] = @sprintf("%s", -$i);
    $endforeach
    return $strings;
}

fn void string_to_char() @test
{
	int valid = 0;
    foreach (str : m())
    {
        char? x = string::to_integer{char}(str);
        if (try x) valid++;
    }
    assert(valid == 256);
}
fn void string_to_ichar() @test
{
	int valid = 0;
    foreach (str : m2())
    {
        char? x = string::to_integer{ichar}(str);
        if (try x) valid++;
    }
    assert(valid == 129);
}