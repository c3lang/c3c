module test_ct_subscript_aggregate;

struct Point
{
    int x;
    int y;
}

union TestUnion
{
    int a;
    short b;
}

struct Nested
{
    int[4] arr;
    bool flag;
}

fn void test_nested_array() @test
{
    int[2][2] $a = { { 10, 20 }, { 30, 40 } };
    var $b = $a[1];
    $b[0] = 99;
    $assert($b[0] == 99);
    $assert($b[1] == 40);
    $assert($a[1][0] == 30);
}

fn void test_struct_array() @test
{
    Point[2] $pts = { { 1, 2 }, { 3, 4 } };
    var $p = $pts[1];
    $assert($p.x == 3);
    $assert($p.y == 4);
}

fn void test_union_array() @test
{
    TestUnion[2] $us = { { .a = 100 }, { .a = 200 } };
    var $u = $us[1];
    $assert($u.a == 200);
}

fn void test_nested_struct_array() @test
{
    Nested[1] $nodes = { { .arr = { 1, 2, 3, 4 }, .flag = true } };
    var $sub = $nodes[0].arr;
    $sub[2] = 42;
    $assert($sub[0] == 1);
    $assert($sub[2] == 42);
    $assert($sub[3] == 4);
}

fn void test_slice_array() @test
{
    int[][2] $slices = { { 1, 2, 3 }, { 4, 5 } };
    var $s = $slices[1];
    $assert($s.len == 2);
    $assert($s[0] == 4);
    $assert($s[1] == 5);
}

fn void test_foreach_nested() @test
{
    int[2][2] $matrix = { { 1, 2 }, { 3, 4 } };
    var $sum = 0;
    $foreach $row : $matrix:
        $foreach $val : $row:
            $sum += $val;
        $endforeach
    $endforeach
    $assert($sum == 10);
}
