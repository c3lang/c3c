module defined_swizzle_assign;

fn int[<3>] rv() => { 1, 2, 3 };
const int[<3>] CV = { 1, 2, 3 };

fn void defined_swizzle_assign() @test
{
	int[<3>] v;
	$assert $defined(v.xy = { 1, 2 });
	$assert $defined(v.zx = { 1, 2 });
	$assert $defined(v.x = 1);
	$assert $defined(v.xy += { 1, 2 });
	$assert $defined(v.xy++);
	$assert !$defined(rv().xy = { 1, 2 });
	$assert !$defined(CV.xy = { 1, 2 });
	v.zx = { 5, 6 };
	assert(v == { 6, 0, 5 });
}
