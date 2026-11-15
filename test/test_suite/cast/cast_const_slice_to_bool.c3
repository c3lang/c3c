module test;

struct Pair
{
	int a;
	int b;
}

// A constant slice is true when it is non-empty, whatever its contents are.
// Each check is in its own function so that one failure doesn't hide the others.
fn void zero_initialized_array()
{
	$assert((bool)(int[])(int[3]){});
}

fn void zero_initialized_array_len_1()
{
	$assert((bool)(int[])(int[1]){});
}

fn void zero_initialized_struct_array()
{
	$assert((bool)(Pair[])(Pair[2]){});
}

// These already work, and must keep working.
fn void explicit_zeros()
{
	$assert((bool)(int[])(int[3]){ 0, 0, 0 });
}

fn void non_zero_values()
{
	$assert((bool)(int[])(int[3]){ 1, 2, 3 });
}

fn void empty_slice()
{
	$assert(!(bool)(int[]){});
}

fn void empty_string()
{
	$assert(!(bool)"");
}

fn void non_empty_string()
{
	$assert((bool)"a");
}
