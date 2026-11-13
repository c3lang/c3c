module ct_foreach_sparse_slice;

const int[] SPARSE = { [2] = 7 };
const int[] SPARSE_MID = { [0] = 1, [3] = 4 };
const int[] FULL = { 1, 2, 3 };

fn void ct_foreach_over_sparse_slice() @test
{
	int[3] got;
	$foreach $i, $v : SPARSE:
		got[$i] = $v;
	$endforeach
	assert(got == { 0, 0, 7 });
}

fn void ct_foreach_over_sparse_slice_with_gap() @test
{
	int[4] got;
	int count;
	$foreach $i, $v : SPARSE_MID:
		got[$i] = $v;
		count++;
	$endforeach
	assert(count == 4);
	assert(got == { 1, 0, 0, 4 });
}

fn void ct_foreach_over_full_slice() @test
{
	int sum;
	$foreach $v : FULL:
		sum += $v;
	$endforeach
	assert(sum == 6);
}
