module compound_assign_order;

int eval_counter = 0;
int idx_order = 0;
int val_order = 0;

fn int get_idx()
{
	eval_counter++;
	idx_order = eval_counter;
	return 0;
}

fn int get_val()
{
	eval_counter++;
	val_order = eval_counter;
	return 10;
}

/*
// I am fixing this one
fn void test_compound_assign_eval_order() @test
{
	eval_counter = 0;
	idx_order = 0;
	val_order = 0;

	int[2] arr = { 1, 2 };
	arr[get_idx()] += get_val();

	assert(idx_order == 1);
	assert(val_order == 2);
	assert(arr[0] == 11);
}
*/
fn void test_simple_assign_eval_order() @test
{
	eval_counter = 0;
	idx_order = 0;
	val_order = 0;

	int[2] arr = { 1, 2 };
	arr[get_idx()] = get_val();

	// In C3/LLVM, simple assignment evaluates RHS first
	assert(val_order == 1);
	assert(idx_order == 2);
	assert(arr[0] == 10);
}

