module test;

// A partially designated vector literal must lower to a flat LLVM vector,
// otherwise selecting between two of them produced invalid IR.
fn float[<3>] pick(bool first)
{
	return first ? (float[<3>]){ .x = 1.0f } : (float[<3>]){ .y = 1.0f };
}

fn int main()
{
	float[<3>] a = pick(true);
	float[<3>] b = pick(false);
	assert(a[0] == 1.0f && a[1] == 0.0f && a[2] == 0.0f);
	assert(b[0] == 0.0f && b[1] == 1.0f && b[2] == 0.0f);
	return 0;
}
