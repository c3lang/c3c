module std::sort;

fn int cmp_int_ref(int* x, int* y)
{
	return *x - *y;
}

fn int cmp_int_value(int x, int y)
{
	return x - y;
}


fn uint key_int_ref(int* x)
{
	return (uint)(*x + int.min);
}

fn uint key_int_value(int x)
{
	return (uint)(x + int.min);
}