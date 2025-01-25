int[?] foo = {1,2,3};
fn int* bar(int index)
{
	int* array = &foo;
	return &array[index];
}

fn int* baz(int index)
{
	int[] array = &foo;
	return &array[index];
}

fn int* abc(int index)
{
	int[3] array;
	return &array[1]; // #error: invalid once the function returns
}

fn int* abc2(int index)
{
	int val;
	return &val; // #error: invalid once the function returns
}