module test;

struct Struct
{
	int field;
}

fn int main()
{
	Struct[4] arr;

	arr[1] = {.field = 1};
	arr[1..2] = {1};
	arr[1..2] = {.field = 1};
	return 0;
}
