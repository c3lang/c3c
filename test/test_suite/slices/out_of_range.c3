module ccc;

fn int main()
{
	int[3] abc = {1, 2, 3};
	abc[1:3]; // #error: End index out of bounds, was 3, exceeding max index 2.
	return 0;
}
