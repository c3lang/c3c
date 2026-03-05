fn int main()
{
	int[<6>] c = 2;
	c = $$matrix_transpose(c, 0x10, 3); // #error:  Expected row * col to equal 6
	return 0;
}