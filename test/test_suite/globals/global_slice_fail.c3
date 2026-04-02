module somemain;
int* ref_to_2nd = &list_of_ints[0]; // #error:  The expression must be a constant value.
int[] list_of_ints = { 1, 2, };
fn int main()
{
	int* y = ref_to_2nd;
    return 0;
}