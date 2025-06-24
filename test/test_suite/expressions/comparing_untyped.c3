fn int main()
{
	assert({0, 1, 2} == {0, 1, 2}, "Slices must be equivalent"); // #error: Both sides are untyped and cannot be compared
	return 0;
}