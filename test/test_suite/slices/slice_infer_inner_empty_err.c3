fn int main()
{
	int [*][] y = { }; // #error: Inferring the slice inner type from an empty initializer is not possible
	return 0;
}