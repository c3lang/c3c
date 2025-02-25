fn int main()
{
	int a;
	switch (a)
	{
		case 1: // #error: Fallthrough cases
			// ...
		case 2:
	}
    return 0;
}