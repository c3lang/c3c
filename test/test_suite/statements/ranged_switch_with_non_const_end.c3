fn void main()
{
	for (int i = 0; ; )
	{
		switch (i)
		{
			case 4 .. i: // #error: Ranges must be constant integers or enum values, but this is not an integer
		}
	}
}