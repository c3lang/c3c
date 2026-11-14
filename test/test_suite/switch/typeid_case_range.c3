fn void test()
{
	typeid t = float;
	switch (t)
	{
		case int..double: // #error: Case ranges are not allowed when switching on a typeid
			break;
		default:
			break;
	}
}
