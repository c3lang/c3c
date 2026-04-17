struct Test
{
	int a;
}

fn Test test(int a)
{
	switch (a)
	{
		case 0x9: return {3};
		case 0xd: return {4};
		case 0x01..0x1a: return {5}; // #error:  The same case value appears more than once
		case 0x02: return {5};
		default: return {};
	}
}

fn Test test2(int a)
{
	switch (a)
	{
		case 0x01..0x1a: return {5};
		case 0x02: return {5}; // #error:  The same case value appears more than once
		default: return {};
	}
}

fn int main()
{
	Test t = test(3);
	return 0;
}
