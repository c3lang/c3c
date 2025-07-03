struct Test
{
    struct { int a; }
}

macro test(int x)
{
	return {{}};
}
fn int main()
{
	Test a = test(3);
    return 0;
}