fn void main()
{
	long x;

	int i = (char)x; // Ok
	char c;
	short s1 = (long)c; // Ok
	short s2 = (long)i; // #error: 'int' cannot implicitly be converted
}