fn void main()
{
	int x = 5;

	if (x = 1) {}  // #error: An assignment in a conditional must have an extra parenthesis

	if ((x = 1)) {}

	while (x = 1) {} // #error: An assignment in a conditional must have an extra parenthesis

	while (x = 1, x = 1) {} // #error: An assignment in the last conditional must be parenthesized
}