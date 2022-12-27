static initialize
{
	return; // This is fine
}

static initialize
{
	return 123; // #error: It is not possible to convert 'int' to 'void'
}