static initialize
{
	return; // This is fine
}

static initialize
{
	return 123; // #error: You cannot cast 'int' into 'void' even with an explicit cast, so this looks like an error.
}