fn void main()
{
	int! x;
	if (catch err = x)
	{
		default: // #warning: 'if-catch'
			return;
	}
	return;
}