module foob;

fn void main()
{
	{|
		if (1) return 1; // #error: Expected a return statement following this statement
	|};
}
