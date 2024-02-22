module helloworld;

fn void main()
{
	{| return 1; |}; // #error: which must be handled
}