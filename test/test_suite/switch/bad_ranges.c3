module test;

fn void? test1()
{
	double a;
	switch (a)
	{
		case 1.3 .. 4.5: // #error: Ranges must be constant integers
		default:
    }
}

fn void? test2()
{
	int a;
	switch (a)
	{
		case 2 .. 3:
		case a .. 3: // #error: Ranges must be constant integers
		default:
    }
}