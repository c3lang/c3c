module test_repro;

macro void trigger_crash(int x)
{
	int[4] $masks = {};
	$masks[1] <<= x; // #error: The argument must be a constant value
}

fn void main()
{
	trigger_crash(42);
}
