module debugstuff;


<* @ensure return > 0 *>
macro check(int a)
{
	if (a > 0) return 1;
	if (a < 0) return -1; // #error: @ensure
	return 100;
}
fn void main()
{
	check(43);
}