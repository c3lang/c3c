module abc_faults;
faultdef ABC;
<*
 @require a != ABC~! *> // #error: Rethrows using '!' is not allowed in contracts
fn void? test(int a)
{ // #warning: This code will never execute.
}

<* @ensure return != ABC~! *> // #error: Rethrows using '!' is not allowed in contracts
fn int? hello()
{
	return 0;
}

fn void main()
{
	test(3);
	hello();
}