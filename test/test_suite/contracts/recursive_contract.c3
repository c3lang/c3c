module abc_faults;

<* @require test(3) *>
fn void? test(int a) // #error: Contract resolution exceeded the maximum depth for this function; remaining contracts will be ignored
{
}
