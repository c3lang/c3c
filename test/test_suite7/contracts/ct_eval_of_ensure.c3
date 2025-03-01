module test;
<*
 @ensure return > 100
 *>
fn int test(int baz)
{
	return 1; // #error: @ensure "return > 100" violated.
}

extern fn void printf(char*, ...);
fn void main(String[] args)
{
   test(1022);
   printf("Hello\n");
}

