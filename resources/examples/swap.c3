module test;
import libc;

/**
 * @checked *a = *b, *b = *a
 */
macro void @swap(&a, &b)
{
    var temp = *a;
    *a = *b;
    *b = temp;
}

fn void main()
{
	int x = 123;
	int y = 456;
	@swap(x, y);
	libc::printf("x: %d y: %d\n", x, y);
}