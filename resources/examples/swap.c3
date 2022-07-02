module test;
import libc;

/**
 * @checked a = b, b = a
 */
macro void @swap(&a, &b)
{
    $typeof(a) temp = a;
    a = b;
    b = temp;
}

fn void main()
{
	int x = 123;
	int y = 456;
	@swap(x, y);
	libc::printf("x: %d y: %d\n", x, y);
}