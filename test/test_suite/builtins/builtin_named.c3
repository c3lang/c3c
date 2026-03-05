module test;
fn void main()
{
	ichar x = 1;
	ichar z2;
	bool success2 = $$overflow_add(x, file: filename, &z2); // #error: Named arguments are not allowed in builtin calls.
}