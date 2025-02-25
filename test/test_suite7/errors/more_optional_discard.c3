module abc;

macro int mtest1(int a) @nodiscard { return 0; }
macro int! mtest2(int a) { return 0; }
fn int ftest1(int a) @nodiscard { return 0; }
fn int! ftest2(int a) { return 0; }


fn void main()
{
	mtest1(3); // #error: The called macro is marked `@nodiscard`
	mtest2(3); // #error: The macro returns 'int!', which is an optional
	ftest1(3); // #error: The called function is marked `@nodiscard`
	ftest2(3); // #error: The function returns 'int!'
}