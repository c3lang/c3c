fn int main()
{
	const A = x'1234' +++ (ichar[A]); // #error:  The evaluation of this expression is recursive.
}