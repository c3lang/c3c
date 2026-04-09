fn int main()
{
	const A = x'1234' +++ (ichar[A]); // #error: Recursive definition of 'A'
}