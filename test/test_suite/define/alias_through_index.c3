alias MyChar = char;

fn void main()
{
	MyChar[<2>] my = { '1', '2' };
	my[0].ok(); // #error: There is no member or method 'ok' on 'MyChar' (char)
}