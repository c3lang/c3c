alias MyChar = char;

fn void main()
{
	MyChar my = '1';
	my.ok(); // #error: There is no member or method 'ok' on 'MyChar' (char)
}