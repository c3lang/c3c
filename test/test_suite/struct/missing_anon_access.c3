struct Test
{
	struct { int x; }
}

fn void main()
{
	Test t;
	Test::members[0].get(t).x;
	Test::members[0].get(t).y; // #error: here is no field or method 'Test.(anon).y'
}