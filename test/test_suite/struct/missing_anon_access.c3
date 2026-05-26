struct Test
{
	struct { int x; }
}

fn void main()
{
	Test t;
	var $member = Test::members[0];
	t.$member.x;
	t.$member.y; // #error: here is no field or method 'Test.(anon).y'
}