enum Col { RED, BLUE, GREEN }
struct Foo { int a; Col c; }

fn void assign()
{
	Col c;
	c.ordinal = 1; // #error: The ordinal is a value
}

fn void op_assign()
{
	Col c;
	c.ordinal += 1; // #error: The ordinal is a value
}

fn void incdec()
{
	Col c;
	c.ordinal++; // #error: The ordinal is a value
}

fn void addr()
{
	Col c;
	int* p = &c.ordinal; // #error: The ordinal is a value
}

fn void through_pointer(Col* cp)
{
	cp.ordinal = 2; // #error: The ordinal is a value
}

fn void member_addr()
{
	Foo f;
	int* p = &f.c.ordinal; // #error: The ordinal is a value
}

fn void valid(Col* cp)
{
	Col c = BLUE;
	Foo f;
	int x = c.ordinal + cp.ordinal + f.c.ordinal;
	int* t = &&c.ordinal;
	Col* p = &f.c;
	$assert !$defined(c.ordinal = 1);
	$assert !$defined(c.ordinal++);
	$assert !$defined(&c.ordinal);
	$assert !$defined(&f.nope);
	$assert $defined(&f.c);
}
