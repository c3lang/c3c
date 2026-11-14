module narrow_div_assign;

struct Foo { ichar a; short b; }

fn ichar ichar_div(ichar a, ichar b) @noinline { a /= b; return a; }
fn ichar ichar_rem(ichar a, ichar b) @noinline { a %= b; return a; }
fn short short_div(short a, short b) @noinline { a /= b; return a; }
fn short short_rem(short a, short b) @noinline { a %= b; return a; }

fn void narrow_min_div_minus_one() @test
{
	ichar e = -128;
	e /= -1;
	test::eq(e, -128);
	e = -128;
	e %= -1;
	test::eq(e, 0);
	short s = short::min;
	s /= -1;
	test::eq(s, short::min);
	s = short::min;
	s %= -1;
	test::eq(s, 0);
	test::eq(ichar_div(-128, -1), -128);
	test::eq(ichar_rem(-128, -1), 0);
	test::eq(short_div(short::min, -1), short::min);
	test::eq(short_rem(short::min, -1), 0);
}

fn void narrow_div_assign_matches_plain() @test
{
	ichar[*] vals = { -128, -127, -7, -1, 1, 3, 7, 127 };
	foreach (a : vals)
	{
		foreach (b : vals)
		{
			ichar d = a;
			d /= b;
			test::eq(d, (ichar)(a / b));
			ichar m = a;
			m %= b;
			test::eq(m, (ichar)(a % b));
			short sa = a * 256;
			short sd = sa;
			sd /= b;
			test::eq(sd, (short)(sa / b));
			short sm = sa;
			sm %= b;
			test::eq(sm, (short)(sa % b));
		}
	}
}

fn void narrow_unsigned_and_wide_unchanged() @test
{
	char c = 200;
	c /= 7;
	test::eq(c, 28);
	c = 200;
	c %= 7;
	test::eq(c, 4);
	ushort us = 60000;
	us /= 7;
	test::eq(us, 8571);
	int i = -7;
	i /= 2;
	test::eq(i, -3);
	i = -7;
	i %= 2;
	test::eq(i, -1);
	long l = -9;
	l %= 4;
	test::eq(l, -1);
	ichar[<2>] v = { -100, 9 };
	v /= { 3, -2 };
	test::eq(v[0], -33);
	test::eq(v[1], -4);
}

fn void narrow_div_through_pointer_and_field() @test
{
	Foo f = { -128, short::min };
	ichar* p = &f.a;
	*p /= -1;
	f.b %= -1;
	test::eq(f.a, -128);
	test::eq(f.b, 0);
	ichar[3] arr = { -128, 9, -9 };
	foreach (&x : arr) *x /= -1;
	test::eq(arr[0], -128);
	test::eq(arr[1], -9);
	test::eq(arr[2], 9);
}
