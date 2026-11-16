module optional_address_of;

faultdef BAD;

struct OaHolder { int x; int[2] a; }

fn void p(int a, args...) { unreachable(); }

fn void? pass_to_any()
{
	int? z = BAD~;
	p(1, z)!;
}

fn void address_of_faulted_optional() @test
{
	int? z = BAD~;
	int*? zp = &z;
	assert(@catch(zp) == BAD);
	OaHolder? h = BAD~;
	int*? xp = &h.x;
	assert(@catch(xp) == BAD);
	int*? ap = &h.a[1];
	assert(@catch(ap) == BAD);
	any? a = &z;
	assert(@catch(a) == BAD);
	assert(@catch(pass_to_any()) == BAD);
}
