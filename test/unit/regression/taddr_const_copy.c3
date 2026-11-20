module taddr_const_copy;

const int[3] CA = { 1, 2, 3 };
struct Pt { int x; }
const Pt CP = { 7 };

macro int[] @take() => &&CA;

fn void taddr_of_constant_is_a_copy() @test
{
	int[] sl = &&CA;
	sl[1] = 30;
	int[3]* ap = &&CA;
	(*ap)[0] = 10;
	Pt* p = &&CP;
	p.x = 70;
	int[] m = @take();
	m[2] = 90;
	assert(sl[1] == 30 && (*ap)[0] == 10 && p.x == 70 && m[2] == 90);
	assert(CA[0] == 1 && CA[1] == 2 && CA[2] == 3 && CP.x == 7);
}
