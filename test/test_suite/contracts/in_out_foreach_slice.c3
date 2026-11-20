<* @param [out] s *>
fn int f(int[] s)
{
	int c;
	foreach (x : s) c += x; // #error: 'out' parameters may not be read
	return c;
}

<* @param [inout] s *>
fn int g(int[] s)
{
	int c;
	foreach (x : s) c += x;
	return c;
}

<* @param [out] s *>
fn void h(int[] s)
{
	foreach (&p : s) *p = 1;
}

struct Big { int[64] v; }

<* @param [out] p *>
fn void set(int* p) { *p = 1; }

<* @param [in] s *>
fn void w1(int[] s)
{
	foreach (&p : s) *p = 1; // #error: 'in' parameters may not be assigned to
}

<* @param [in] s *>
fn void w2(int[] s)
{
	foreach (&p : s) (*p)++; // #error: 'in' parameters may not be assigned to
}

<* @param [in] s *>
fn void w3(int[] s)
{
	foreach (&p : s) set(p); // #error: An 'in' parameter may not be passed
}

<* @param [in] s *>
fn int r1(Big[] s)
{
	int c;
	foreach (&p : s) c += p.v[0];
	return c;
}

<* @param [inout] s *>
fn void w4(int[] s)
{
	foreach (&p : s) *p = 1;
}

<* @param [in] p *>
fn int rd(int* p) => *p;

<* @param [out] s *>
fn void o1(int[] s)
{
	foreach (&p : s) { int y = *p; } // #error: 'out' parameters may not be read
}

<* @param [out] s *>
fn void o2(int[] s)
{
	foreach (&p : s) *p += 1; // #error: 'out' parameters may not be read
}

<* @param [out] s *>
fn void o3(int[] s)
{
	foreach (&p : s) rd(p); // #error: An 'out' parameter may not be passed
}

<* @param [out] s *>
fn void o4(int[] s)
{
	foreach (i, &p : s) { *p = (int)i; p[0] = 2; }
}

<* @param [inout] s *>
fn void o5(int[] s)
{
	foreach (&p : s) { *p += 1; rd(p); }
}
