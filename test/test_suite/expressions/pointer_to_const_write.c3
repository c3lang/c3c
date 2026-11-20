const int[3] CA = { 1, 2, 3 };

<* @param [out] p *>
fn void set(int* p) { *p = 0; }

fn void ptr_init()
{
	int* p = &CA[0];
	*p = 5; // #error: 'p' points to constant data, which may not be assigned to
}

fn void ptr_index()
{
	int* p = &CA[1];
	p[0] = 5; // #error: 'p' points to constant data, which may not be assigned to
}

fn void ptr_copied()
{
	int* p = &CA[0];
	int* q = p;
	*q = 5; // #error: 'q' points to constant data, which may not be assigned to
}

fn void subslice_copy(int i)
{
	int[] s = CA[i..];
	s[0] = 5;
}

fn void ptr_as_out()
{
	int* p = &CA[0];
	set(p); // #error: const parameter may not be passed
}

fn int ok()
{
	int x;
	int* p = &CA[0];
	int c = *p + p[1];
	p = &x;
	*p = 5;
	int[3] a = CA;
	a[0] = 5;
	return c;
}

<* @param [in] s *>
fn void in_deref_not_pointee(String* s, int[]* sp)
{
	(*s)[0] = 'a';
	(*sp)[0] = 1;
}

<* @param [in] ap *>
fn void in_deref_array(int[3]* ap)
{
	(*ap)[0] = 1; // #error: 'in' parameters may not be assigned to
}

fn void not_const(int i)
{
	int[3] buf;
	int* p = &buf[0];
	bool b = $defined(p = &CA[0]);
	*p = 1;
	int* q = &buf[0];
	if (false) q = &CA[0];
	*q = 2;
	int[] s = (int[3]){ 1, 2, 3 }[i..];
	s[0] = 4;
}
