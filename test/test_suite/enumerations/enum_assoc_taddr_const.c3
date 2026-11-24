enum Ff : (int* p) { A { &&3 } }
const int* P = &&5;

<* @param [out] x *>
fn void wr(int* x) { *x = 1; }

fn void assoc_is_const()
{
	wr(Ff.A.p); // #error: const parameter may not be passed
}

fn void const_is_const()
{
	wr(P); // #error: const parameter may not be passed
}
