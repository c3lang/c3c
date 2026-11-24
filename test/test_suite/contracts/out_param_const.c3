struct Pt { int x; int y; }
enum Col : (Pt pt) { RED { { 1, 2 } } }
const Pt CP = { 1, 2 };
const Pt[2] CA = { { 1, 2 }, { 3, 4 } };
const int[3] CI = { 1, 2, 3 };

<* @param [inout] self *>
fn void Pt.bump(&self) { self.x++; }
<* @param [out] self *>
fn void Pt.set(&self) { self.x = 1; }
<* @param [in] self *>
fn int Pt.get(&self) => self.x;
<* @param [inout] p *>
fn void bump_ptr(Pt* p) { p.x++; }
<* @param [inout] p *>
fn void bump_int(int* p) { (*p)++; }

fn void method_on_const()
{
	CP.bump(); // #error: A const parameter may not be passed into a function or macro as an 'out' or 'inout' argument
}

fn void out_method_on_const()
{
	CP.set(); // #error: A const parameter may not be passed into a function or macro as an 'out' or 'inout' argument
}

fn void explicit_address()
{
	bump_ptr(&CP); // #error: A const parameter may not be passed into a function or macro as an 'out' or 'inout' argument
}

fn void const_array_element()
{
	CA[1].bump(); // #error: A const parameter may not be passed into a function or macro as an 'out' or 'inout' argument
}

fn void const_array_element_address()
{
	bump_int(&CI[1]); // #error: A const parameter may not be passed into a function or macro as an 'out' or 'inout' argument
}

fn void const_member_address()
{
	bump_int(&CP.x); // #error: A const parameter may not be passed into a function or macro as an 'out' or 'inout' argument
}

fn void associated_value()
{
	Col c = RED;
	c.pt.bump(); // #error: A const parameter may not be passed into a function or macro as an 'out' or 'inout' argument
}

fn void local_const()
{
	const Pt LP = { 5, 6 };
	LP.bump(); // #error: A const parameter may not be passed into a function or macro as an 'out' or 'inout' argument
}

fn void valid()
{
	Col c = RED;
	Pt v;
	Pt[2] arr;
	int x = CP.get() + CA[1].get() + c.pt.get();
	v.bump();
	v.set();
	arr[1].bump();
	bump_ptr(&v);
	(Pt){ 1, 2 }.bump();
}
