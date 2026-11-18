struct Pt { int x; int y; }
fn int Pt.sum(&self) => self.x + self.y;
enum Col : (int val, int[2] arr, Pt pt, int[<2>] v)
{
	RED { 1, { 1, 2 }, { 3, 4 }, { 5, 6 } },
	BLUE { 2, { 7, 8 }, { 9, 10 }, { 11, 12 } }
}

fn void assign()
{
	Col c;
	c.val = 3; // #error: Associated enum values are constant
}

fn void incdec()
{
	Col c;
	c.val++; // #error: Associated enum values are constant
}

fn void through_pointer(Col* cp)
{
	cp.val += 2; // #error: Associated enum values are constant
}

fn void subscript()
{
	Col c;
	c.arr[0] = 9; // #error: Associated enum values are constant
}

fn void field()
{
	Col c;
	c.pt.x = 9; // #error: Associated enum values are constant
}

fn void vector_element()
{
	Col c;
	c.v[1] = 9; // #error: Associated enum values are constant
}

fn void valid(Col* cp)
{
	Col c = BLUE;
	int x = c.val + cp.val + c.arr[1] + c.pt.y + c.v.y + Col.RED.val;
	int s = c.pt.sum();
	int* t = &&c.val;
	int* p = &c.val;
	int* q = &c.arr[1];
	int* r = &c.pt.y;
	int[2]* a = &c.arr;
	$assert !$defined(c.val = 1);
	$assert !$defined(c.arr[0] = 1);
	$assert $defined(&c.val);
}
