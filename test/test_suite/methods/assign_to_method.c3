struct Foo { int a; }
alias MFn = fn int(Foo*);
macro Foo.mm(&self) => 1;
fn int Foo.m(&self) => 1;
fn int int.my(&self) => 3;
fn int other(Foo* f) => 2;
fn int oi(int* i) => 4;

fn void sub_assign_literal()
{
	(int){}.hash -= 0; // #error: You cannot assign a value to a macro
}

fn void assign_builtin_method()
{
	int x;
	x.hash = 0; // #error: You cannot assign a value to a macro
}

fn void incdec_builtin_method()
{
	int x;
	x.hash++; // #error: You cannot assign a value to a macro
}

fn void assign_macro_method()
{
	Foo f;
	f.mm = 1; // #error: You cannot assign a value to a macro
}

fn void op_assign_macro_method_ptr()
{
	Foo* f;
	--f.mm; // #error: You cannot assign a value to a macro
}

fn void assign_function_method()
{
	Foo f;
	f.m = &other; // #error: You cannot assign a value to a function
}

fn void op_assign_function_method()
{
	Foo f;
	f.m += 1; // #error: You cannot assign a value to a function
}

fn void assign_builtin_type_function_method()
{
	int x;
	x.my = &oi; // #error: You cannot assign a value to a function
}

fn void defined()
{
	int x;
	Foo f;
	$assert !$defined(x.hash = 0);
	$assert !$defined(x.hash += 1);
	$assert !$defined(x.hash++);
	$assert !$defined(f.mm = 1);
	$assert !$defined(f.m = &other);
	$assert !$defined(x.my = &oi);
	$assert $defined(f.a = 1);
	f.a += f.mm() + f.m() + x.my();
	MFn p = &Foo.m;
}
