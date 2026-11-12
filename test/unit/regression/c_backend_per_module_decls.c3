<* The C backend declares in each module's file only what its code uses. Macros and generics put code of one module
into another module's file, so these reach declarations the using module does not import. *>
module c_backend_per_module_decls_lib;

struct Rgb
{
	char r, g, b;
}

enum Color : char (int bits, Rgb rgb)
{
	RED   { 1, { 255, 0, 0 } },
	GREEN { 2, { 0, 255, 0 } },
}

struct Box
{
	int[4] values;
	Rgb color;
}

alias IntFn = fn int(int);

interface Shape
{
	fn int area();
}

struct Square (Shape)
{
	int side;
}

fn int Square.area(&self) @dynamic => self.side * self.side;

int counter @private;

fn int bump(int x) @private
{
	counter += x;
	return counter;
}

fn int twice(int x) => x * 2;

fn Box make_box() => { { 1, 2, 3, 4 }, { 1, 2, 3 } };

macro int call_private(x) => bump(x);
macro int* counter_ref() => &counter;
macro IntFn private_fn() => &bump;
macro Rgb* color_rgb_ref(Color c) => &&c.rgb;

module c_backend_per_module_decls @test;
import c_backend_per_module_decls_lib;

fn void macro_uses_private_function_and_global()
{
	int before = *c_backend_per_module_decls_lib::counter_ref();
	assert(c_backend_per_module_decls_lib::call_private(3) == before + 3);
	assert(*c_backend_per_module_decls_lib::counter_ref() == before + 3);
}

fn void function_addresses()
{
	IntFn f = &c_backend_per_module_decls_lib::twice;
	assert(f(4) == 8);
	IntFn p = c_backend_per_module_decls_lib::private_fn();
	int before = *c_backend_per_module_decls_lib::counter_ref();
	assert(p(1) == before + 1);
}

fn void struct_returned_by_value()
{
	Box b = c_backend_per_module_decls_lib::make_box();
	assert(b.values[3] == 4 && b.color.b == 3);
}

fn void enum_associated_struct_values()
{
	Color c = GREEN;
	assert(c.bits == 2);
	assert(c.rgb.g == 255);
	assert(c_backend_per_module_decls_lib::color_rgb_ref(RED).r == 255);
}

fn void interface_dispatch()
{
	Square s = { 3 };
	Shape shape = &s;
	assert(shape.area() == 9);
}
