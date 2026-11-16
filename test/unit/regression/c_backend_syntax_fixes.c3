// Constructs whose generated C failed to compile, found by the C backend's --test syntax check.
module c_backend_syntax_fixes_helper;

fn int? static_optional()
{
	static int? x = 120;
	return x;
}

module std::c_backend_syntax_fixes @test;
import c_backend_syntax_fixes_helper;

const String STRING_NULL = (String)null[0..0];
const String FOO = STRING_NULL;
bitstruct Foo2 : char[2] { bool enable_help; bool enable_version; }
alias Foo = char[500];
struct MixedHFAv3arr { float[<3>][3] arr; ichar[16] b; }

fn void const_global_set_at_startup()
{
	assert(FOO.len == 1 && FOO.ptr == null);
}

fn void char_array_bitstruct_truth()
{
	Foo2 a = { .enable_help };
	Foo2 b = { .enable_version };
	Foo2 c = { .enable_version, .enable_help };
	assert(!(a & b));
	assert(a & c);
}

fn void pointer_vector_increment()
{
	Foo* x = mem::calloc(Foo::size * 10);
	defer mem::free(x);
	Foo*[<1>] z = { x };
	for (int i = 0; i < 3; i++) z++;
	z--;
	assert((iptr)z[0] - (iptr)x == 2 * Foo::size);
}

fn void swizzle_of_array_element()
{
	MixedHFAv3arr b;
	b.arr[1].x++;
	b.arr[1].x += 2;
	assert(b.arr[1] == (float[<3>]){ 3, 0, 0 });
}

fn void static_optional_starts_without_fault()
{
	assert(c_backend_syntax_fixes_helper::static_optional()!! == 120);
}

fn void failed_embed_is_a_fault()
{
	char*? data = $embed("does_not_exist");
	assert(!@ok(data));
}

fn void vector_saturating()
{
	char[<2>] x = { 123, 222 };
	char[<2>] y = { 143, 50 };
	ichar[<2>] z = { 120, -120 };
	assert($$sat_add(x, y) == (char[<2>]){ 255, 255 });
	assert($$sat_add(z, z) == (ichar[<2>]){ 127, -128 });
	assert($$sat_sub(x, y) == (char[<2>]){ 0, 172 });
	assert($$sat_shl(x, (char[<2>]){ 1, 1 }) == (char[<2>]){ 246, 255 });
}
