module panic_messages;
import std, libc;

faultdef BAD_THING;
enum Color { RED, GREEN }
alias VoidFn = fn void();
interface Greeter { fn void greet(); }
struct Thing { int a; }

JmpBuf panic_buf;
String panic_message;
String panic_function;
String panic_summary;

int zero = 0;
int minus_one = -1;
int ten = 10;
uint uten = 10;
ulong huge = 0x8000_0000_0000_0000;
VoidFn null_fn = null;
int* null_ptr = null;
Thing* null_thing = null;
typeid int_type = int::typeid;
int128 big = (int128)1 << 100;
uint128 ubig = (uint128)1 << 100;
int[<2>] vec_values = { 1, 2 };
int[<2>] vec_shifts = { 1, 40 };
int[<2>] vec_negative = { -3, 1 };
uint[<2>] uvec_values = { 1, 2 };
uint[<2>] uvec_shifts = { 1, 40 };

fn void panic_handler(String summary, String file, String function, int line, String fmt, any[] args)
{
	panic_summary = summary;
	panic_message = fmt.len ? string::tformat(fmt, ...args) : summary.tcopy();
	panic_function = function.tcopy();
	libc::longjmp(&panic_buf, 1);
}

macro void @expect_panic(String expected; @body())
{
	PanicFn saved = builtin::panic;
	builtin::panic = &panic_handler;
	panic_message = "";
	if (libc::setjmp(&panic_buf) == 0) @body();
	builtin::panic = saved;
	assert(panic_message == expected, "Got '%s', expected '%s'", panic_message, expected);
}

<* @require x > 0 : "x must be positive" *>
fn void require_message(int x) { }

<* @require x > 0 *>
fn void require_plain(int x) { }

<* @ensure return > 0 : "return must be positive" *>
fn int ensure_message() { return zero; }

<* @ensure return > 0 *>
fn int ensure_plain() { return zero; }

fn int? failing() { return BAD_THING~; }

macro void Thing.touch_macro(&self) { self.a++; }
fn void Thing.touch(&self) { self.a++; }
<* @param [&in] p *>
macro int ref_macro(int* p) => *p;
<* @param [&in] p *>
fn int ref_fn(int* p) => *p;

macro void @expect_full_panic(String expected_summary, String expected; @body())
{
	@expect_panic(expected) { @body(); };
	assert(panic_summary == expected_summary, "Got summary '%s', expected '%s'", panic_summary, expected_summary);
}

fn void test_contracts() @test @feat(!(WIN32 & AARCH64)) @feat(!WASM) @feat(SAFE_MODE)
{
	@expect_panic("@require \"x > 0\" violated: 'x must be positive'.") { require_message(zero); };
	@expect_panic("@require \"x > 0\" violated.") { require_plain(zero); };
	@expect_panic("@ensure \"return > 0\" violated: 'return must be positive'.") { ensure_message(); };
	@expect_panic("@ensure \"return > 0\" violated.") { ensure_plain(); };
	assert(panic_summary == "@ensure violated");
}

fn void test_asserts() @test @feat(!(WIN32 & AARCH64)) @feat(!WASM) @feat(SAFE_MODE)
{
	@expect_panic("Assert \"zero > 0\" failed.") { assert(zero > 0); };
	@expect_panic("100% custom") { assert(zero > 0, "100% custom"); };
	@expect_panic("formatted 10") { assert(zero > 0, "formatted %d", ten); };
}

fn void test_arithmetic() @test @feat(!(WIN32 & AARCH64)) @feat(!WASM) @feat(SAFE_MODE)
{
	@expect_panic("Division by zero.") { ten = ten / zero; };
	@expect_panic("% by zero.") { ten = ten % zero; };
	@expect_panic("Shift amount out of range (was 40).") { ten = ten << (ten * 4); };
	@expect_panic("Shift amount out of range (was 40).") { uten = uten >> (uten * 4); };
	@expect_panic("Shift amount out of range (was -1).") { ten = ten << minus_one; };
	@expect_panic("Shift amount out of range (was 9223372036854775808).") { huge = huge >> huge; };
}

fn void test_indexing() @test @feat(!(WIN32 & AARCH64)) @feat(!WASM) @feat(SAFE_MODE)
{
	int[3] array;
	int[] slice = array[..];
	@expect_panic("Array index out of bounds (array had size 3, index was 10)") { array[ten] = 1; };
	assert(panic_summary == "Array index out of bounds");
	@expect_panic("Negative array indexing (index was -1)") { array[minus_one] = 1; };
	@expect_panic("Array index out of bounds (array had size 3, index was 10)") { slice[uten] = 1; };
	@expect_panic("Index exceeds array length (array had size 3, index was 10).") { (void)slice[ten..]; };
	@expect_panic("Negative indexing (-1)") { (void)slice[minus_one..]; };
	@expect_panic("End index out of bounds (end index of 10 exceeds size of 3)") { (void)slice[0..ten]; };
	@expect_panic("Slice copy length mismatch (3 != 2).") { slice[..] = array[:(usz)(zero + 2)]; };
}

fn void test_other() @test @feat(!(WIN32 & AARCH64)) @feat(!WASM) @feat(SAFE_MODE)
{
	@expect_panic("Unexpected fault 'panic_messages::BAD_THING' was unwrapped!") { (void)failing()!!; };
	@expect_panic("Attempt to convert a negative value (-1) to enum 'Color' failed.") { Color c = (Color)minus_one; (void)c; };
	@expect_panic("Attempting to convert 10 to enum 'Color' failed as the value exceeds the max ordinal (1).") { Color c = (Color)ten; (void)c; };
	@expect_panic("Calling null function pointer, 'null_fn' was null.") { null_fn(); };
	assert(panic_function == "test_other");
}

fn void test_summaries() @test @feat(!(WIN32 & AARCH64)) @feat(!WASM) @feat(SAFE_MODE)
{
	int[3] array;
	int[] slice = array[..];
	@expect_full_panic("Assert failed", "Assert \"zero > 0\" failed.") { assert(zero > 0); };
	@expect_full_panic("100% custom", "100% custom") { assert(zero > 0, "100% custom"); };
	@expect_full_panic("Assert violation", "formatted 10") { assert(zero > 0, "formatted %d", ten); };
	@expect_full_panic("Assert violation", "Assert violation") { assert(false); };
	@expect_full_panic("@require violated", "@require \"x > 0\" violated.") { require_plain(zero); };
	@expect_full_panic("@require violated", "@require \"x > 0\" violated: 'x must be positive'.") { require_message(zero); };
	@expect_full_panic("@ensure violated", "@ensure \"return > 0\" violated.") { ensure_plain(); };
	@expect_full_panic("Division by zero.", "Division by zero.") { ten = ten / zero; };
	@expect_full_panic("% by zero.", "% by zero.") { ten = ten % zero; };
	@expect_full_panic("Invalid shift", "Shift amount out of range (was -1).") { ten = ten << minus_one; };
	@expect_full_panic("Array index out of bounds", "Array index out of bounds (array had size 3, index was 10)") { array[ten] = 1; };
	@expect_full_panic("Negative array indexing", "Negative array indexing (index was -1)") { array[minus_one] = 1; };
	@expect_full_panic("Index exceeds array len", "Index exceeds array length (array had size 3, index was 10).") { (void)slice[ten..]; };
	@expect_full_panic("Negative value", "Negative indexing (-1)") { (void)slice[minus_one..]; };
	@expect_full_panic("End index out of bounds", "End index out of bounds (end index of 10 exceeds size of 3)") { (void)slice[0..ten]; };
	@expect_full_panic("Length mismatch", "Slice copy length mismatch (3 != 2).") { slice[..] = array[:(usz)(zero + 2)]; };
	@expect_full_panic("Force unwrap failed!", "Unexpected fault 'panic_messages::BAD_THING' was unwrapped!") { (void)failing()!!; };
	@expect_full_panic("Attempt to convert negative value to enum failed.", "Attempt to convert a negative value (-1) to enum 'Color' failed.") { Color c = (Color)minus_one; (void)c; };
	@expect_full_panic("Failed integer to enum conversion", "Attempting to convert 10 to enum 'Color' failed as the value exceeds the max ordinal (1).") { Color c = (Color)ten; (void)c; };
	@expect_full_panic("Calling null function pointer", "Calling null function pointer, 'null_fn' was null.") { null_fn(); };
}

fn void test_pointers() @test @feat(!(WIN32 & AARCH64)) @feat(!WASM) @feat(SAFE_MODE)
{
	int[3] array;
	@expect_full_panic("Dereference of null pointer", "Dereference of null pointer, 'null_ptr' was null.") { (void)*null_ptr; };
	int* unaligned = (int*)((char*)&array + 1);
	@expect_full_panic("Unaligned pointer access detected", "Unaligned access: ptr % 4 = 1, use mem::load / mem::store for unaligned access.") { (void)*unaligned; };
	@expect_full_panic("Called a method on a null value.", "Called a method on a null value.") { null_thing.touch_macro(); };
	@expect_full_panic("Reference parameter was passed a null pointer argument.", "Reference parameter 'self' was passed a null pointer argument.") { null_thing.touch(); };
	@expect_full_panic("Passed null to a ref ('&') parameter.", "Passed null to a ref ('&') parameter.") { (void)ref_macro(null_ptr); };
	@expect_full_panic("Reference parameter was passed a null pointer argument.", "Reference parameter 'p' was passed a null pointer argument.") { (void)ref_fn(null_ptr); };
}

fn void test_dynamic() @test @feat(!(WIN32 & AARCH64)) @feat(!WASM) @feat(SAFE_MODE)
{
	int value;
	any thing = &value;
	@expect_full_panic("No method could be found on target", "No method 'greet' could be found on target") { Greeter g = (Greeter)thing; g.greet(); };
	@expect_full_panic("Attempted to access 'inner' on non composite type", "Attempted to access 'inner' on non composite type") { (void)int_type.inner; };
	@expect_full_panic("Attempted to access 'names' on non enum/fault type.", "Attempted to access 'names' on non enum/fault type.") { (void)int_type.names; };
	@expect_full_panic("Attempted to access 'len' on non array type", "Attempted to access 'len' on non array type") { (void)int_type.len; };
	@expect_full_panic("Infinite loop found", "Infinite loop found") { for (;;) {} };
}

fn void test_slices() @test @feat(!(WIN32 & AARCH64)) @feat(!WASM) @feat(SAFE_MODE)
{
	int[3] array;
	int[] slice = array[..];
	@expect_full_panic("Negative size", "Negative size (slice was: [2..0])") { (void)slice[2..zero]; };
	@expect_full_panic("Negative slice length", "Negative value (-1) given for slice length.") { (void)slice[2:minus_one]; };
	@expect_full_panic("End index out of bounds", "End index out of bounds (end index of 9 exceeds size of 3)") { (void)slice[zero:ten]; };
}

fn void test_wide_values() @test @feat(!(WIN32 & AARCH64)) @feat(!WASM) @feat(SAFE_MODE)
{
	@expect_panic("Shift amount out of range (was 1267650600228229401496703205376).") { big = big << big; };
	big = (int128)1 << 100;
	@expect_panic("Shift amount out of range (was 1267650600228229401496703205376).") { ubig = ubig >> ubig; };
	ubig = (uint128)1 << 100;
	@expect_panic("Attempt to convert a negative value (-1267650600228229401496703205376) to enum 'Color' failed.") { Color c = (Color)-big; (void)c; };
	@expect_panic("Attempting to convert 1267650600228229401496703205376 to enum 'Color' failed as the value exceeds the max ordinal (1).") { Color c = (Color)big; (void)c; };
	@expect_panic("Attempting to convert 1267650600228229401496703205376 to enum 'Color' failed as the value exceeds the max ordinal (1).") { Color c = (Color)ubig; (void)c; };
}

fn void test_vector_shifts() @test @feat(!(WIN32 & AARCH64)) @feat(!WASM) @feat(SAFE_MODE)
{
	@expect_panic("Shift amount out of range (was 40).") { vec_values = vec_values << vec_shifts; };
	@expect_panic("Shift amount out of range (was -3).") { vec_values = vec_values << vec_negative; };
	@expect_panic("Shift amount out of range (was 40).") { uvec_values = uvec_values >> uvec_shifts; };
}

fn void test_narrow_values() @test @feat(!(WIN32 & AARCH64)) @feat(!WASM) @feat(SAFE_MODE)
{
	int[3] array;
	ichar negative = -1;
	char large = 200;
	ichar shift_c = 40;
	short shift_s = 50;
	@expect_panic("Negative array indexing (index was -1)") { array[negative] = 1; };
	@expect_panic("Array index out of bounds (array had size 3, index was 200)") { array[large] = 1; };
	@expect_panic("Shift amount out of range (was 40).") { shift_c = shift_c << shift_c; };
	@expect_panic("Shift amount out of range (was 50).") { shift_s = shift_s << shift_s; };
	@expect_panic("Attempting to convert 200 to enum 'Color' failed as the value exceeds the max ordinal (1).") { Color c = (Color)large; (void)c; };
	@expect_panic("Attempt to convert a negative value (-1) to enum 'Color' failed.") { Color c = (Color)negative; (void)c; };
}

fn void test_shift_amount_sign() @test @feat(!(WIN32 & AARCH64)) @feat(!WASM) @feat(SAFE_MODE)
{
	ichar narrow_byte = -1;
	short narrow = -2;
	int amount = -3;
	uint large = 3_000_000_000;
	int value = 1;
	uint uvalue = 1;
	long lvalue = 1;
	int128 wide = 1;
	short[<2>] narrow_vec = { 1, -2 };
	@expect_panic("Shift amount out of range (was -1).") { value = value << narrow_byte; };
	@expect_panic("Shift amount out of range (was -2).") { value = value << narrow; };
	@expect_panic("Shift amount out of range (was -2).") { value = value >> narrow; };
	@expect_panic("Shift amount out of range (was -2).") { value <<= narrow; };
	@expect_panic("Shift amount out of range (was -2).") { uvalue = uvalue << narrow; };
	@expect_panic("Shift amount out of range (was -2).") { lvalue = lvalue << narrow; };
	@expect_panic("Shift amount out of range (was -2).") { wide = wide << narrow; };
	@expect_panic("Shift amount out of range (was -3).") { uvalue = uvalue >> amount; };
	@expect_panic("Shift amount out of range (was 3000000000).") { value = value << large; };
	@expect_panic("Shift amount out of range (was -2).") { vec_values = vec_values << narrow_vec; };
	@expect_panic("Shift amount out of range (was -2).") { uvec_values = uvec_values << narrow_vec; };
}

fn void test_panic_fn_default() @test
{
	assert((void*)$$PANIC_FN == (void*)&builtin::default_panic);
}
