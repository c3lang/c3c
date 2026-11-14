module assign_early_exit;
import std::io;

bitstruct AeeBits : uint
{
	int a : 0..7;
	int b : 8..15;
}

macro int? aee_bad() => io::EOF~;

int aee_calls;

fn int? aee_fbad()
{
	aee_calls++;
	return io::EOF~;
}

fn int aee_side()
{
	aee_calls++;
	return 0;
}

fn void? arr_r() { int[3] a; a[aee_side()] = aee_bad()!; }
fn void? arr_l() { int[3] a; a[aee_bad()!] = aee_side(); }
fn void? arr_both() { int[3] a; a[aee_bad()!] = aee_bad()!; }
fn void? vec_r() { int[<3>] v; v[aee_side()] = aee_bad()!; }
fn void? vec_l() { int[<3>] v; v[aee_bad()!] = aee_side(); }
fn void? vec_both() { int[<3>] v; v[aee_bad()!] = aee_bad()!; }
fn void? vec_rt() { int[<3>] v; v[aee_side()] = aee_fbad()!; }
fn void? bits_r() { AeeBits[2] b; b[aee_side()].a = aee_bad()!; }
fn void? bits_l() { AeeBits[2] b; b[aee_bad()!].a = aee_side(); }
fn void? bits_both() { AeeBits[2] b; b[aee_bad()!].a = aee_bad()!; }
fn void? bits_rt() { AeeBits[2] b; b[aee_side()].a = aee_fbad()!; }
fn void? bits_single_r() { AeeBits b; b.a = aee_bad()!; }
fn void? swz_r() { int[<4>][2] v; v[aee_side()].xy = { aee_bad()!, 1 }; }
fn void? swz_l() { int[<4>][2] v; v[aee_bad()!].xy = { aee_side(), 1 }; }
fn void? swz_both() { int[<4>][2] v; v[aee_bad()!].xy = { aee_bad()!, 1 }; }
fn void? slice_r() { int[3] a; a[aee_side()..1] = aee_bad()!; }
fn void? slice_l() { int[3] a; a[aee_bad()!..1] = aee_side(); }
fn void? vec_add_r() { int[<3>] v; v[aee_side()] += aee_bad()!; }
fn void? vec_add_l() { int[<3>] v; v[aee_bad()!] += aee_side(); }
fn void? bits_add_r() { AeeBits[2] b; b[aee_side()].a += aee_bad()!; }
fn void? bits_add_l() { AeeBits[2] b; b[aee_bad()!].a += aee_side(); }
fn void? swz_add_r() { int[<4>][2] v; v[aee_side()].xy += { aee_bad()!, 1 }; }
fn int? vec_value() { int[<3>] v; return (v[aee_side()] = aee_bad()!) + 1; }
fn int? bits_value() { AeeBits[2] b; return (b[aee_side()].a = aee_bad()!) + 1; }
fn int? arr_read() { int[3] a; return a[aee_bad()!]; }
fn int? ptr_read() { int[3] a; int* p = &a; return p[aee_bad()!]; }
fn int? slice_read() { int[3] a; int[] s = &a; return s[aee_bad()!]; }
fn int? vec_read() { int[<3>] v; return v[aee_bad()!]; }

macro void @expect_eof(#call, int calls)
{
	aee_calls = 0;
	assert(@catch(#call) == io::EOF);
	assert(aee_calls == calls);
}

fn void rhs_exit_skips_lhs() @test
{
	@expect_eof(arr_r(), 0);
	@expect_eof(vec_r(), 0);
	@expect_eof(vec_rt(), 1);
	@expect_eof(bits_r(), 0);
	@expect_eof(bits_rt(), 1);
	@expect_eof(bits_single_r(), 0);
	@expect_eof(swz_r(), 0);
	@expect_eof(slice_r(), 0);
	@expect_eof(vec_value(), 0);
	@expect_eof(bits_value(), 0);
}

fn void lhs_exit_after_rhs() @test
{
	@expect_eof(arr_l(), 1);
	@expect_eof(vec_l(), 1);
	@expect_eof(bits_l(), 1);
	@expect_eof(swz_l(), 1);
	@expect_eof(slice_l(), 1);
}

fn void both_exit() @test
{
	@expect_eof(arr_both(), 0);
	@expect_eof(vec_both(), 0);
	@expect_eof(bits_both(), 0);
	@expect_eof(swz_both(), 0);
}

fn void compound_assign_exit() @test
{
	@expect_eof(vec_add_r(), 1);
	@expect_eof(vec_add_l(), 0);
	@expect_eof(bits_add_r(), 1);
	@expect_eof(bits_add_l(), 0);
	@expect_eof(swz_add_r(), 1);
}

fn void subscript_read_exit() @test
{
	@expect_eof(arr_read(), 0);
	@expect_eof(ptr_read(), 0);
	@expect_eof(slice_read(), 0);
	@expect_eof(vec_read(), 0);
}
