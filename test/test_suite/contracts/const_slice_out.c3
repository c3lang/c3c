const int[] CS = { 1, 2, 3 };

<* @param [out] p *>
fn void wr(int* p) { *p = 1; }

<* @param [out] s *>
fn void sl(int[] s) { s[0] = 1; }

fn void element_const_index()
{
	wr(&CS[1]); // #error: const parameter may not be passed
}

fn void slice_runtime_range(int i)
{
	sl(CS[i..]); // #error: const parameter may not be passed
}

fn void assign_runtime_index(int i)
{
	CS[i] = 1; // #error: cannot assign to an element of a constant
}

fn void assign_const_index()
{
	CS[0] = 1; // #error: cannot assign to an element of a constant
}

int[3] g;
const int[][] SS = { &g };
const int[] GS = &g;

fn void element_slice_points_elsewhere(int i)
{
	SS[i][0] = 1;
	wr(&SS[i][2]);
	wr(&SS[0][2]);
	GS[i] = 1;
	sl(GS);
}

typedef Keys = int[];
fn int Keys.get(self, usz i) @operator([]) => 0;
fn void Keys.set(self, usz i, int v) @operator([]=) {}
fn usz Keys.len(self) @operator(len) => 0;
const Keys K = { 1, 2 };

<* @param [out] k *>
fn void set_keys(Keys k) { k[0] = 1; }

fn void overloaded_set(usz i)
{
	K[i] = 7;
	set_keys(K);
}

extern const int[] EXT;

fn void extern_const(int i)
{
	wr(&EXT[0]);
	EXT[i] = 1;
}
