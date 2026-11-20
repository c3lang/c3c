module subscript_typedef_fallback;

typedef PlainSlice = int[];
typedef PlainArr = int[3];
constset SliceSet : int[] { B = { 1, 2 } }
constset ArrSet : int[3] { A = { 1, 2, 3 } }

fn void plain_typedefs_use_underlying() @test
{
	int[3] a = { 1, 2, 3 };
	PlainSlice s = (PlainSlice)a[..];
	PlainArr arr = { 4, 5, 6 };
	s[0] = 7;
	arr[0] = 8;
	assert(s[0] == 7 && s.len == 3 && lengthof(s) == 3);
	assert(arr[0] == 8 && arr.len == 3 && lengthof(arr) == 3);
	int sum;
	foreach (x : s) sum += x;
	foreach (x : arr) sum += x;
	foreach (x : SliceSet.B) sum += x;
	assert(sum == 7 + 2 + 3 + 8 + 5 + 6 + 1 + 2);
}

fn void lengthof_constset() @test
{
	ArrSet a = ArrSet.A;
	SliceSet b = SliceSet.B;
	assert(lengthof(a) == 3 && lengthof(b) == 2);
	assert(lengthof(ArrSet.A) == 3 && lengthof(SliceSet.B) == 2);
	assert(a.len == 3 && b.len == 2);
}
