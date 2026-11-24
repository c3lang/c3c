typedef OnlyIdx = int[];
fn int OnlyIdx.get(self, sz i) @operator([]) => 0;
typedef OnlyLen = int[];
fn sz OnlyLen.len(self) @operator(len) => 0;
typedef ValOnly = int[];
fn int ValOnly.get(self, sz i) @operator([]) => 0;
fn sz ValOnly.len(self) @operator(len) => 0;
typedef IdxArr = int[3];
fn int IdxArr.get(self, sz i) @operator([]) => 0;
constset Ptrs : int* { A = null }

fn void foreach_no_len(OnlyIdx o)
{
	foreach (x : o) {} // #error: 'OnlyIdx' can't be used with 'foreach', since it has no '@operator(len)'
}

fn void foreach_no_index(OnlyLen o)
{
	foreach (x : o) {} // #error: 'OnlyLen' can't be used with 'foreach', since it has no '@operator([])' or '@operator(&[])'
}

fn void foreach_no_ref(ValOnly o)
{
	foreach (&x : o) {} // #error: 'ValOnly' has no '@operator(&[])', so 'foreach' can only iterate it by value
}

fn void index_no_fallback(OnlyLen o)
{
	int x = o[0]; // #error: Indexing a value of type 'OnlyLen' is not possible
}

fn void lengthof_no_fallback(OnlyIdx o)
{
	sz n = lengthof(o); // #error: 'OnlyIdx' does not support lengthof()
}

fn void len_no_fallback_slice(OnlyIdx o)
{
	usz n = o.len; // #error: There is no field or method 'OnlyIdx.len'
}

fn void len_no_fallback_array(IdxArr o)
{
	usz n = o.len; // #error: There is no field or method 'IdxArr.len'
}

fn void lengthof_no_fallback_array(IdxArr o)
{
	sz n = lengthof(o); // #error: 'IdxArr' does not support lengthof()
}

fn void constset_of_pointers(Ptrs p)
{
	foreach (x : p) {} // #error: It's not possible to enumerate an expression of type 'Ptrs'
}

fn void defined_len(IdxArr o)
{
	$assert !$defined(o.len);
}
