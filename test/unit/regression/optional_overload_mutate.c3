module optional_overload_mutate;
struct Box { int[4] v; }
fn int Box.get(&self, usz i) @operator([]) => self.v[i];
fn void Box.set(&self, usz i, int x) @operator([]=) { self.v[i] = x; }
fn usz Box.len(&self) @operator(len) => 4;
faultdef BAD;
fn void optional_parent_overload_mutate() @test
{
	Box? ob = { { 1, 2, 3, 4 } };
	(void)ob[1]++;
	(void)ob[^1]++;
	(void)(ob[0] += 10);
	(void)(ob[^2] *= 5);
	int? old = ob[2]--;
	assert(ob!!.v == { 11, 3, 14, 5 } && old!! == 15);
	Box? bad = BAD~;
	int? r = bad[1]++;
	assert(@catch(r) == BAD);
	int? r2 = (bad[^1] += 3);
	assert(@catch(r2) == BAD);
}

struct MaybeBox { int[4] v; int sets; }
fn int? MaybeBox.get(&self, usz i) @operator([])
{
	if (self.v[i] < 0) return BAD~;
	return self.v[i];
}
fn void MaybeBox.set(&self, usz i, int x) @operator([]=) { self.v[i] = x; self.sets++; }
fn usz MaybeBox.len(&self) @operator(len) => 4;

fn void optional_getter_mutate() @test
{
	MaybeBox b = { { 1, -1, 3, 4 }, 0 };
	int? a = b[0]++;
	assert(a!! == 1);
	int? c = (b[^1] += 10);
	assert(c!! == 14);
	int? d = --b[2];
	assert(d!! == 2);
	assert(b.v == { 2, -1, 2, 14 } && b.sets == 3);
	int? e = b[1]++;
	assert(@catch(e) == BAD);
	int? f = (b[1] *= 2);
	assert(@catch(f) == BAD);
	int? g = (b[^3] -= 1);
	assert(@catch(g) == BAD);
	assert(b.v[1] == -1 && b.sets == 3);
}
