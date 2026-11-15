module overload_subscript_from_end;

struct Box { int[4] v; }
fn int Box.get(&self, usz i) @operator([]) => self.v[i];
fn void Box.set(&self, usz i, int x) @operator([]=) { self.v[i] = x; }
fn usz Box.len(&self) @operator(len) => 4;

struct RefBox { int[4] v; }
fn int* RefBox.ref(&self, usz i) @operator(&[]) => &self.v[i];
fn int RefBox.get(&self, usz i) @operator([]) => self.v[i];
fn void RefBox.set(&self, usz i, int x) @operator([]=) { self.v[i] = x; }
fn usz RefBox.len(&self) @operator(len) => 4;

struct Money { int cents; }
fn Money Money.add(self, Money o) @operator(+) => { self.cents + o.cents };
struct Wallet { Money[3] m; }
fn Money Wallet.get(&self, usz i) @operator([]) => self.m[i];
fn void Wallet.set(&self, usz i, Money x) @operator([]=) { self.m[i] = x; }
fn usz Wallet.len(&self) @operator(len) => 3;

int calls;
Box global_box = { { 1, 2, 3, 4 } };
fn Box* get_box() { calls++; return &global_box; }
fn usz one() { calls += 10; return 1; }

fn void get_and_set_from_end() @test
{
	Box b = { { 1, 2, 3, 4 } };
	b[^1] += 10;
	b[^2]++;
	--b[^4];
	int old = b[^3]++;
	assert(old == 2);
	assert(b.v == { 0, 3, 4, 14 });
	b[1] *= 5;
	assert(b.v[1] == 15);
}

fn void ref_overload_from_end() @test
{
	RefBox r = { { 1, 2, 3, 4 } };
	r[^1] += 5;
	r[^2]++;
	assert(r.v == { 1, 2, 4, 9 });
}

fn void element_operator_overload_from_end() @test
{
	Wallet w = { { { 1 }, { 2 }, { 3 } } };
	w[^1] += (Money){ 10 };
	w[1] += (Money){ 20 };
	assert(w.m[0].cents == 1 && w.m[1].cents == 22 && w.m[2].cents == 13);
}

fn void evaluated_once() @test
{
	calls = 0;
	(*get_box())[^one()] += 100;
	assert(global_box.v[3] == 104);
	assert(calls == 11);
}
