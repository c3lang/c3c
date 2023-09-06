/**
 * @require Type.kindof == UNSIGNED_INT
 **/
module std::collections::bitmap(<Type>);
import std::collections::list;

def BitmapList = List(<Type>);

struct Bitmap
{
	BitmapList data;
}

fn void Bitmap.init(&self, usz initial_capacity = 1, Allocator* using = mem::heap())
{
	self.data.init(initial_capacity, using);
}

fn void Bitmap.tinit(&self)
{
	self.init(.using = mem::temp());
}

fn usz Bitmap.cardinality(&self)
{
	usz n;
	foreach (x : self.data)
	{
		n += x.popcount();
	}
	return n;
}

fn void Bitmap.set(&self, usz i)
{
	usz q = i / Type.sizeof;
	usz r = i % Type.sizeof;
	if (q >= self.data.len())
	{
		self.data.reserve(q + 1);
		self.data.size = q + 1;
	}
	self.data.set(q, self.data[q] | (1 << r));
}

fn void Bitmap.unset(&self, usz i)
{
	usz q = i / Type.sizeof;
	usz r = i % Type.sizeof;
	if (q >= self.data.len()) return;
	self.data.set(q, self.data[q] &~ (1 << r));
}

fn bool Bitmap.get(&self, usz i) @operator([]) @inline
{
	usz q = i / Type.sizeof;
	usz r = i % Type.sizeof;
	if (q >= self.data.len()) return false;
	return self.data[q] & (1 << r) != 0;
}

fn usz Bitmap.len(&self) @operator(len)
{
	usz n = self.data.len();
	if (n > 0) n -= self.data[n - 1].ctz();
	return n;
}

fn void Bitmap.set_bool(&self, usz i, bool value) @operator([]=) @inline
{
	if (value) return self.set(i);
	self.unset(i);
}