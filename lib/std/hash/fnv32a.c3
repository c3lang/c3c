// Copyright (c) 2021 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by the MIT license
// a copy of which can be found in the LICENSE_STDLIB file.
module std::hash::fnv32a;

typedef Fnv32a = uint;

const FNV32A_START @private = 0x811c9dc5;
const FNV32A_MUL @private = 0x01000193;

macro void update(h, char x) @private => *h = (*h ^ ($typeof(*h))x) * FNV32A_MUL;

fn void Fnv32a.init(&self)
{
	*self = FNV32A_START;
}

fn void Fnv32a.update(&self, char[] data)
{
	Fnv32a h = *self;
	foreach (char x : data)
	{
		update(&h, x);
	}
	*self = h;
}

macro void Fnv32a.update_char(&self, char c)
{
	update(self, c);
}

fn uint hash(char[] data)
{
	uint h = FNV32A_START;
	foreach (char x : data)
	{
		update(&h, x);
	}
	return h;
}
