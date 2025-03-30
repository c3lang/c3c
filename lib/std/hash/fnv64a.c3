// Copyright (c) 2021 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by the MIT license
// a copy of which can be found in the LICENSE_STDLIB file.
module std::hash::fnv64a;

typedef Fnv64a = ulong;

const FNV64A_START @private = 0xcbf29ce484222325;
const FNV64A_MUL @private = 0x00000100000001b3;

macro void update(h, char x) @private => *h = (*h ^ ($typeof(*h))x) * FNV64A_MUL;

fn void Fnv64a.init(&self)
{
	*self = FNV64A_START;
}

fn void Fnv64a.update(&self, char[] data)
{
	Fnv64a h = *self;
	foreach (char x : data)
	{
		update(&h, x);
	}
	*self = h;
}

macro void Fnv64a.update_char(&self, char c)
{
	update(self, c);
}

fn ulong hash(char[] data)
{
	ulong h = FNV64A_START;
	foreach (char x : data)
	{
		update(&h, x);
	}
	return h;
}
