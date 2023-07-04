// Copyright (c) 2021 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by the MIT license
// a copy of which can be found in the LICENSE_STDLIB file.
module std::hash::fnv32a;

def Fnv32a = distinct uint;

const FNV32A_START @private = 0x811c9dc5;
const FNV32A_MUL @private = 0x01000193;

macro void @update(uint &h, char x) @private => h = (h * FNV32A_MUL) ^ x;

fn void Fnv32a.init(&this)
{
    *this = FNV32A_START;
}

fn void Fnv32a.update(&this, char[] data)
{
	uint h = (uint)*this;
	foreach (char x : data)
    {
        @update(h, x);
    }
    *this = (Fnv32a)h;
}

macro void Fnv32a.update_char(&this, char c)
{
	@update(*this, x);
}

fn uint encode(char[] data)
{
	uint h = FNV32A_START;
	foreach (char x : data)
    {
		@update(h, x);
    }
    return h;
}