module std::crypto::rc4;
// Copyright (c) 2021 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by the MIT license
// a copy of which can be found in the LICENSE_STDLIB file.

struct Rc4
{
	uint i, j;
	char[256] state;
}

/**
 * Initialize the RC4 state.
 *
 * @param [inout] this "The RC4 state"
 * @param [in] key "The key to use"
 * @require key.len > 0 "The key must be at least 1 byte long"
 **/
fn void Rc4.init(Rc4* this, char[] key)
{
	// Init the state matrix
	foreach (char i, &c : this.state) *c = i;
	for (int i = 0, int j = 0; i < 256; i++)
	{
		j = (j + this.state[i] + key[i % key.len]) & 0xFF;
		@swap(this.state[i], this.state[j]);
	}
	this.i = 0;
	this.j = 0;
}

/**
 * Encrypt or decrypt a sequence of bytes.
 *
 * @param [inout] this "The RC4 State"
 * @param [in] in "The input"
 * @param [out] out "The output"
 * @require in.len <= out.len "Output would overflow"
 **/
fn void Rc4.crypt(Rc4* this, char[] in, char[] out)
{
	uint i = this.i;
	uint j = this.j;
	char* state = &this.state;
	isz len = in.len;
	foreach (idx, c : in)
	{
		i = (i + 1) & 0xFF;
		j = (j + state[i]) & 0xFF;
		@swap(state[i], state[j]);
		out[idx] = in[idx] ^ state[(state[i] + state[j]) & 0xFF];
	}
	this.i = i;
	this.j = j;
}

/**
 * Clear the rc4 state.
 *
 * @param [out] this "The RC4 State"
 **/
fn void Rc4.destroy(Rc4* this)
{
	*this = {};
}
