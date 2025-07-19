// Copyright (c) 2025 Zack Puhl <github@xmit.xyz>. All rights reserved.
// Use of this source code is governed by the MIT license
// a copy of which can be found in the LICENSE_STDLIB file.
//
// MetroHash64 and MetroHash128 are different enough to warrant their own
//   modules, and there would be no reason to create a generic module just
//   for the two. If you inspect the differences, the only shared portion
//   of the entire process is the `update` method.
//
module std::hash::metro64;


const ulong[4] K @local = {
	0xd6d018f5,
	0xa2aa033b,
	0x62992fc1,
	0x30bc5b29,
};


struct MetroHash64
{
	union
	{
		ulong[4] state;
		ulong result;
	}
	union
	{
		ulong[4] stomach_64;
		char[32] stomach;
	}
	ulong bytes;
	ulong vseed;
}


fn ulong hash(char[] data, ulong seed = 0)
{
	MetroHash64 m;
	m.init(seed);
	m.update(data);
	return m.final();
}


fn void MetroHash64.init(&self, ulong seed = 0)
{
	self.vseed = (seed + K[2]) * K[0];

	self.state[0] = self.vseed;
	self.state[1] = self.vseed;
	self.state[2] = self.vseed;
	self.state[3] = self.vseed;
}


fn void MetroHash64.update(&self, char[] data)
{
	if (self.bytes % 32)   // partial buffer
	{
		ulong to_fill = min(data.len, (32 - (self.bytes % 32)));

		self.stomach[(self.bytes % 32):to_fill] = data[:to_fill];

		data = data[to_fill..];
		self.bytes += to_fill;

		if (self.bytes % 32) return;   // still awaiting more input, or final

		self.state[0] += self.stomach_64[0] * K[0]; self.state[0] = self.state[0].rotr(29) + self.state[2];
		self.state[1] += self.stomach_64[1] * K[1]; self.state[1] = self.state[1].rotr(29) + self.state[3];
		self.state[2] += self.stomach_64[2] * K[2]; self.state[2] = self.state[2].rotr(29) + self.state[0];
		self.state[3] += self.stomach_64[3] * K[3]; self.state[3] = self.state[3].rotr(29) + self.state[1];
	}

	self.bytes += data.len;

	for (; data.len >= 32; data = data[32:^32])
	{
		self.state[0] += @unaligned_load(((ulong*)data.ptr)[0], 1) * K[0]; self.state[0] = self.state[0].rotr(29) + self.state[2];
		self.state[1] += @unaligned_load(((ulong*)data.ptr)[1], 1) * K[1]; self.state[1] = self.state[1].rotr(29) + self.state[3];
		self.state[2] += @unaligned_load(((ulong*)data.ptr)[2], 1) * K[2]; self.state[2] = self.state[2].rotr(29) + self.state[0];
		self.state[3] += @unaligned_load(((ulong*)data.ptr)[3], 1) * K[3]; self.state[3] = self.state[3].rotr(29) + self.state[1];
	}

	// Gobble up the leftover bytes. Nom nom.
	if (data.len > 0) self.stomach[:data.len] = data[..];
}


fn ulong MetroHash64.final(&self)
{
	if (self.bytes >= 32)
	{
		self.state[2] ^= (((self.state[0] + self.state[3]) * K[0]) + self.state[1]).rotr(37) * K[1];
		self.state[3] ^= (((self.state[1] + self.state[2]) * K[1]) + self.state[0]).rotr(37) * K[0];
		self.state[0] ^= (((self.state[0] + self.state[2]) * K[0]) + self.state[3]).rotr(37) * K[1];
		self.state[1] ^= (((self.state[1] + self.state[3]) * K[1]) + self.state[2]).rotr(37) * K[0];

		self.state[0] = self.vseed + (self.state[0] ^ self.state[1]);
	}

	char[] final_data = self.stomach[:(self.bytes % 32)];

	if (final_data.len >= 16)
	{
		self.state[1] = self.state[0] + @unaligned_load(((ulong*)final_data.ptr)[0], 1) * K[2]; self.state[1] = self.state[1].rotr(29) * K[3];
		self.state[2] = self.state[0] + @unaligned_load(((ulong*)final_data.ptr)[1], 1) * K[2]; self.state[2] = self.state[2].rotr(29) * K[3];
		self.state[1] ^= (self.state[1] * K[0]).rotr(21) + self.state[2];
		self.state[2] ^= (self.state[2] * K[3]).rotr(21) + self.state[1];
		self.state[0] += self.state[2];

		final_data = final_data[16:^16];
	}

	if (final_data.len >= 8)
	{
		self.state[0] += @unaligned_load(((ulong*)final_data.ptr)[0], 1) * K[3];
		self.state[0] ^= self.state[0].rotr(55) * K[1];

		final_data = final_data[8:^8];
	}

	if (final_data.len >= 4)
	{
		self.state[0] += @unaligned_load(((uint*)final_data.ptr)[0], 1) * K[3];
		self.state[0] ^= self.state[0].rotr(26) * K[1];

		final_data = final_data[4:^4];
	}

	if (final_data.len >= 2)
	{
		self.state[0] += @unaligned_load(((ushort*)final_data.ptr)[0], 1) * K[3];
		self.state[0] ^= self.state[0].rotr(48) * K[1];

		final_data = final_data[2:^2];
	}

	if (final_data.len >= 1)
	{
		self.state[0] += ((char*)final_data.ptr)[0] * K[3];
		self.state[0] ^= self.state[0].rotr(37) * K[1];
	}

	self.state[0] ^= self.state[0].rotr(28);
	self.state[0] *= K[0];
	self.state[0] ^= self.state[0].rotr(29);

	return self.result;
}
