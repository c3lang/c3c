module std::hash::murmur3;

<*
 @param [in] data : "The data to hash"
 @param seed : "The seed to use for hashing"
 @require (data.len / 4) <= int.max : "Too much data"
*>
fn uint hash32(char[] data, uint seed)
{
	int nblocks = (int)data.len / 4;
	uint h1 = seed;

	const uint C1 = 0xcc9e2d51;
	const uint C2 = 0x1b873593;

	uint* blocks = (uint *)(data.ptr + nblocks * 4);

	for (int i = -nblocks; i != 0; i++)
    {
		uint k1 = getblock32(blocks, i);

		k1 *= C1;
		k1 = k1.rotl(15);
		k1 *= C2;

		h1 ^= k1;
		h1 = h1.rotl(13);
		h1 = h1 * 5U + 0xe6546b64;
	}

	char* tail = data.ptr + nblocks * 4;

	uint k1;

	switch (data.len & 3)
	{
		case 3: k1 ^= tail[2] << 16; nextcase;
		case 2: k1 ^= tail[1] << 8; nextcase;
		case 1: k1 ^= tail[0]; k1 *= C1; k1 = k1.rotl(15); k1 *= C2; h1 ^= k1;
    }

	h1 ^= (uint)data.len;

	h1 = fmix32(h1);

	return h1;
}

<*
 @param [in] data : "The data to hash"
 @param seed : "The seed to use for hashing"
 @require (data.len / 16) <= int.max : "Too much data"

*>
fn uint128 hash128_64(char[] data, uint seed)
{
	ulong len = data.len;
	int nblocks = (int)(len / 16);

	ulong h1 = seed;
	ulong h2 = seed;

    const ulong C1 = 0x87c37b91114253d5UL;
    const ulong C2 = 0x4cf5ad432745937fUL;

	ulong* blocks = (ulong*)data.ptr; // Unaligned!

    for (int i = 0; i < nblocks; i++)
    {
		ulong k1 = getblock64(blocks, i * 2 + 0);
		ulong k2 = getblock64(blocks, i * 2 + 1);

		k1 *= C1; k1 = k1.rotl(31); k1 *= C2; h1 ^= k1;

		h1 = h1.rotl(27); h1 += h2; h1 = h1 * 5U + 0x52dce729;

		k2 *= C2; k2 = k2.rotl(33); k2 *= C1; h2 ^= k2;

		h2 = h2.rotl(31); h2 += h1; h2 = h2 * 5U + 0x38495ab5;
	}

	char* tail = data.ptr + nblocks * 16;
	ulong k1, k2;

    switch (len & 15)
    {
		case 15: k2 ^= ((ulong)tail[14]) << 48; nextcase;
		case 14: k2 ^= ((ulong)tail[13]) << 40; nextcase;
		case 13: k2 ^= ((ulong)tail[12]) << 32; nextcase;
		case 12: k2 ^= ((ulong)tail[11]) << 24; nextcase;
		case 11: k2 ^= ((ulong)tail[10]) << 16; nextcase;
		case 10: k2 ^= ((ulong)tail[ 9]) << 8;  nextcase;
		case  9: k2 ^= ((ulong)tail[ 8]) << 0;
			k2 *= C2; k2 = k2.rotl(33); k2 *= C1; h2 ^= k2;
			nextcase;
		case  8: k1 ^= ((ulong)tail[ 7]) << 56; nextcase;
		case  7: k1 ^= ((ulong)tail[ 6]) << 48; nextcase;
		case  6: k1 ^= ((ulong)tail[ 5]) << 40; nextcase;
		case  5: k1 ^= ((ulong)tail[ 4]) << 32; nextcase;
		case  4: k1 ^= ((ulong)tail[ 3]) << 24; nextcase;
		case  3: k1 ^= ((ulong)tail[ 2]) << 16; nextcase;
		case  2: k1 ^= ((ulong)tail[ 1]) << 8;  nextcase;
		case  1: k1 ^= ((ulong)tail[ 0]) << 0;
			k1 *= C1; k1 = k1.rotl(31); k1 *= C2; h1 ^= k1;
	}


	h1 ^= len;
	h2 ^= len;

	h1 += h2;
	h2 += h1;

	h1 = fmix64(h1);
	h2 = fmix64(h2);


	h1 += h2;
	h2 += h1;

	return h1 + (uint128)h2 << 64U;
}

<*
 @param [in] data : "The data to hash"
 @param seed : "The seed to use for hashing"
 @require data.len <= uint.max : "Too much data"
*>
fn uint128 hash128_32(char[] data, uint seed)
{
	uint len = data.len;
	int nblocks = (int)(len / 16);

	uint h1 = seed;
	uint h2 = seed;
	uint h3 = seed;
	uint h4 = seed;

	const uint C1 = 0x239b961b;
	const uint C2 = 0xab0e9789;
	const uint C3 = 0x38b34ae5;
	const uint C4 = 0xa1e38b93;

	uint* blocks = (uint *)(data.ptr + nblocks * 16);

	for (int i = -nblocks; i != 0; i++)
	{
		uint k1 = getblock32(blocks, i * 4 + 0);
		uint k2 = getblock32(blocks, i * 4 + 1);
		uint k3 = getblock32(blocks, i * 4 + 2);
		uint k4 = getblock32(blocks, i * 4 + 3);

		k1 *= C1; k1 = k1.rotl(15); k1 *= C2; h1 ^= k1;
		h1 = h1.rotl(19); h1 += h2; h1 = h1 * 5U + 0x561ccd1b;
		k2 *= C2; k2 = k2.rotl(16); k2 *= C3; h2 ^= k2;
		h2 = h2.rotl(17); h2 += h3; h2 = h2 * 5U + 0x0bcaa747;
		k3 *= C3; k3 = k3.rotl(17); k3 *= C4; h3 ^= k3;
		h3 = h3.rotl(15); h3 += h4; h3 = h3 * 5U + 0x96cd1c35;
		k4 *= C4; k4 = k4.rotl(18); k4 *= C1; h4 ^= k4;
		h4 = h4.rotl(13); h4 += h1; h4 = h4 * 5U + 0x32ac3b17;
	}

	char* tail = data.ptr + nblocks * 16;

	uint k1, k2, k3, k4;

	switch (len & 15)
	{
		case 15: k4 ^= tail[14] << 16; nextcase;
		case 14: k4 ^= tail[13] << 8;  nextcase;
		case 13: k4 ^= tail[12] << 0;
			k4 *= C4; k4 = k4.rotl(18); k4 *= C1; h4 ^= k4;
			nextcase;
		case 12: k3 ^= tail[11] << 24; nextcase;
		case 11: k3 ^= tail[10] << 16; nextcase;
		case 10: k3 ^= tail[ 9] << 8;  nextcase;
		case  9: k3 ^= tail[ 8] << 0;
			k3 *= C3; k3 = k3.rotl(17); k3 *= C4; h3 ^= k3;
			nextcase;
		case  8: k2 ^= tail[ 7] << 24; nextcase;
		case  7: k2 ^= tail[ 6] << 16; nextcase;
		case  6: k2 ^= tail[ 5] << 8;  nextcase;
		case  5: k2 ^= tail[ 4] << 0;
			k2 *= C2; k2 = k2.rotl(16); k2 *= C3; h2 ^= k2;
			nextcase;
		case  4: k1 ^= tail[ 3] << 24; nextcase;
		case  3: k1 ^= tail[ 2] << 16; nextcase;
		case  2: k1 ^= tail[ 1] << 8;  nextcase;
		case  1: k1 ^= tail[ 0] << 0;
			k1 *= C1; k1 = k1.rotl(15); k1 *= C2; h1 ^= k1;
	}

	h1 ^= len; h2 ^= len; h3 ^= len; h4 ^= len;

	h1 += h2; h1 += h3; h1 += h4;
	h2 += h1; h3 += h1; h4 += h1;

	h1 = fmix32(h1);
	h2 = fmix32(h2);
	h3 = fmix32(h3);
	h4 = fmix32(h4);

	h1 += h2; h1 += h3; h1 += h4;
	h2 += h1; h3 += h1; h4 += h1;

	return h1 + (uint128)h2 << 32U + (uint128)h3 << 64U + (uint128)h4 << 96U;
}

macro uint getblock32(uint* p, int i) @local
{
	UIntLE* p_le = (UIntLE*)p + i;
	return mem::load(p_le, 1).val;
}

macro ulong getblock64(ulong* p, int i) @local
{
	ULongLE* p_le = (ULongLE*)p + i;
	return mem::load(p_le, 1).val;
}

macro uint fmix32(uint h) @local
{
	h ^= h >> 16UL;
	h *= 0x85ebca6b;
	h ^= h >> 13UL;
	h *= 0xc2b2ae35;
	h ^= h >> 16UL;

	return h;
}


macro ulong fmix64(ulong k) @local
{
	k ^= k >> 33U;
	k *= 0xff51afd7ed558ccd;
	k ^= k >> 33U;
	k *= 0xc4ceb9fe1a85ec53;
	k ^= k >> 33U;

	return k;
}

