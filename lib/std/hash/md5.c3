module std::hash::md5;
import std::hash::hmac;
import std::bits;

const BLOCK_BYTES = 64;
const HASH_BYTES = 16;

struct Md5
{
	uint lo, hi;
	uint a, b, c, d;
	char[64] buffer;
	uint[16] block;
}

alias HmacMd5 = Hmac{Md5, HASH_BYTES, BLOCK_BYTES};
alias hmac = hmac::hash{Md5, HASH_BYTES, BLOCK_BYTES};
alias pbkdf2 = hmac::pbkdf2{Md5, HASH_BYTES, BLOCK_BYTES};

fn char[HASH_BYTES] hash(char[] data)
{
	Md5 md5;
	md5.init();
	md5.update(data);
	return md5.final();
}

fn void Md5.init(&self)
{
	self.a = 0x67452301;
	self.b = 0xefcdab89;
	self.c = 0x98badcfe;
	self.d = 0x10325476;

	self.lo = 0;
	self.hi = 0;
}


fn void Md5.update(&ctx, char[] data)
{
	uint saved_lo = ctx.lo;
	if ((ctx.lo = (saved_lo + data.len) & 0x1fffffff) < saved_lo) ctx.hi++;
	ctx.hi += data.len >> 29;

	usz used = (usz)saved_lo & 0x3f;

	if (used)
	{
		usz available = 64 - used;

		if (data.len < available)
		{
			ctx.buffer[used:data.len] = data[..];
			return;
		}
		ctx.buffer[used:available] = data[:available];
		data = data[available..];
		body(ctx, &ctx.buffer, 64);
	}

	if (data.len >= 64)
	{
		data = body(ctx, data, data.len & ~(usz)0x3f)[:data.len & 0x3f];
	}
	ctx.buffer[:data.len] = data[..];
}

fn char[HASH_BYTES] Md5.final(&ctx)
{
	usz used = (usz)ctx.lo & 0x3f;
	ctx.buffer[used++] = 0x80;

	usz available = 64 - used;

	if (available < 8)
	{
		ctx.buffer[used:available] = 0;
		body(ctx, &ctx.buffer, 64);
		used = 0;
		available = 64;
	}
	ctx.buffer[used:available - 8] = 0;

	ctx.lo <<= 3;
	ctx.buffer[56:4] = bitcast(ctx.lo, char[4])[..];
	ctx.buffer[60:4] = bitcast(ctx.hi, char[4])[..];

	body(ctx, &ctx.buffer, 64);

	char[16] res @noinit;
	res[0:4] = bitcast(ctx.a, char[4])[..];
	res[4:4] = bitcast(ctx.b, char[4])[..];
	res[8:4] = bitcast(ctx.c, char[4])[..];
	res[12:4] = bitcast(ctx.d, char[4])[..];
	*ctx = {};
	return res;
}

module std::hash::md5 @private;

const uint[64] MD5_T @private = {
	0xd76aa478, 0xe8c7b756, 0x242070db, 0xc1bdceee,
	0xf57c0faf, 0x4787c62a, 0xa8304613, 0xfd469501,
	0x698098d8, 0x8b44f7af, 0xffff5bb1, 0x895cd7be,
	0x6b901122, 0xfd987193, 0xa679438e, 0x49b40821,
	0xf61e2562, 0xc040b340, 0x265e5a51, 0xe9b6c7aa,
	0xd62f105d, 0x02441453, 0xd8a1e681, 0xe7d3fbc8,
	0x21e1cde6, 0xc33707d6, 0xf4d50d87, 0x455a14ed,
	0xa9e3e905, 0xfcefa3f8, 0x676f02d9, 0x8d2a4c8a,
	0xfffa3942, 0x8771f681, 0x6d9d6122, 0xfde5380c,
	0xa4beea44, 0x4bdecfa9, 0xf6bb4b60, 0xbebfbc70,
	0x289b7ec6, 0xeaa127fa, 0xd4ef3085, 0x04881d05,
	0xd9d4d039, 0xe6db99e5, 0x1fa27cf8, 0xc4ac5665,
	0xf4292244, 0x432aff97, 0xab9423a7, 0xfc93a039,
	0x655b59c3, 0x8f0ccc92, 0xffeff47d, 0x85845dd1,
	0x6fa87e4f, 0xfe2ce6e0, 0xa3014314, 0x4e0811a1,
	0xf7537e82, 0xbd3af235, 0x2ad7d2bb, 0xeb86d391
};

const int[16] MD5_S @private = {
	7, 12, 17, 22,
	5,  9, 14, 20,
	4, 11, 16, 23,
	6, 10, 15, 21
};

// Implementation
macro @f(x, y, z) => z ^ (x & (y ^ z));
macro @g(x, y, z) => y ^ (z & (x ^ y));
macro @h(x, y, z) => (x ^ y) ^ z;
macro @h2(x, y, z) => x ^ (y ^ z);
macro @i(x, y, z) => y ^ (x | ~z);

macro void @step(#f, a, b, c, d, ptr, n, t, s)
{
	*a += #f(b, c, d) + mem::load((uint *)&ptr[n * 4], 2) + t;
	*a = (*a << s) | ((*a & 0xffffffff) >> (32 - s));
	*a += b;
}


fn char* body(Md5* ctx, void* data, usz size)
{
	char* ptr = data;
	uint a = ctx.a;
	uint b = ctx.b;
	uint c = ctx.c;
	uint d = ctx.d;

	do
	{
		uint saved_a = a;
		uint saved_b = b;
		uint saved_c = c;
		uint saved_d = d;

		/* Round 1 */
		for (int i = 0; i < 16; i += 4)
		{
			@step(@f, &a, b, c, d, ptr, i + 0, MD5_T[i + 0], MD5_S[0]);
			@step(@f, &d, a, b, c, ptr, i + 1, MD5_T[i + 1], MD5_S[1]);
			@step(@f, &c, d, a, b, ptr, i + 2, MD5_T[i + 2], MD5_S[2]);
			@step(@f, &b, c, d, a, ptr, i + 3, MD5_T[i + 3], MD5_S[3]);
		}

		/* Round 2 */
		for (int i = 0; i < 16; i += 4)
		{
			@step(@g, &a, b, c, d, ptr, (1 + 5 * (i + 0)) % 16, MD5_T[16 + i + 0], MD5_S[4]);
			@step(@g, &d, a, b, c, ptr, (1 + 5 * (i + 1)) % 16, MD5_T[16 + i + 1], MD5_S[5]);
			@step(@g, &c, d, a, b, ptr, (1 + 5 * (i + 2)) % 16, MD5_T[16 + i + 2], MD5_S[6]);
			@step(@g, &b, c, d, a, ptr, (1 + 5 * (i + 3)) % 16, MD5_T[16 + i + 3], MD5_S[7]);
		}

		/* Round 3 */
		for (int i = 0; i < 16; i += 4)
		{
			@step(@h, &a, b, c, d,  ptr, (5 + 3 * (i + 0)) % 16, MD5_T[32 + i + 0], MD5_S[8]);
			@step(@h, &d, a, b, c,  ptr, (5 + 3 * (i + 1)) % 16, MD5_T[32 + i + 1], MD5_S[9]);
			@step(@h, &c, d, a, b,  ptr, (5 + 3 * (i + 2)) % 16, MD5_T[32 + i + 2], MD5_S[10]);
			@step(@h, &b, c, d, a,  ptr, (5 + 3 * (i + 3)) % 16, MD5_T[32 + i + 3], MD5_S[11]);
		}

		/* Round 4 */
		for (int i = 0; i < 16; i += 4)
		{
			@step(@i, &a, b, c, d, ptr, (7 * (i + 0)) % 16, MD5_T[48 + i + 0], MD5_S[12]);
			@step(@i, &d, a, b, c, ptr, (7 * (i + 1)) % 16, MD5_T[48 + i + 1], MD5_S[13]);
			@step(@i, &c, d, a, b, ptr, (7 * (i + 2)) % 16, MD5_T[48 + i + 2], MD5_S[14]);
			@step(@i, &b, c, d, a, ptr, (7 * (i + 3)) % 16, MD5_T[48 + i + 3], MD5_S[15]);
		}

		a += saved_a;
		b += saved_b;
		c += saved_c;
		d += saved_d;

		ptr += 64;

	} while (size -= 64);

	ctx.a = a;
	ctx.b = b;
	ctx.c = c;
	ctx.d = d;

	return ptr;
}



