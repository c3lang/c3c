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

// Implementation
macro @f(x, y, z) => z ^ (x & (y ^ z));
macro @g(x, y, z) => y ^ (z & (x ^ y));
macro @h(x, y, z) => (x ^ y) ^ z;
macro @h2(x, y, z) => x ^ (y ^ z);
macro @i(x, y, z) => y ^ (x | ~z);

macro @step(#f, a, b, c, d, ptr, n, t, s)
{
	*a += #f(b, c, d) + @unaligned_load(*(uint *)&ptr[n * 4], 2) + t;
	*a = (*a << s) | ((*a & 0xffffffff) >> (32 - s));
	*a += b;
}


fn char* body(Md5* ctx, void* data, usz size)
{
	char* ptr;
	uint a, b, c, d;
	uint saved_a, saved_b, saved_c, saved_d;
	ptr = data;
	a = ctx.a;
	b = ctx.b;
	c = ctx.c;
	d = ctx.d;

	do
	{
		saved_a = a;
		saved_b = b;
		saved_c = c;
		saved_d = d;

/* Round 1 */
		@step(@f, &a, b, c, d, ptr, 0, 0xd76aa478, 7)  ;
		@step(@f, &d, a, b, c, ptr, 1, 0xe8c7b756, 12) ;
		@step(@f, &c, d, a, b, ptr, 2, 0x242070db, 17) ;
		@step(@f, &b, c, d, a, ptr, 3, 0xc1bdceee, 22) ;
		@step(@f, &a, b, c, d, ptr, 4, 0xf57c0faf, 7)  ;
		@step(@f, &d, a, b, c, ptr, 5, 0x4787c62a, 12) ;
		@step(@f, &c, d, a, b, ptr, 6, 0xa8304613, 17) ;
		@step(@f, &b, c, d, a, ptr, 7, 0xfd469501, 22) ;
		@step(@f, &a, b, c, d, ptr, 8, 0x698098d8, 7)  ;
		@step(@f, &d, a, b, c, ptr, 9, 0x8b44f7af, 12) ;
		@step(@f, &c, d, a, b, ptr, 10, 0xffff5bb1, 17);
		@step(@f, &b, c, d, a, ptr, 11, 0x895cd7be, 22);
		@step(@f, &a, b, c, d, ptr, 12, 0x6b901122, 7) ;
		@step(@f, &d, a, b, c, ptr, 13, 0xfd987193, 12);
		@step(@f, &c, d, a, b, ptr, 14, 0xa679438e, 17);
		@step(@f, &b, c, d, a, ptr, 15, 0x49b40821, 22);

/* Round 2 */
		@step(@g, &a, b, c, d, ptr, 1, 0xf61e2562, 5)  ;
		@step(@g, &d, a, b, c, ptr, 6, 0xc040b340, 9)  ;
		@step(@g, &c, d, a, b, ptr, 11, 0x265e5a51, 14);
		@step(@g, &b, c, d, a, ptr, 0, 0xe9b6c7aa, 20) ;
		@step(@g, &a, b, c, d, ptr, 5, 0xd62f105d, 5)  ;
		@step(@g, &d, a, b, c, ptr, 10, 0x02441453, 9) ;
		@step(@g, &c, d, a, b, ptr, 15, 0xd8a1e681, 14);
		@step(@g, &b, c, d, a, ptr, 4, 0xe7d3fbc8, 20) ;
		@step(@g, &a, b, c, d, ptr, 9, 0x21e1cde6, 5)  ;
		@step(@g, &d, a, b, c, ptr, 14, 0xc33707d6, 9) ;
		@step(@g, &c, d, a, b, ptr, 3, 0xf4d50d87, 14) ;
		@step(@g, &b, c, d, a, ptr, 8, 0x455a14ed, 20) ;
		@step(@g, &a, b, c, d, ptr, 13, 0xa9e3e905, 5) ;
		@step(@g, &d, a, b, c, ptr, 2, 0xfcefa3f8, 9)  ;
		@step(@g, &c, d, a, b, ptr, 7, 0x676f02d9, 14) ;
		@step(@g, &b, c, d, a, ptr, 12, 0x8d2a4c8a, 20);

/* Round 3 */
		@step(@h,  &a, b, c, d, ptr, 5, 0xfffa3942, 4);
		@step(@h2, &d, a, b, c, ptr, 8, 0x8771f681, 11);
		@step(@h,  &c, d, a, b, ptr, 11, 0x6d9d6122, 16);
		@step(@h2, &b, c, d, a, ptr, 14, 0xfde5380c, 23);
		@step(@h,  &a, b, c, d, ptr, 1, 0xa4beea44, 4);
		@step(@h2, &d, a, b, c, ptr, 4, 0x4bdecfa9, 11);
		@step(@h,  &c, d, a, b, ptr, 7, 0xf6bb4b60, 16);
		@step(@h2, &b, c, d, a, ptr, 10, 0xbebfbc70, 23);
		@step(@h,  &a, b, c, d, ptr, 13, 0x289b7ec6, 4)   ;
		@step(@h2, &d, a, b, c, ptr, 0, 0xeaa127fa, 11)  ;
		@step(@h,  &c, d, a, b, ptr, 3, 0xd4ef3085, 16)   ;
		@step(@h2, &b, c, d, a, ptr, 6, 0x04881d05, 23)  ;
		@step(@h,  &a, b, c, d, ptr, 9, 0xd9d4d039, 4)   ;
		@step(@h2, &d, a, b, c, ptr, 12, 0xe6db99e5, 11) ;
		@step(@h,  &c, d, a, b, ptr, 15, 0x1fa27cf8, 16)  ;
		@step(@h2, &b, c, d, a, ptr, 2, 0xc4ac5665, 23)  ;

/* Round 4 */
		@step(@i, &a, b, c, d, ptr, 0, 0xf4292244, 6)   ;
		@step(@i, &d, a, b, c, ptr, 7, 0x432aff97, 10)  ;
		@step(@i, &c, d, a, b, ptr, 14, 0xab9423a7, 15) ;
		@step(@i, &b, c, d, a, ptr, 5, 0xfc93a039, 21)  ;
		@step(@i, &a, b, c, d, ptr, 12, 0x655b59c3, 6)  ;
		@step(@i, &d, a, b, c, ptr, 3, 0x8f0ccc92, 10)  ;
		@step(@i, &c, d, a, b, ptr, 10, 0xffeff47d, 15) ;
		@step(@i, &b, c, d, a, ptr, 1, 0x85845dd1, 21)  ;
		@step(@i, &a, b, c, d, ptr, 8, 0x6fa87e4f, 6)   ;
		@step(@i, &d, a, b, c, ptr, 15, 0xfe2ce6e0, 10) ;
		@step(@i, &c, d, a, b, ptr, 6, 0xa3014314, 15)  ;
		@step(@i, &b, c, d, a, ptr, 13, 0x4e0811a1, 21) ;
		@step(@i, &a, b, c, d, ptr, 4, 0xf7537e82, 6)   ;
		@step(@i, &d, a, b, c, ptr, 11, 0xbd3af235, 10) ;
		@step(@i, &c, d, a, b, ptr, 2, 0x2ad7d2bb, 15)  ;
		@step(@i, &b, c, d, a, ptr, 9, 0xeb86d391, 21)  ;

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



