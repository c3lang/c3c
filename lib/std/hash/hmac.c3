module std::hash::hmac{HashAlg, HASH_BYTES, BLOCK_BYTES};
import std::crypto;

struct Hmac
{
	HashAlg a, b;
}

fn char[HASH_BYTES] hash(char[] key, char[] message)
{
	Hmac hmac @noinit;
	hmac.init(key);
	hmac.update(message);
	return hmac.final();
}

<*
 @require output.len > 0 : "Output must be greater than zero"
 @require output.len < int.max / HASH_BYTES : "Output is too large"
*>
fn void pbkdf2(char[] pw, char[] salt, uint iterations, char[] output)
{
	usz l = output.len / HASH_BYTES;
	usz r = output.len % HASH_BYTES;

	Hmac hmac;
	hmac.init(pw);

	char[] dst_curr = output;
	for (usz i = 1; i <= l; i++)
	{
		@derive(&hmac, salt, iterations, i, dst_curr[:HASH_BYTES]);
		dst_curr = dst_curr[HASH_BYTES..];
	}

	if (r > 0)
	{
		char[HASH_BYTES] tmp;
		@derive(&hmac, salt, iterations, l + 1, &tmp);
		dst_curr[..] = tmp[:dst_curr.len];
		mem::zero_volatile(&tmp);
	}
}

fn void Hmac.init(&self, char[] key)
{
	char[BLOCK_BYTES] buffer;
	if (key.len > BLOCK_BYTES)
	{
		self.a.init();
		self.a.update(key);
		buffer[:HASH_BYTES] = self.a.final()[..];
	}
	else
	{
		buffer[:key.len] = key[..];
	}

	foreach (&b : buffer) *b ^= IPAD;

	self.a.init();
	self.a.update(&buffer);

	foreach (&b : buffer) *b ^= IPAD ^ OPAD;

	self.b.init();
	self.b.update(&buffer);

	mem::zero_volatile(&buffer);
}

fn void Hmac.update(&self, char[] data)
{
	self.a.update(data);
}

fn char[HASH_BYTES] Hmac.final(&self)
{
	self.b.update(&&self.a.final());
	return self.b.final();
}

const IPAD @private = 0x36;
const OPAD @private = 0x5C;

macro @derive(Hmac *hmac_start, char[] salt, uint iterations, usz index, char[] out)
{
	assert(out.len == HASH_BYTES);
	char[HASH_BYTES] tmp @noinit;
	defer mem::zero_volatile(&tmp);
	Hmac hmac  = *hmac_start;
	hmac.update(salt);
	UIntBE be = { (uint)index };
	hmac.update(&&bitcast(be, char[4]));
	tmp = hmac.final();
	out[..] = tmp[..];
	for (int it = 1; it < iterations; it++)
	{
		hmac = *hmac_start;
		hmac.update(&tmp);
		tmp = hmac.final();
		foreach (i, v : tmp)
		{
			out[i] ^= v;
		}
	}
}