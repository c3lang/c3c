module std::hash::hmac(<HashAlg, HASH_BYTES, BLOCK_BYTES>);

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

	buffer = {};
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
