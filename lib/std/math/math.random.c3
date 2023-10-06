module std::math::random;

protocol Random
{
	fn void set_seed(&self, char[] input);
	fn char next_byte(&self);
	fn ushort next_short(&self);
	fn uint next_int(&self);
	fn ulong next_long(&self);
	fn uint128 next_int128(&self);
	fn void next_bytes(&self, char[] buffer);
}

macro Random.seed(&self, seed)
{
	self.set_seed(@as_char_view(seed));
}

fn int Random.next(&random, int max)
{
	return (int)(random.next_double() * max);
}

fn bool Random.next_bool(&self)
{
	return self.next_byte() & 1 == 0;
}

fn float Random.next_float(&self)
{
	uint val = self.next_int() & (1 << 24 - 1);
	return val / (float)(1 << 24);
}

fn double Random.next_double(&self)
{
	ulong val = self.next_long() & (1UL << 53 - 1);
	return val * 0x1.0p-53;
}

macro uint128 @long_to_int128(#function) => (uint128)#function << 64 + #function;
macro ulong @int_to_long(#function) => (ulong)#function << 32 + #function;
macro uint @short_to_int(#function) => (uint)#function << 16 + #function;
macro ushort @char_to_short(#function) => (ushort)#function << 8 + #function;

macro @random_value_to_bytes(#function, char[] bytes)
{
	var $byte_size = $sizeof(#function());
	usz len = bytes.len;
	// Same size or smaller? Then just copy.
	while (len > 0)
	{
		var value = #function();
		if (len <= $byte_size)
		{
			bytes[..] = ((char*)&value)[:len];
			return;
		}
		bytes[:$byte_size] = ((char*)&value)[:$byte_size];
		len -= $byte_size;
		bytes = bytes[$byte_size..];
	}
	unreachable();
}