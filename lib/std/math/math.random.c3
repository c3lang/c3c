module std::math::random;

interface Random
{
	fn void set_seed(char[] input);
	fn char next_byte();
	fn ushort next_short();
	fn uint next_int();
	fn ulong next_long();
	fn uint128 next_int128();
	fn void next_bytes(char[] buffer);
}

macro bool is_random(random) => $assignable(random, Random*);

/**
 * @require is_random(random)
 **/
macro void seed(random, seed)
{
	random.set_seed(@as_char_view(seed));
}

macro void seed_entropy(random)
{
	random.set_seed(&&entropy());
}

/**
 * @require is_random(random)
 **/
macro int next(random, int max)
{
	return (int)(next_double(random) * max);
}

fn int rand(int max) @builtin
{
	tlocal Sfc64Random default_random;
	tlocal bool initialized = false;
	if (!initialized)
	{
		seed_entropy(&default_random);
		initialized = true;
	}
	return next(&default_random, max);
}
/**
 * @require is_random(random)
 **/
macro void next_bool(random)
{
	return random.next_byte() & 1;
}

/**
 * @require is_random(random)
 **/
macro float next_float(random)
{
	uint val = random.next_int() & (1 << 24 - 1);
	return val / (float)(1 << 24);
}

/**
 * @require is_random(random)
 **/
macro double next_double(random)
{
	ulong val = random.next_long() & (1UL << 53 - 1);
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