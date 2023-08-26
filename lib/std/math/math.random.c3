module std::math;

struct Random
{
	RandomInterface* fns;
}

def RandomSeedFn = fn void(Random*, char[] seed);
def RandomNextBytesFn = fn void(Random*, char[] buffer);

struct RandomInterface
{
	RandomSeedFn seed_fn;
	RandomNextBytesFn next_bytes_fn;
}

/**
 * @param [&inout] random
 * @param [inout] buffer
 **/
fn void Random.next_bytes(&random, char[] buffer)
{
	if (!buffer.len) return;
	random.fns.next_bytes_fn(random, buffer);
}

/**
 * @param [&inout] self
 **/
fn char Random.next_byte(&self)
{
	char result @noinit;
	self.next_bytes(((char*)&result)[:1]);
	return result;
}

/**
 * @param [&inout] self
 **/
fn uint Random.next_short(&self)
{
	ushort result @noinit;
	self.next_bytes(((char*)&result)[:2]);
	return result;
}

/**
 * @param [&inout] self
 **/
fn uint Random.next_int(&self)
{
	uint result @noinit;
	self.next_bytes(((char*)&result)[:4]);
	return result;
}

/**
 * @param [&inout] self
 **/
fn ulong Random.next_long(&self)
{
	ulong result @noinit;
	self.next_bytes(((char*)&result)[:8]);
	return result;
}

/**
 * @param [&inout] self
 **/
fn uint128 Random.next_uint128(&self)
{
	uint128 result @noinit;
	self.next_bytes(((char*)&result)[:16]);
	return result;
}

/**
 * @param [&inout] self
 * @require types::is_numerical($typeof(seed))
 **/
macro void Random.seed_random(&self, seed)
{
	self.fns.seed_fn(self, @as_char_view(seed)) @inline;
}

/**
 * @param [&inout] random
 * @param [in] seed
 **/
fn void Random.set_seed(&random, char[] seed)
{
	random.fns.seed_fn(random, seed);
}

fn int Random.next(&random, int max)
{
	return (int)(random.next_double() * max);
}

/**
 * @param [&inout] self
 **/
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