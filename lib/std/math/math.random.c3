module std::math;

struct Random
{
	RandomInterface fns;
	void* state;
}

typedef RandomSeedFn = fn void(Random*, char[] seed);
typedef RandomNextBytesFn = fn void(Random*, char[] buffer);
typedef RandomNextFn = fn uint(Random*, int bits);

struct RandomInterface
{
	RandomSeedFn seed_fn;
	RandomNextBytesFn next_bytes_fn;
	RandomNextFn next_fn;
}

/**
 * @param [&inout] random
 * @param [inout] buffer
 **/
fn void Random.next_bytes(Random* random, char[] buffer)
{
	if (!buffer.len) return;
	if (RandomNextBytesFn func = random.fns.next_bytes_fn)
	{
		func(random, buffer);
		return;
	}
	if (RandomNextFn func = random.fns.next_fn)
	{
		usz current = 0;
		while (current < buffer.len)
		{
			char[4] res = bitcast(func(random, 32), char[4]);
			foreach (c : res)
			{
				buffer[current++] = c;
				if (current == buffer.len) return;
			}
		}
		return;
	}
	unreachable("Invalid Random type.");
}

/**
 * @param [&inout] random
 * @require bits >= 0 && bits <= 32
 **/
fn uint Random.next(Random* random, int bits)
{
	if (bits == 0) return 0;
	if (RandomNextFn func = random.fns.next_fn) return func(random, bits);
	int bytes = (bits + 7) / 8;
	char[4] buffer;
	char[] b = buffer[:bytes];
	assert(random.fns.next_bytes_fn, "Invalid Random");
	random.fns.next_bytes_fn(random, b) @inline;
	uint next = 0;
	foreach (char c : b)
	{
		next = next << 8 + c;
	}
	if (bits < 32) next >>= bytes * 8 - bits;
	return next;
}

/**
 * @param [&inout] random
 **/
fn ulong Random.next_long(Random* random)
{
	char[8] buffer;
	random.next_bytes(&buffer);
	return bitcast(buffer, ulong);
}

/**
 * @param [&inout] random
 **/
fn void Random.set_seed(Random* random, long seed)
{
	random.fns.seed_fn(random, &&bitcast(seed, char[8])) @inline;
}

/**
 * @param [&inout] random
 * @param [in] seed
 **/
fn void Random.set_seeds(Random* random, char[] seed)
{
	random.fns.seed_fn(random, seed);
}

/**
 * @param [&inout] random
 **/
fn bool Random.next_bool(Random* random)
{
    return random.next(1) != 0;
}

fn float Random.next_float(Random* r)
{
    return r.next(24) / (float)(1 << 24);
}

fn double Random.next_double(Random* r)
{
    return (((long)(r.next(26)) << 27) + r.next(27)) * 0x1.0p-53;
}

fn uint Random.next_int(Random* r)
{
    return r.next(32) @inline;
}
