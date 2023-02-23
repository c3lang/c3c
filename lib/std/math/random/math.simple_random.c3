module std::math;

typedef SimpleRandom = distinct ulong;

const long SIMPLE_RANDOM_MULTIPLIER @local = 0x5DEECE66D;
const long SIMPLE_RANDOM_ADDEND @local = 0xB;
const long SIMPLE_RANDOM_MASK @local = (1u64 << 48) - 1;

RandomInterface simple_random_interface = {
	.seed_fn = fn (random, seed) => ((SimpleRandom*)random.state).set_seeds(seed),
	.next_fn = fn (random, bits) => ((SimpleRandom*)random.state).next(bits)
};

fn Random SimpleRandom.as_random(SimpleRandom* random)
{
	return { .fns = simple_random_interface, .state = random };
}

fn uint SimpleRandom.next(SimpleRandom* r, int bits)
{
    ulong nextseed = ((ulong)*r * SIMPLE_RANDOM_MULTIPLIER + SIMPLE_RANDOM_ADDEND) & SIMPLE_RANDOM_MASK;
    *r = (SimpleRandom)nextseed;
    return (uint)(nextseed >> (48 - bits));
}

fn void SimpleRandom.set_seed(SimpleRandom* r, ulong seed)
{
    *r = (SimpleRandom)((seed ^ SIMPLE_RANDOM_MULTIPLIER) & SIMPLE_RANDOM_MASK);
}
fn void SimpleRandom.set_seeds(SimpleRandom* r, char[] seed)
{
	char[8] full;
	foreach (i, c : seed)
	{
		full[i % 8] ^= c;
	}
	r.set_seed(bitcast(full, ulong));
}
