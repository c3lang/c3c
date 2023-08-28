module std::math;

struct SimpleRandom
{
	inline Random random;
	SimpleRandomState state;
}

def SimpleRandomState = distinct ulong;

const RandomInterface SIMPLE_RANDOM_INTERFACE @local = {
	.seed_fn = (RandomSeedFn)&SimpleRandom.set_seed,
	.next_bytes_fn = (RandomNextBytesFn)&SimpleRandom.next_bytes,
};

fn void SimpleRandom.init(&self)
{
	self.random.fns = &SIMPLE_RANDOM_INTERFACE;
}

/**
 * @require bytes.len > 0
 **/
fn void SimpleRandom.next_bytes(&self, char[] bytes)
{
	@random_value_to_bytes(self.state.next_int, bytes);
}

fn void SimpleRandom.set_seed(&self, char[] seed)
{
	char[8] full;
	foreach (i, c : seed)
	{
		full[i % 8] ^= c;
	}
	self.state.set_seed(bitcast(full, ulong));
}

fn void SimpleRandomState.set_seed(&self, ulong seed)
{
	*self = (SimpleRandomState)(seed ^ SIMPLE_RANDOM_MULTIPLIER) & SIMPLE_RANDOM_MASK;
}

fn uint SimpleRandomState.next_int(&self)
{
	ulong nextseed = ((ulong)*self * SIMPLE_RANDOM_MULTIPLIER + SIMPLE_RANDOM_ADDEND) & SIMPLE_RANDOM_MASK;
	*self = (SimpleRandomState)nextseed;
	return (uint)(nextseed >> (48 - 32));
}

const long SIMPLE_RANDOM_MULTIPLIER @local = 0x5DEECE66D;
const long SIMPLE_RANDOM_ADDEND @local = 0xB;
const long SIMPLE_RANDOM_MASK @local = (1u64 << 48) - 1;
