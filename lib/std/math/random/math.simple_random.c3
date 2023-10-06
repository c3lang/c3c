module std::math::random;

def SimpleRandom = distinct ulong;


fn void SimpleRandom.set_seed(&self, char[] seed) : Random
{
	char[8] full;
	foreach (i, c : seed)
	{
		full[i % 8] ^= c;
	}
	*self = (SimpleRandom)(bitcast(full, ulong) ^ SIMPLE_RANDOM_MULTIPLIER) & SIMPLE_RANDOM_MASK;
}

fn uint SimpleRandom.next_int(&self) : Random
{
	ulong nextseed = ((ulong)*self * SIMPLE_RANDOM_MULTIPLIER + SIMPLE_RANDOM_ADDEND) & SIMPLE_RANDOM_MASK;
	*self = (SimpleRandom)nextseed;
	return (uint)(nextseed >> (48 - 32));
}

/**
 * @require bytes.len > 0
 **/
fn void SimpleRandom.next_bytes(&self, char[] bytes) : Random => @random_value_to_bytes(self.next_int, bytes);
fn uint128 SimpleRandom.next_int128(&self) : Random => @long_to_int128(self.next_long());
fn ulong SimpleRandom.next_long(&self) : Random => @int_to_long(self.next_int());
fn ushort SimpleRandom.next_short(&self) : Random => (ushort)self.next_int();
fn char SimpleRandom.next_byte(&self) : Random => (char)self.next_int();

const long SIMPLE_RANDOM_MULTIPLIER @local = 0x5DEECE66D;
const long SIMPLE_RANDOM_ADDEND @local = 0xB;
const long SIMPLE_RANDOM_MASK @local = (1u64 << 48) - 1;
