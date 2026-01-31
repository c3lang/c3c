module std::math::random;

struct Xorshiro128PPRandom (Random) {
    uint [4] state;
}

<*
 @require seed.len > 0
*>
fn void Xorshiro128PPRandom.set_seed(&self, char[] seed) @dynamic
{
    self.state = random::make_seed(uint [4], seed);
}

// Xorshiro128++ implementation
fn uint Xorshiro128PPRandom.next_int(&self) @dynamic
{
    uint result = (self.state[0] + self.state[3]).rotl(7);

    uint t  = self.state[1] << 9;

    self.state[2] ^= self.state[0];
    self.state[3] ^= self.state[1];
    self.state[1] ^= self.state[2];
    self.state[0] ^= self.state[3];

    self.state[2] ^= t;

    self.state[3] = self.state[0].rotl(11);

    return result;
}

<*
 @require bytes.len > 0
*>
fn void Xorshiro128PPRandom.next_bytes(&self, char[] bytes) @dynamic => @random_value_to_bytes(self.next_int, bytes);
fn uint128 Xorshiro128PPRandom.next_int128(&self) @dynamic => @long_to_int128(self.next_long());
fn ulong Xorshiro128PPRandom.next_long(&self) @dynamic => @int_to_long(self.next_int());
fn ushort Xorshiro128PPRandom.next_short(&self) @dynamic => (ushort)self.next_int();
fn char Xorshiro128PPRandom.next_byte(&self) @dynamic => (char)self.next_int();
