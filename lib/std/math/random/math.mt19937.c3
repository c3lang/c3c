module std::math::random;

struct Mt19937Random (Random)
{
    uint[624] state;
    int index;
}

<*
 @require seed.len > 0
 @require seed.len <= 624
*>
fn void Mt19937Random.set_seed(&self, char[] seed) @dynamic
{
    // derive a good uint start value
    uint s = 0;
    for (int i = 0; i < 624; i++) {
        s = 1812433253 * (s ^ s >> 30) + i + seed[i % seed.len];
    }

    // actually populate state array
    for (int i = 0; i < 624; i++) {
        s = 1812433253 * (s ^ s >> 30) + i + seed[i % seed.len];
        self.state[i] = s;
    }

    self.index = 0;
}

// see https://en.wikipedia.org/wiki/Mersenne_Twister#C_code
fn uint Mt19937Random.next_int(&self) @dynamic
{
    int k = self.index;
    int j = k - 623;
    if (j < 0) j += 624;

    uint x = (self.state[k] & 0x80000000) | (self.state[j] & 0x7fffffff);

    uint xA = x >> 1;
    if (x & 0x00000001) xA ^= 0x9908b0df;

    j = k - 227;
    if (j < 0) j += 624;

    x = self.state[j] ^ xA;
    self.state[k++] = x;

    if (k >= 624) k = 0;
    self.index = k;

    uint y = x ^ (x >> 11);
    y = y ^ ((y << 7) & 0x9d2c5680);
    y = y ^ ((y << 15) & 0xefc60000);

    return y ^ (y >> 18);
}

<*
 @require bytes.len > 0
*>
fn void Mt19937Random.next_bytes(&self, char[] bytes) @dynamic => @random_value_to_bytes(self.next_int, bytes);
fn uint128 Mt19937Random.next_int128(&self) @dynamic => @long_to_int128(self.next_long());
fn ulong Mt19937Random.next_long(&self) @dynamic => @int_to_long(self.next_int());
fn ushort Mt19937Random.next_short(&self) @dynamic => (ushort)self.next_int();
fn char Mt19937Random.next_byte(&self) @dynamic => (char)self.next_int();
