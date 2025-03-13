module std::hash::sha256;

import std::hash::hmac;

const BLOCK_SIZE = 64;
const HASH_SIZE = 32;

const uint[64] K @local = {
    0x428a2f98, 0x71374491, 0xb5c0fbcf, 0xe9b5dba5, 0x3956c25b, 0x59f111f1, 0x923f82a4, 0xab1c5ed5,
    0xd807aa98, 0x12835b01, 0x243185be, 0x550c7dc3, 0x72be5d74, 0x80deb1fe, 0x9bdc06a7, 0xc19bf174,
    0xe49b69c1, 0xefbe4786, 0x0fc19dc6, 0x240ca1cc, 0x2de92c6f, 0x4a7484aa, 0x5cb0a9dc, 0x76f988da,
    0x983e5152, 0xa831c66d, 0xb00327c8, 0xbf597fc7, 0xc6e00bf3, 0xd5a79147, 0x06ca6351, 0x14292967,
    0x27b70a85, 0x2e1b2138, 0x4d2c6dfc, 0x53380d13, 0x650a7354, 0x766a0abb, 0x81c2c92e, 0x92722c85,
    0xa2bfe8a1, 0xa81a664b, 0xc24b8b70, 0xc76c51a3, 0xd192e819, 0xd6990624, 0xf40e3585, 0x106aa070,
    0x19a4c116, 0x1e376c08, 0x2748774c, 0x34b0bcb5, 0x391c0cb3, 0x4ed8aa4a, 0x5b9cca4f, 0x682e6ff3,
    0x748f82ee, 0x78a5636f, 0x84c87814, 0x8cc70208, 0x90befffa, 0xa4506ceb, 0xbef9a3f7, 0xc67178f2
};

// Right rotate function
macro uint @rotr(uint x, uint n) @local => (((x) >> (n)) | ((x) << (32 - (n))));

// SHA-256 functions
macro uint @ch(uint x, uint y, uint z) @local => (x & y) ^ (~x & z);
macro uint @maj(uint x, uint y, uint z) @local => (x & y) ^ (x & z) ^ (y & z);
macro uint @_sigma0(uint x) @local => @rotr(x, 2) ^ @rotr(x, 13) ^ @rotr(x, 22);
macro uint @_sigma1(uint x) @local => @rotr(x, 6) ^ @rotr(x, 11) ^ @rotr(x, 25);
macro uint @sigma0(uint x) @local => @rotr(x, 7) ^ @rotr(x, 18) ^ (x >> 3);
macro uint @sigma1(uint x) @local => @rotr(x, 17) ^ @rotr(x, 19) ^ (x >> 10);

struct Sha256
{
    uint[8] state;
    ulong bitcount;
    char[BLOCK_SIZE] buffer;
}

alias HmacSha256 = Hmac{Sha256, HASH_SIZE, BLOCK_SIZE};
alias hmac = hmac::hash{Sha256, HASH_SIZE, BLOCK_SIZE};
alias pbkdf2 = hmac::pbkdf2{Sha256, HASH_SIZE, BLOCK_SIZE};

fn char[HASH_SIZE] hash(char[] data)
{
    Sha256 sha256 @noinit;
    sha256.init();
    sha256.update(data);
    return sha256.final();
}

fn void Sha256.init(&self)
{
    // Sha256 initialization constants
    *self = {
        .state = {
            0x6A09E667,
            0xBB67AE85,
            0x3C6EF372,
            0xA54FF53A,
            0x510E527F,
            0x9B05688C,
            0x1F83D9AB,
            0x5BE0CD19
        }
    };
}

<*
 @param [in] data
 @require data.len <= uint.max
*>
fn void Sha256.update(&self, char[] data) {
    uint i = 0;
    uint len = data.len;
    uint buffer_pos = (uint)(self.bitcount / 8) % BLOCK_SIZE;
    self.bitcount += (ulong)(len * 8);

    while (len--) {
        self.buffer[buffer_pos++] = data[i++];
        if (buffer_pos == BLOCK_SIZE) {
            sha256_transform(&self.state, &self.buffer);
            buffer_pos = 0;  // Reset buffer position
        }
    }
}

fn char[HASH_SIZE] Sha256.final(&self) {
    char[HASH_SIZE] hash;
    ulong i = (self.bitcount / 8) % BLOCK_SIZE;

    // Append 0x80 to the buffer
    self.buffer[i++] = 0x80;

    // Pad the buffer with zeros
    if (i > BLOCK_SIZE - 8) {
        while (i < BLOCK_SIZE) {
            self.buffer[i++] = 0x00;
        }
        sha256_transform(&self.state, &self.buffer);
        i = 0;  // Reset buffer index after transformation
    }
    
    while (i < BLOCK_SIZE - 8) {
        self.buffer[i++] = 0x00;
    }

    // Append the bitcount in big-endian format
    for (int j = 0; j < 8; ++j) {
        self.buffer[BLOCK_SIZE - 8 + j] = (char)((self.bitcount >> (56 - j * 8)) & 0xFF);
    }

    sha256_transform(&self.state, &self.buffer);

    // Convert state to the final hash
    for (i = 0; i < 8; ++i) {
        hash[i * 4] = (char)((self.state[i] >> 24) & 0xFF);
        hash[i * 4 + 1] = (char)((self.state[i] >> 16) & 0xFF);
        hash[i * 4 + 2] = (char)((self.state[i] >> 8) & 0xFF);
        hash[i * 4 + 3] = (char)(self.state[i] & 0xFF);
    }
    
    return hash;
}

<*
 @param [&inout] state
 @param [&in] buffer
*>
fn void sha256_transform(uint* state, char* buffer) @local {
    uint a, b, c, d, e, f, g, h, t1, t2;
    uint[64] m;
    int i;

    // Prepare the message schedule
    for (i = 0; i < 16; ++i) {
     m[i] = ((uint)buffer[i * 4] << 24) | ((uint)buffer[i * 4 + 1] << 16) |
       ((uint)buffer[i * 4 + 2] << 8) | ((uint)buffer[i * 4 + 3]);  // Ensure values are cast to uint for correct shifts
    }
    for (i = 16; i < 64; ++i) {
        m[i] = @sigma1(m[i - 2]) + m[i - 7] + @sigma0(m[i - 15]) + m[i - 16];
    }

    // Initialize working variables
    a = state[0];
    b = state[1];
    c = state[2];
    d = state[3];
    e = state[4];
    f = state[5];
    g = state[6];
    h = state[7];

    // Perform the main SHA-256 compression function
    for (i = 0; i < 64; ++i) {
        t1 = h + @_sigma1(e) + @ch(e, f, g) + K[i] + m[i];
        t2 = @_sigma0(a) + @maj(a, b, c);
        h = g;
        g = f;
        f = e;
        e = d + t1;
        d = c;
        c = b;
        b = a;
        a = t1 + t2;
    }

    // Update the state
    state[0] += a;
    state[1] += b;
    state[2] += c;
    state[3] += d;
    state[4] += e;
    state[5] += f;
    state[6] += g;
    state[7] += h;
    a = b = c = d = e = f = g = h = t1 = t2 = i = 0;
    m[:64] = buffer[:64] = 0;
}