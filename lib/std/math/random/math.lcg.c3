module std::math::random;

// Move ODD_PHI into a shared module
const ODD_PHI128 @local = 0x9e3779b97f4a7c15f39cc0605cedc835;
const ODD_PHI64 @local = 0x9e3779b97f4a7c15;
const ODD_PHI32 @local = 0x9e3779b9;
const ODD_PHI16 @local = 0x9e37;

const MUL_LCG128 @local = 0xdb36357734e34abb0050d0761fcdfc15;
const MUL_LCG64 @local = 0xd1342543de82ef95;
const MUL_LCG32 @local = 0x915f77f5;
const MUL_LCG16 @local = 0x915d; // TODO: Find good constant


// Lcg128_64
def Lcg128Random = distinct uint128;

fn void Lcg128Random.set_seed(&self, char[] input) : Random
{
	*self = (Lcg128Random)random::make_seed(uint128, input);
}


fn ulong Lcg128Random.next_long(&self) : Random
{
	uint128* s = (uint128*)self;
	ulong result = (ulong)(*s >> 64);
	*s = *s * MUL_LCG128 + ODD_PHI128;
	return result;
}

/**
 * @require bytes.len > 0
 **/
fn void Lcg128Random.next_bytes(&self, char[] bytes) : Random => @random_value_to_bytes(self.next_long, bytes);
fn uint128 Lcg128Random.next_int128(&self) : Random => @long_to_int128(self.next_long());
fn uint Lcg128Random.next_int(&self) : Random => (uint)self.next_long();
fn ushort Lcg128Random.next_short(&self) : Random => (ushort)self.next_long();
fn char Lcg128Random.next_byte(&self) : Random => (char)self.next_long();


// -------------------------------- Lcg64_32 --------------------------------
def Lcg64Random = distinct ulong;

fn void Lcg64Random.set_seed(&self, char[] seed) : Random
{
	*self = (Lcg64Random)random::make_seed(ulong, seed);
}


fn uint Lcg64Random.next_int(&self) : Random
{
	ulong* s = (ulong*)self;
	uint result = (uint)(*s >> 32);
	*s = *s * MUL_LCG64 + ODD_PHI64;
	return result;
}

/**
 * @require bytes.len > 0
 **/
fn void Lcg64Random.next_bytes(&self, char[] bytes) : Random => @random_value_to_bytes(self.next_int, bytes);
fn uint128 Lcg64Random.next_int128(&self) : Random => @long_to_int128(self.next_long());
fn ulong Lcg64Random.next_long(&self) : Random => @int_to_long(self.next_int());
fn ushort Lcg64Random.next_short(&self) : Random => (ushort)self.next_int();
fn char Lcg64Random.next_byte(&self) : Random => (char)self.next_int();

// -------------------------------- Lcg32_16 --------------------------------

def Lcg32Random = distinct uint;

fn void Lcg32Random.set_seed(&self, char[] seed) : Random
{
	*self = (Lcg32Random)random::make_seed(uint, seed);
}

fn ushort Lcg32Random.next_short(&self) : Random
{
	uint* s = (uint*)self;
	ushort result = (ushort)(*s >> 16);
	*s = *s * MUL_LCG32 + ODD_PHI32;
	return result;
}

fn void Lcg32Random.next_bytes(&self, char[] bytes) : Random => @random_value_to_bytes(self.next_short, bytes);
fn uint128 Lcg32Random.next_int128(&self) : Random => @long_to_int128(self.next_long());
fn ulong Lcg32Random.next_long(&self) : Random => @int_to_long(self.next_int());
fn uint Lcg32Random.next_int(&self) : Random => @short_to_int(self.next_short());
fn char Lcg32Random.next_byte(&self) : Random => (char)self.next_short();


// -------------------------------- Lcg16_8 --------------------------------
def Lcg16Random = distinct ushort;

fn void Lcg16Random.set_seed(&self, char[] seed) : Random
{
	*self = (Lcg16Random)random::make_seed(ushort, seed);
}

fn char Lcg16Random.next_byte(&self) : Random
{
	ushort* s = (ushort*)self;
	char result = (char)(*s >> 8);
	*s = *s * MUL_LCG16 + ODD_PHI16;
	return result;
}

fn void Lcg16Random.next_bytes(&self, char[] bytes) : Random => @random_value_to_bytes(self.next_byte, bytes);
fn uint128 Lcg16Random.next_int128(&self) : Random => @long_to_int128(self.next_long());
fn ulong Lcg16Random.next_long(&self) : Random => @int_to_long(self.next_int());
fn uint Lcg16Random.next_int(&self) : Random => @short_to_int(self.next_short());
fn ushort Lcg16Random.next_short(&self) : Random => @char_to_short(self.next_byte());
