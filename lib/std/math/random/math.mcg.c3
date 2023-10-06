module std::math::random;

const MUL_MCG128 @local = 0x9e3779b97f4a7c15f39cc0605cedc835;
const MUL_MCG64 @local = 0xf1357aea2e62a9c5;
const MUL_MCG32 @local = 0x93d765dd;
const MUL_MCG16 @local = 0x93d5; // TODO: Find good constant

// Mcg128_64

def Mcg128Random = distinct uint128;

fn void Mcg128Random.set_seed(&self, char[] seed) : Random
{
	*self = (Mcg128Random)(random::make_seed(uint128, seed) | 1);
}

/**
 * @require bytes.len > 0
 **/
fn void Mcg128Random.next_bytes(&self, char[] bytes) : Random
{
	@random_value_to_bytes(self.next_long, bytes);
}

fn ulong Mcg128Random.next_long(&self) : Random
{
	uint128* s = (uint128*)self;
	ulong result = (ulong)(*s >> 64);
	*s *= MUL_MCG128;
	return result;
}

fn uint128 Mcg128Random.next_int128(&self) : Random => @long_to_int128(self.next_long());
fn uint Mcg128Random.next_int(&self) : Random => (uint)self.next_long();
fn ushort Mcg128Random.next_short(&self) : Random => (ushort)self.next_long();
fn char Mcg128Random.next_byte(&self) : Random => (char)self.next_long();




// Mcg64RandomState

def Mcg64Random = distinct ulong;

fn void Mcg64Random.set_seed(&self, char[] seed) : Random
{
	*self = (Mcg64Random)random::make_seed(ulong, seed) | 1;
}

/**
 * @require bytes.len > 0
 **/
fn void Mcg64Random.next_bytes(&self, char[] bytes) : Random
{
	@random_value_to_bytes(self.next_int, bytes);
}

fn uint Mcg64Random.next_int(&self) : Random
{
	ulong* s = (ulong*)self;
	uint result = (uint)(*s >> 32);
	*s *= MUL_MCG64;
	return result;
}

fn uint128 Mcg64Random.next_int128(&self) : Random => @long_to_int128(self.next_long());
fn ulong Mcg64Random.next_long(&self) : Random => @int_to_long(self.next_int());
fn ushort Mcg64Random.next_short(&self) : Random => (ushort)self.next_int();
fn char Mcg64Random.next_byte(&self) : Random => (char)self.next_int();


// Mcg32Random

def Mcg32Random = distinct uint;

fn void Mcg32Random.set_seed(&self, char[] seed) : Random
{
	*self = (Mcg32Random)random::make_seed(uint, seed) | 1;
}

fn ushort Mcg32Random.next_short(&self)
{
	uint* s = (uint*)self;
	ushort result = (ushort)(*s >> 16);
	*s *= MUL_MCG32;
	return result;
}

/**
 * @require bytes.len > 0
 **/
fn void Mcg32Random.next_bytes(&self, char[] bytes) : Random => @random_value_to_bytes(self.next_short, bytes);
fn uint128 Mcg32Random.next_int128(&self) : Random => @long_to_int128(self.next_long());
fn ulong Mcg32Random.next_long(&self) : Random => @int_to_long(self.next_int());
fn uint Mcg32Random.next_int(&self) : Random => @short_to_int(self.next_short());
fn char Mcg32Random.next_byte(&self) : Random => (char)self.next_short();

// -------------------------------- Mcg16RandomState --------------------------------

def Mcg16Random = distinct ushort;

fn void Mcg16Random.set_seed(&self, char[] seed) : Random
{
	*self = (Mcg16Random)random::make_seed(ushort, seed) | 1;
}

fn char Mcg16Random.next_byte(&self) : Random
{
	ushort* s = (ushort*)self;
	char result = (char)(*s >> 8);
	*s *= MUL_MCG16;
	return result;
}

fn void Mcg16Random.next_bytes(&self, char[] bytes) : Random => @random_value_to_bytes(self.next_byte, bytes);
fn uint128 Mcg16Random.next_int128(&self) : Random => @long_to_int128(self.next_long());
fn ulong Mcg16Random.next_long(&self) : Random => @int_to_long(self.next_int());
fn uint Mcg16Random.next_int(&self) : Random => @short_to_int(self.next_short());
fn ushort Mcg16Random.next_short(&self) : Random => @char_to_short(self.next_byte());
