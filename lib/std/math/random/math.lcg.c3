module std::math;

// Move ODD_PHI into a shared module
const ODD_PHI128 @local = 0x9e3779b97f4a7c15f39cc0605cedc835;
const ODD_PHI64 @local = 0x9e3779b97f4a7c15;
const ODD_PHI32 @local = 0x9e3779b9;
const ODD_PHI16 @local = 0x9e37;

const MUL_LCG128 @local = 0xdb36357734e34abb0050d0761fcdfc15;
const MUL_LCG64 @local = 0xd1342543de82ef95;
const MUL_LCG32 @local = 0x915f77f5;
const MUL_LCG16 @local = 0x915d; // TODO: Find good constant


// -------------------------------- Lcg128_64 --------------------------------

struct Lcg128Random
{
	inline Random random;
	Lcg128RandomState state;
}

def Lcg128RandomState = distinct uint128;

const RandomInterface LCG_128_RANDOM_INTERFACE @local = {
	.seed_fn = (RandomSeedFn)&Lcg128Random.set_seed,
	.next_bytes_fn = (RandomNextBytesFn)&Lcg128Random.next_bytes,
};

fn void Lcg128Random.init(&self)
{
	self.random.fns = &LCG_128_RANDOM_INTERFACE;
}

fn void Lcg128Random.set_seed(&self, char[] input)
{
	self.state.set_seed(random::make_seed(uint128, input));
}

/**
 * @require bytes.len > 0
 **/
fn void Lcg128Random.next_bytes(&self, char[] bytes)
{
	@random_value_to_bytes(self.state.next_long, bytes);
}

fn void Lcg128RandomState.set_seed(&self, uint128 seed) @inline
{
	*self = (Lcg128RandomState)seed;
}

fn ulong Lcg128RandomState.next_long(&self)
{
	uint128* s = (uint128*)self;
	ulong result = (ulong)(*s >> 64);
	*s = *s * MUL_LCG128 + ODD_PHI128;
	return result;
}


// -------------------------------- Lcg64_32 --------------------------------

struct Lcg64Random
{
	inline Random random;
	Lcg64RandomState state;
}

def Lcg64RandomState = distinct ulong;

const RandomInterface LCG_64_RANDOM_INTERFACE @local = {
	.seed_fn = (RandomSeedFn)&Lcg64Random.set_seed,
	.next_bytes_fn = (RandomNextBytesFn)&Lcg64Random.next_bytes,
};

fn void Lcg64Random.init(&self)
{
	self.random.fns = &LCG_64_RANDOM_INTERFACE;
}

fn void Lcg64Random.set_seed(&self, char[] seed)
{
	self.state.set_seed(random::make_seed(ulong, seed));
}

/**
 * @require bytes.len > 0
 **/
fn void Lcg64Random.next_bytes(&self, char[] bytes)
{
	@random_value_to_bytes(self.state.next_int, bytes);
}

fn void Lcg64RandomState.set_seed(&self, ulong seed)
{
	*self = (Lcg64RandomState)seed;
}

fn uint Lcg64RandomState.next_int(&self)
{
	ulong* s = (ulong*)self;
	uint result = (uint)(*s >> 32);
	*s = *s * MUL_LCG64 + ODD_PHI64;
	return result;
}


// -------------------------------- Lcg32_16 --------------------------------

def Lcg32RandomState = distinct uint;

fn void Lcg32RandomState.set_seed(&self, uint seed)
{
	*self = (Lcg32RandomState)seed;
}

fn ushort Lcg32RandomState.next_short(&self)
{
	uint* s = (uint*)self;
	ushort result = (ushort)(*s >> 16);
	*s = *s * MUL_LCG32 + ODD_PHI32;
	return result;
}


// -------------------------------- Lcg16_8 --------------------------------

def Lcg16RandomState = distinct ushort;

fn void Lcg16RandomState.set_seed(&self, ushort seed)
{
	*self = (Lcg16RandomState)seed;
}

fn char Lcg16RandomState.next_byte(&self)
{
	ushort* s = (ushort*)self;
	char result = (char)(*s >> 8);
	*s = *s * MUL_LCG16 + ODD_PHI16;
	return result;
}
