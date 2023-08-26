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


// -------------------------------- Pcg128_64 --------------------------------

struct Pcg128Random
{
	inline Random random;
	Pcg128RandomState state;
}

def Pcg128RandomState = distinct uint128;

const RandomInterface PCG_128_RANDOM_INTERFACE @local = {
	.seed_fn = (RandomSeedFn)&Pcg128Random.set_seed,
	.next_bytes_fn = (RandomNextBytesFn)&Pcg128Random.next_bytes,
};

fn void Pcg128Random.init(&self)
{
	self.random.fns = &PCG_128_RANDOM_INTERFACE;
}

fn void  Pcg128Random.set_seed(&self, char[] input)
{
	self.state.set_seed(random::make_seed(uint128, input));
}

/**
 * @require bytes.len > 0
 **/
fn void Pcg128Random.next_bytes(&self, char[] bytes)
{
	@random_value_to_bytes(self.state.next_long, bytes);
}

fn void Pcg128RandomState.set_seed(&self, uint128 seed)
{
	*self = (Pcg128RandomState)seed;
}

fn ulong Pcg128RandomState.next_long(&self)
{
	const ROT_SHIFT = 64 - 6;
	uint128* s = (uint128*)self;
	uint128 xor = *s ^ *s >> ((128 - ROT_SHIFT) / 2);
	char rot = (char)(*s >> (128 - 6));
	*s = *s * MUL_LCG128 + ODD_PHI128;
	return ((ulong)(xor >> ROT_SHIFT)).rotr(rot);
}


// -------------------------------- Pcg64_32 --------------------------------

struct Pcg64Random
{
	inline Random random;
	Pcg64RandomState state;
}

def Pcg64RandomState = distinct ulong;

const RandomInterface PCG_64_RANDOM_INTERFACE @local = {
	.seed_fn = (RandomSeedFn)&Pcg64Random.set_seed,
	.next_bytes_fn = (RandomNextBytesFn)&Pcg64Random.next_bytes,
};

fn void Pcg64Random.init(&self)
{
	self.random.fns = &PCG_64_RANDOM_INTERFACE;
}

fn void  Pcg64Random.set_seed(&self, char[] input)
{
	self.state.set_seed(random::make_seed(ulong, input));
}

/**
 * @require bytes.len > 0
 **/
fn void Pcg64Random.next_bytes(&self, char[] bytes)
{
	@random_value_to_bytes(self.state.next_int, bytes);
}

fn void Pcg64RandomState.set_seed(&self, ulong seed)
{
	*self = (Pcg64RandomState)seed;
}

fn uint Pcg64RandomState.next_int(&self)
{
	const ROT_SHIFT = 32 - 5;
	ulong* s = (ulong*)self;
	ulong xor = *s ^ *s >> ((64 - ROT_SHIFT) / 2);
	char rot = (char)(*s >> (64 - 5));
	*s = *s * MUL_LCG64 + ODD_PHI64;
	return ((uint)(xor >> ROT_SHIFT)).rotr(rot);
}


// -------------------------------- Pcg32_16 --------------------------------

struct Pcg32Random
{
	inline Random random;
	Pcg32RandomState state;
}

def Pcg32RandomState = distinct uint;

const RandomInterface PCG_32_RANDOM_INTERFACE @local = {
	.seed_fn = (RandomSeedFn)&Pcg32Random.set_seed,
	.next_bytes_fn = (RandomNextBytesFn)&Pcg32Random.next_bytes,
};

fn void Pcg32Random.init(&self)
{
	self.random.fns = &PCG_32_RANDOM_INTERFACE;
}

fn void  Pcg32Random.set_seed(&self, char[] input)
{
	self.state.set_seed(random::make_seed(uint, input));
}

/**
 * @require bytes.len > 0
 **/
fn void Pcg32Random.next_bytes(&self, char[] bytes)
{
	@random_value_to_bytes(self.state.next_short, bytes);
}

fn void Pcg32RandomState.set_seed(&self, uint seed)
{
	*self = (Pcg32RandomState)seed;
}

fn ushort Pcg32RandomState.next_short(&self)
{
	const ROT_SHIFT = 16 - 4;
	uint* s = (uint*)self;
	uint xor = *s ^ *s >> ((32 - ROT_SHIFT) / 2);
	char rot = (char)(*s >> (32 - 4));
	*s = *s * MUL_LCG32 + ODD_PHI32;
	return ((ushort)(xor >> ROT_SHIFT)).rotr(rot);
}


// -------------------------------- Pcg16_8 --------------------------------

struct Pcg16Random
{
	inline Random random;
	Pcg16RandomState state;
}

def Pcg16RandomState = distinct ushort;

const RandomInterface PCG_16_RANDOM_INTERFACE @local = {
	.seed_fn = (RandomSeedFn)&Pcg16Random.set_seed,
	.next_bytes_fn = (RandomNextBytesFn)&Pcg16Random.next_bytes,
};

fn void Pcg16Random.init(&self)
{
	self.random.fns = &PCG_16_RANDOM_INTERFACE;
}

fn void  Pcg16Random.set_seed(&self, char[] input)
{
	self.state.set_seed(random::make_seed(ushort, input));
}

/**
 * @require bytes.len > 0
 **/
fn void Pcg16Random.next_bytes(&self, char[] bytes)
{
	@random_value_to_bytes(self.state.next_byte, bytes);
}

fn void Pcg16RandomState.set_seed(&self, ushort seed)
{
	*self = (Pcg16RandomState)seed;
}

fn char Pcg16RandomState.next_byte(&self)
{
	const ROT_SHIFT = 8 - 3;
	ushort* s = (ushort*)self;
	ushort xor = *s ^ *s >> ((16 - ROT_SHIFT) / 2);
	char rot = (char)(*s >> (16 - 3));
	*s = *s * MUL_LCG16 + ODD_PHI16;
	return ((char)(xor >> ROT_SHIFT)).rotr(rot);
}
