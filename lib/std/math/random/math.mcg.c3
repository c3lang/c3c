module std::math;

const MUL_MCG128 @local = 0x9e3779b97f4a7c15f39cc0605cedc835;
const MUL_MCG64 @local = 0xf1357aea2e62a9c5;
const MUL_MCG32 @local = 0x93d765dd;
const MUL_MCG16 @local = 0x93d5; // TODO: Find good constant

// -------------------------------- Mcg128_64 --------------------------------

struct Mcg128Random
{
	inline Random random;
	Mcg128RandomState state;
}

def Mcg128RandomState = distinct uint128;

const RandomInterface MCG_128_RANDOM_INTERFACE @local = {
	.seed_fn = (RandomSeedFn)&Mcg128Random.set_seed,
	.next_bytes_fn = (RandomNextBytesFn)&Mcg128Random.next_bytes,
};

fn void Mcg128Random.init(&self)
{
	self.random.fns = &MCG_64_RANDOM_INTERFACE;
}

fn void Mcg128Random.set_seed(&self, char[] seed)
{
	self.state.set_seed(random::make_seed(uint128, seed));
}

/**
 * @require bytes.len > 0
 **/
fn void Mcg128Random.next_bytes(&self, char[] bytes)
{
	@random_value_to_bytes(self.state.next_long, bytes);
}

fn void Mcg128RandomState.set_seed(&self, uint128 seed)
{
	*self = (Mcg128RandomState)seed | 1;
}

fn ulong Mcg128RandomState.next_long(&self)
{
	uint128* s = (uint128*)self;
	ulong result = (ulong)(*s >> 64);
	*s *= MUL_MCG128;
	return result;
}




// -------------------------------- Mcg64RandomState --------------------------------

struct Mcg64Random
{
	inline Random random;
	Mcg64RandomState state;
}

const RandomInterface MCG_64_RANDOM_INTERFACE @local = {
	.seed_fn = (RandomSeedFn)&Mcg64Random.set_seed,
	.next_bytes_fn = (RandomNextBytesFn)&Mcg64Random.next_bytes,
};

def Mcg64RandomState = distinct ulong;

fn void Mcg64Random.init(&self)
{
	self.random.fns = &MCG_64_RANDOM_INTERFACE;
}

fn void Mcg64Random.set_seed(&self, char[] seed)
{
	self.state.set_seed(random::make_seed(ulong, seed));
}

/**
 * @require bytes.len > 0
 **/
fn void Mcg64Random.next_bytes(&self, char[] bytes)
{
	@random_value_to_bytes(self.state.next_int, bytes);
}

fn void Mcg64RandomState.set_seed(&self, ulong seed)
{
	*self = (Mcg64RandomState)self | 1;
}

fn uint Mcg64RandomState.next_int(&self)
{
	ulong* s = (ulong*)self;
	uint result = (uint)(*s >> 32);
	*s *= MUL_MCG64;
	return result;
}


// -------------------------------- Mcg32RandomState --------------------------------

def Mcg32RandomState = distinct uint;

fn void Mcg32RandomState.set_seed(&self, uint seed)
{
	*self = (Mcg32RandomState)seed | 1;
}

fn ushort Mcg32RandomState.next_short(&self)
{
	uint* s = (uint*)self;
	ushort result = (ushort)(*s >> 16);
	*s *= MUL_MCG32;
	return result;
}


// -------------------------------- Mcg16RandomState --------------------------------

def Mcg16RandomState = distinct ushort;

fn void Mcg16RandomState.set_seed(&self, ushort seed)
{
	*self = (Mcg16RandomState)seed | 1;
}

fn char Mcg16RandomState.next_byte(&self)
{
	ushort* s = (ushort*)self;
	char result = (char)(*s >> 8);
	*s *= MUL_MCG16;
	return result;
}
