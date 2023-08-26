module std::math;

// Move ODD_PHI into a shared module
const ODD_PHI128 @local = 0x9e3779b97f4a7c15f39cc0605cedc835;
const ODD_PHI64 @local = 0x9e3779b97f4a7c15;
const ODD_PHI32 @local = 0x9e3779b9;
const ODD_PHI16 @local = 0x9e37;
const ODD_PHI8 @local = 0x9f;


// -------------------------------- Sfc128 --------------------------------

struct Sfc128Random
{
	inline Random random;
	Sfc128RandomState state;
}

def Sfc128RandomState = distinct uint128[4];


const RandomInterface SFC_128_RANDOM_INTERFACE @local = {
	.seed_fn = (RandomSeedFn)&Sfc128Random.set_seed,
	.next_bytes_fn = (RandomNextBytesFn)&Sfc128Random.next_bytes,
};

fn void Sfc128Random.init(&self)
{
	self.random.fns = &SFC_128_RANDOM_INTERFACE;
}

fn void  Sfc128Random.set_seed(&self, char[] input)
{
	self.state.set_seed(random::make_seed(uint128[4], input));
}

/**
 * @require bytes.len > 0
 **/
fn void Sfc128Random.next_bytes(&self, char[] bytes)
{
	@random_value_to_bytes(self.state.next_int128, bytes);
}

fn void Sfc128RandomState.set_seed(&self, uint128[4] seed)
{
	*self = (Sfc128RandomState)seed;
}

fn uint128 Sfc128RandomState.next_int128(&self) // TODO: Find good constant
{
	uint128* s = (uint128[4]*)self;
	uint128 result = s[0] + s[1] + s[3];
	s[0] = s[1] ^ s[1] >> 11;
	s[1] = s[2] + s[2] << 3;
	s[2] = s[2].rotr(40) + result;
	s[3] += ODD_PHI128;
	return result;
}


// -------------------------------- Sfc64 --------------------------------


struct Sfc64Random
{
	inline Random random;
	Sfc64RandomState state;
}

def Sfc64RandomState = distinct ulong[4];

const RandomInterface SFC_64_RANDOM_INTERFACE @local = {
	.seed_fn = (RandomSeedFn)&Sfc64Random.set_seed,
	.next_bytes_fn = (RandomNextBytesFn)&Sfc64Random.next_bytes,
};

fn void Sfc64Random.init(&self)
{
	self.random.fns = &SFC_64_RANDOM_INTERFACE;
}

fn void  Sfc64Random.set_seed(&self, char[] input)
{
	self.state.set_seed(random::make_seed(ulong[4], input));
}

/**
 * @require bytes.len > 0
 **/
fn void Sfc64Random.next_bytes(&self, char[] bytes)
{
	@random_value_to_bytes(self.state.next_long, bytes);
}

fn void Sfc64RandomState.set_seed(&self, ulong[4] seed)
{
	*self = (Sfc64RandomState)seed;
}

fn ulong Sfc64RandomState.next_long(&self)
{
	ulong* s = (ulong[4]*)self;
	ulong result = s[0] + s[1] + s[3];
	s[0] = s[1] ^ s[1] >> 11;
	s[1] = s[2] + s[2] << 3;
	s[2] = s[2].rotr(40) + result;
	s[3] += ODD_PHI64;
	return result;
}


// -------------------------------- Sfc32 --------------------------------


struct Sfc32Random
{
	inline Random random;
	Sfc32RandomState state;
}

def Sfc32RandomState = distinct uint[4];

const RandomInterface SFC_32_RANDOM_INTERFACE @local = {
	.seed_fn = (RandomSeedFn)&Sfc32Random.set_seed,
	.next_bytes_fn = (RandomNextBytesFn)&Sfc32Random.next_bytes,
};

fn void Sfc32Random.init(&self)
{
	self.random.fns = &SFC_32_RANDOM_INTERFACE;
}

fn void Sfc32Random.set_seed(&self, char[] input)
{
	self.state.set_seed(random::make_seed(uint[4], input));
}

/**
 * @require bytes.len > 0
 **/
fn void Sfc32Random.next_bytes(&self, char[] bytes)
{
	@random_value_to_bytes(self.state.next_int, bytes);
}

fn void Sfc32RandomState.set_seed(&self, uint[4] seed)
{
	*self = (Sfc32RandomState)seed;
}

fn uint Sfc32RandomState.next_int(&sfc)
{
	uint* s = (uint[4]*)sfc;
	uint result = s[0] + s[1] + s[3];
	s[0] = s[1] ^ s[1] >> 9;
	s[1] = s[2] + s[2] << 3;
	s[2] = s[2].rotr(11) + result;
	s[3] += ODD_PHI32;
	return result;
}


// -------------------------------- Sfc16 --------------------------------

struct Sfc16Random
{
	inline Random random;
	Sfc16RandomState state;
}

def Sfc16RandomState = distinct ushort[4];

const RandomInterface SFC_16_RANDOM_INTERFACE @local = {
	.seed_fn = (RandomSeedFn)&Sfc16Random.set_seed,
	.next_bytes_fn = (RandomNextBytesFn)&Sfc16Random.next_bytes,
};

fn void Sfc16Random.init(&self)
{
	self.random.fns = &SFC_16_RANDOM_INTERFACE;
}

fn void  Sfc16Random.set_seed(&self, char[] input)
{
	self.state.set_seed(random::make_seed(ushort[4], input));
}

/**
 * @require bytes.len > 0
 **/
fn void Sfc16Random.next_bytes(&self, char[] bytes)
{
	@random_value_to_bytes(self.state.next_short, bytes);
}

fn void Sfc16RandomState.set_seed(&self, ushort[4] seed)
{
	*self = (Sfc16RandomState)seed;
}

fn ushort Sfc16RandomState.next_short(&seed)
{
	ushort* s = (ushort[4]*)seed;
	ushort result = s[0] + s[1] + s[3];
	s[0] = s[1] ^ s[1] >> 2;
	s[1] = s[2] + s[2] << 3;
	s[2] = s[2].rotr(12) + result;
	s[3] += ODD_PHI16;
	return result;
}


// -------------------------------- Sfc8 --------------------------------

struct Sfc8Random
{
	inline Random random;
	Sfc8RandomState state;
}

def Sfc8RandomState = distinct char[4];

const RandomInterface SFC_8_RANDOM_INTERFACE @local = {
	.seed_fn = (RandomSeedFn)&Sfc8Random.set_seed,
	.next_bytes_fn = (RandomNextBytesFn)&Sfc8Random.next_bytes,
};

fn void Sfc8Random.init(&self)
{
	self.random.fns = &SFC_8_RANDOM_INTERFACE;
}

fn void  Sfc8Random.set_seed(&self, char[] input)
{
	self.state.set_seed(random::make_seed(char[4], input));
}

/**
 * @require bytes.len > 0
 **/
fn void Sfc8Random.next_bytes(&self, char[] bytes)
{
	@random_value_to_bytes(self.state.next_byte, bytes);
}

fn void Sfc8RandomState.set_seed(&self, char[4] seed)
{
	*self = (Sfc8RandomState)seed;
}

fn char Sfc8RandomState.next_byte(&self) // TODO: Find better constants
{
	char* s = (char[4]*)self;
	char result = s[0] + s[1] + s[3];
	s[0] = s[1] ^ s[1] >> 1;
	s[1] = s[2] + s[2] << 2;
	s[2] = s[2].rotr(3) + result;
	s[3] += ODD_PHI8;
	return result;
}
