module std::math;

// Move ODD_PHI into a shared module
const ODD_PHI128 @local = 0x9e3779b97f4a7c15f39cc0605cedc835;
const ODD_PHI64 @local = 0x9e3779b97f4a7c15;
const ODD_PHI32 @local = 0x9e3779b9;
const ODD_PHI16 @local = 0x9e37;
const ODD_PHI8 @local = 0x9f;


// -------------------------------- Msws128 --------------------------------

struct Msws128Random
{
	inline Random random;
	Msws128RandomState state;
}

struct Msws128RandomState
{
	uint128 state0, state1;
	uint128 weyl0, weyl1;
}

const RandomInterface MSWS_128_RANDOM_INTERFACE @local = {
	.seed_fn = (RandomSeedFn)&Msws128Random.set_seed,
	.next_bytes_fn = (RandomNextBytesFn)&Msws128Random.next_bytes,
};

fn void Msws128Random.init(&self)
{
	self.random.fns = &MSWS_128_RANDOM_INTERFACE;
}

fn void  Msws128Random.set_seed(&self, char[] input)
{
	self.state.set_seed(random::make_seed(uint128[4], input));
}

/**
 * @require bytes.len > 0
 **/
fn void Msws128Random.next_bytes(&self, char[] bytes)
{
	@random_value_to_bytes(self.state.next_int128, bytes);
}

fn void Msws128RandomState.set_seed(&self, uint128[4] seed)
{
	*self = bitcast(seed, Msws128RandomState);
}

fn uint128 Msws128RandomState.next_int128(&self)
{
	uint128 s0 = self.state0;
	self.state0 = self.state0 * self.state0 + self.weyl0;
	self.state0 = self.state0.rotr(64);
	self.weyl0 += ODD_PHI128;

	self.state1 = self.state1 * self.state1 + self.weyl1;
	uint128 s1 = self.state1;
	self.state1 = self.state1.rotr(64);
	self.weyl1 -= ODD_PHI128;

	return s0 + s1;
}


// -------------------------------- Msws64 --------------------------------

struct Msws64Random
{
	inline Random random;
	Msws64RandomState state;
}

struct Msws64RandomState
{
	ulong state0, state1;
	ulong weyl0, weyl1;
}

const RandomInterface MSWS_64_RANDOM_INTERFACE @local = {
	.seed_fn = (RandomSeedFn)&Msws64Random.set_seed,
	.next_bytes_fn = (RandomNextBytesFn)&Msws64Random.next_bytes,
};

fn void Msws64Random.init(&self)
{
	self.random.fns = &MSWS_64_RANDOM_INTERFACE;
}

fn void  Msws64Random.set_seed(&self, char[] input)
{
	self.state.set_seed(random::make_seed(ulong[4], input));
}

/**
 * @require bytes.len > 0
 **/
fn void Msws64Random.next_bytes(&self, char[] bytes)
{
	@random_value_to_bytes(self.state.next_long, bytes);
}

fn void Msws64RandomState.set_seed(&self, ulong[4] seed)
{
	*self = bitcast(seed, Msws64RandomState);
}

fn ulong Msws64RandomState.next_long(&self)
{
	ulong s0 = self.state0;
	self.state0 = self.state0 * self.state0 + self.weyl0;
	self.state0 = self.state0.rotr(32);
	self.weyl0 += ODD_PHI64;

	self.state1 = self.state1 * self.state1 + self.weyl1;
	ulong s1 = self.state1;
	self.state1 = self.state1.rotr(32);
	self.weyl1 -= ODD_PHI64;

	return s0 + s1;
}


// -------------------------------- Msws32 --------------------------------

struct Msws32Random
{
	inline Random random;
	Msws32RandomState state;
}

struct Msws32RandomState
{
	uint state0, state1;
	uint weyl0, weyl1;
}

const RandomInterface MSWS_32_RANDOM_INTERFACE @local = {
	.seed_fn = (RandomSeedFn)&Msws32Random.set_seed,
	.next_bytes_fn = (RandomNextBytesFn)&Msws32Random.next_bytes,
};

fn void Msws32Random.init(&self)
{
	self.random.fns = &MSWS_32_RANDOM_INTERFACE;
}

fn void  Msws32Random.set_seed(&self, char[] input)
{
	self.state.set_seed(random::make_seed(uint[4], input));
}

/**
 * @require bytes.len > 0
 **/
fn void Msws32Random.next_bytes(&self, char[] bytes)
{
	@random_value_to_bytes(self.state.next_int, bytes);
}

fn void Msws32RandomState.set_seed(&self, uint[4] seed)
{
	*self = bitcast(seed, Msws32RandomState);
}

fn uint Msws32RandomState.next_int(&self)
{
	uint s0 = self.state0;
	self.state0 = self.state0 * self.state0 + self.weyl0;
	self.state0 = self.state0.rotr(16);
	self.weyl0 += ODD_PHI32;

	self.state1 = self.state1 * self.state1 + self.weyl1;
	uint s1 = self.state1;
	self.state1 = self.state1.rotr(16);
	self.weyl1 -= ODD_PHI32;

	return s0 + s1;
}


// -------------------------------- Msws16 --------------------------------

struct Msws16Random
{
	inline Random random;
	Msws16RandomState state;
}

struct Msws16RandomState
{
	ushort state0, state1;
	ushort weyl0, weyl1;
}

const RandomInterface MSWS_16_RANDOM_INTERFACE @local = {
	.seed_fn = (RandomSeedFn)&Msws16Random.set_seed,
	.next_bytes_fn = (RandomNextBytesFn)&Msws16Random.next_bytes,
};

fn void Msws16Random.init(&self)
{
	self.random.fns = &MSWS_16_RANDOM_INTERFACE;
}

fn void  Msws16Random.set_seed(&self, char[] input)
{
	self.state.set_seed(random::make_seed(ushort[4], input));
}

/**
 * @require bytes.len > 0
 **/
fn void Msws16Random.next_bytes(&self, char[] bytes)
{
	@random_value_to_bytes(self.state.next_short, bytes);
}


fn void Msws16RandomState.set_seed(&self, ushort[4] seed)
{
	*self = bitcast(seed, Msws16RandomState);
}

fn ushort Msws16RandomState.next_short(&self)
{
	ushort s0 = self.state0;
	self.state0 = self.state0 * self.state0 + self.weyl0;
	self.state0 = self.state0.rotr(8);
	self.weyl0 += ODD_PHI16;

	self.state1 = self.state1 * self.state1 + self.weyl1;
	ushort s1 = self.state1;
	self.state1 = self.state1.rotr(8);
	self.weyl1 -= ODD_PHI16;

	return s0 + s1;
}


// -------------------------------- Msws8 --------------------------------

struct Msws8Random
{
	inline Random random;
	Msws8RandomState state;
}

struct Msws8RandomState
{
	char state0, state1;
	char weyl0, weyl1;
}

const RandomInterface MSWS_8_RANDOM_INTERFACE @local = {
	.seed_fn = (RandomSeedFn)&Msws8Random.set_seed,
	.next_bytes_fn = (RandomNextBytesFn)&Msws8Random.next_bytes,
};

fn void Msws8Random.init(&self)
{
	self.random.fns = &MSWS_8_RANDOM_INTERFACE;
}

fn void  Msws8Random.set_seed(&self, char[] input)
{
	self.state.set_seed(random::make_seed(char[4], input));
}

/**
 * @require bytes.len > 0
 **/
fn void Msws8Random.next_bytes(&self, char[] bytes)
{
	@random_value_to_bytes(self.state.next_byte, bytes);
}

fn void Msws8RandomState.set_seed(&msws, char[4] seed)
{
	*msws = bitcast(seed, Msws8RandomState);
}

fn char Msws8RandomState.next_byte(&self)
{
	char s0 = self.state0;
	self.state0 = self.state0 * self.state0 + self.weyl0;
	self.state0 = self.state0.rotr(4);
	self.weyl0 += ODD_PHI8;

	self.state1 = self.state1 * self.state1 + self.weyl1;
	char s1 = self.state1;
	self.state1 = self.state1.rotr(4);
	self.weyl1 -= ODD_PHI8;

	return s0 + s1;
}
