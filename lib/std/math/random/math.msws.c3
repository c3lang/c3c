module std::math;

// Move ODD_PHI into a shared module
const ODD_PHI128 @local = 0x9e3779b97f4a7c15f39cc0605cedc835;
const ODD_PHI64 @local = 0x9e3779b97f4a7c15;
const ODD_PHI32 @local = 0x9e3779b9;
const ODD_PHI16 @local = 0x9e37;
const ODD_PHI8 @local = 0x9f;


// -------------------------------- Msws128 --------------------------------

struct Msws128 {
	uint128 state0, state1;
	uint128 weyl0, weyl1;
}

fn void Msws128.seed(&msws, char[16 * 4] seed)
{
	*msws = bitcast(seed, Msws128);
}

fn uint128 Msws128.next(&msws)
{
	uint128 s0 = msws.state0;
	msws.state0 = msws.state0 * msws.state0 + msws.weyl0;
	msws.state0 = msws.state0.rotr(64);
	msws.weyl0 += ODD_PHI128;

	msws.state1 = msws.state1 * msws.state1 + msws.weyl1;
	uint128 s1 = msws.state1;
	msws.state1 = msws.state1.rotr(64);
	msws.weyl1 -= ODD_PHI128;

	return s0 + s1;
}


// -------------------------------- Msws64 --------------------------------

struct Msws64 {
	ulong state0, state1;
	ulong weyl0, weyl1;
}

fn void Msws64.seed(&msws, char[8 * 4] seed)
{
	*msws = bitcast(seed, Msws64);
}

fn ulong Msws64.next(&msws)
{
	ulong s0 = msws.state0;
	msws.state0 = msws.state0 * msws.state0 + msws.weyl0;
	msws.state0 = msws.state0.rotr(32);
	msws.weyl0 += ODD_PHI64;

	msws.state1 = msws.state1 * msws.state1 + msws.weyl1;
	ulong s1 = msws.state1;
	msws.state1 = msws.state1.rotr(32);
	msws.weyl1 -= ODD_PHI64;

	return s0 + s1;
}


// -------------------------------- Msws32 --------------------------------

struct Msws32 {
	uint state0, state1;
	uint weyl0, weyl1;
}

fn void Msws32.seed(&msws, char[4 * 4] seed)
{
	*msws = bitcast(seed, Msws32);
}

fn uint Msws32.next(&msws)
{
	uint s0 = msws.state0;
	msws.state0 = msws.state0 * msws.state0 + msws.weyl0;
	msws.state0 = msws.state0.rotr(16);
	msws.weyl0 += ODD_PHI32;

	msws.state1 = msws.state1 * msws.state1 + msws.weyl1;
	uint s1 = msws.state1;
	msws.state1 = msws.state1.rotr(16);
	msws.weyl1 -= ODD_PHI32;

	return s0 + s1;
}


// -------------------------------- Msws16 --------------------------------

struct Msws16 {
	ushort state0, state1;
	ushort weyl0, weyl1;
}

fn void Msws16.seed(&msws, char[2 * 4] seed)
{
	*msws = bitcast(seed, Msws16);
}

fn ushort Msws16.next(&msws)
{
	ushort s0 = msws.state0;
	msws.state0 = msws.state0 * msws.state0 + msws.weyl0;
	msws.state0 = msws.state0.rotr(8);
	msws.weyl0 += ODD_PHI16;

	msws.state1 = msws.state1 * msws.state1 + msws.weyl1;
	ushort s1 = msws.state1;
	msws.state1 = msws.state1.rotr(8);
	msws.weyl1 -= ODD_PHI16;

	return s0 + s1;
}


// -------------------------------- Msws8 --------------------------------

struct Msws8 {
	char state0, state1;
	char weyl0, weyl1;
}

fn void Msws8.seed(&msws, char[1 * 4] seed)
{
	*msws = bitcast(seed, Msws8);
}

fn char Msws8.next(&msws)
{
	char s0 = msws.state0;
	msws.state0 = msws.state0 * msws.state0 + msws.weyl0;
	msws.state0 = msws.state0.rotr(4);
	msws.weyl0 += ODD_PHI8;

	msws.state1 = msws.state1 * msws.state1 + msws.weyl1;
	char s1 = msws.state1;
	msws.state1 = msws.state1.rotr(4);
	msws.weyl1 -= ODD_PHI8;

	return s0 + s1;
}
