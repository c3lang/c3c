// For more information about the various Blackman-Vigna Xorshift PRNGs, see: https://prng.di.unimi.it/
//   Also: https://vigna.di.unimi.it/ftp/papers/ScrambledLinear.pdf
//
module std::math::random;

enum XoshiroOperation
{
	PLUS,
	PLUS_PLUS,
	STAR_STAR,
}

<*
 @require @in(BIT_SIZE, 128, 256, 512) : "You must use a supported state size for Xoshiro permutations."
 @require @in(OPERATION, ...XoshiroOperation.values) : "You must use a supported operation type for Xoshiro permutations."
*>
struct Xoshiro (Random) <BIT_SIZE, OPERATION>
{
	uint[<4>]  state @if (BIT_SIZE == 128);
	ulong[<4>] state @if (BIT_SIZE == 256);
	ulong[<8>] state @if (BIT_SIZE == 512);
}

<*
 @require seed.len > 0 : "Seed cannot be empty."
*>
fn void Xoshiro.set_seed(&self, char[] seed) @dynamic
{
	self.state = random::make_seed($typeof(self.state), seed);
}

macro _xoshiro_result(s, usz $bits, XoshiroOperation $op) @local
{
	$if $op == STAR_STAR: return (s[1] * 5).rotl(7) * 9; $endif   // same for all types of xoshiro
	$switch $bits:
		$case 128:
		$case 256:
			$switch $op:
				$case PLUS: return s[0] + s[3];
				$case PLUS_PLUS: return (s[0] + s[3]).rotl($bits == 128 ??? 7 : 23) + s[0];
			$endswitch
		$case 512:
			$switch $op:
				$case PLUS: return s[0] + s[2];
				$case PLUS_PLUS: return (s[0] + s[2]).rotl(17) + s[2];
			$endswitch
	$endswitch
	unreachable();
}

macro void @xoshiro_update(#s, usz $bits) @local
{
	$switch $bits:
		$case 128:
			uint t = (uint)(#s[1] << 9);
			#s[2] ^= #s[0];
			#s[3] ^= #s[1];
			#s[1] ^= #s[2];
			#s[0] ^= #s[3];
			#s[2] ^= t;
			#s[3] = #s[3].rotl(11);
		$case 256:
			ulong t = (ulong)(#s[1] << 17);
			#s[2] ^= #s[0];
			#s[3] ^= #s[1];
			#s[1] ^= #s[2];
			#s[0] ^= #s[3];
			#s[2] ^= t;
			#s[3] = #s[3].rotl(45);
		$case 512:
			ulong t = (ulong)(#s[1] << 11);
			#s[2] ^= #s[0];
			#s[5] ^= #s[1];
			#s[1] ^= #s[2];
			#s[7] ^= #s[3];
			#s[3] ^= #s[4];
			#s[4] ^= #s[5];
			#s[0] ^= #s[6];
			#s[6] ^= #s[7];
			#s[6] ^= t;
			#s[7] = #s[7].rotl(21);
		$default: $error @sprintf("Invalid Xoshiro bit size '%s'.", BIT_SIZE);
	$endswitch
}

macro Xoshiro.next(&self)
{
	var result = _xoshiro_result(self.state, BIT_SIZE, OPERATION);
	@xoshiro_update(self.state, BIT_SIZE);
	return result;
}


<*
 @require bytes.len > 0
*>
fn void Xoshiro.next_bytes(&self, char[] bytes) @dynamic => @random_value_to_bytes(self.next, bytes);

fn uint128 Xoshiro.next_int128(&self) @dynamic => @long_to_int128(self.next_long());

fn ulong Xoshiro.next_long(&self) @dynamic @if (BIT_SIZE != 128) => self.next();
fn uint Xoshiro.next_int(&self) @dynamic @if (BIT_SIZE != 128) => (uint)self.next_long();

fn ulong Xoshiro.next_long(&self) @dynamic @if (BIT_SIZE == 128) => @int_to_long(self.next());
fn uint Xoshiro.next_int(&self) @dynamic @if (BIT_SIZE == 128) => self.next();

fn ushort Xoshiro.next_short(&self) @dynamic => (ushort)self.next_long();

fn char Xoshiro.next_byte(&self) @dynamic => (char)self.next_long();
