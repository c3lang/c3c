module std::math;

const ODD_PHI64 @local = 0x9e3779b97f4a7c15;
const MUL_MCG64 @local = 0xf1357aea2e62a9c5;
const MUL_LCG64 @local = 0xd1342543de82ef95;

// TODO: Make endian independent
fn char[] seeder(usz outChars, char[] input)
{
	$if env::BIG_ENDIAN:
		$echo("Please note that the seeding function behaves differently on BE architectures.");
	$endif

	// Init words
	ulong[] words = malloc(ulong, (outChars + 7) / 8);
	words[..] = ODD_PHI64;

	// Add word at a time
	for (usz i = 0; i < input.len / 8; i++)
	{
		usz j = i % words.len;
		words[j] -= bitcast(*(char[8]*)&input[i * 8], ulong) * MUL_LCG64;
		words[j] ^= words[j] >> 25;
	}

	// Add rest of the bytes
	usz remaining = input.len - input.len / 8 * 8;
	if (remaining)
	{
		ulong rest = MUL_MCG64;
		mem::copy(&rest, &input[^remaining], remaining);
		words[^1] -= rest * MUL_LCG64;
		words[^1] ^= words[^1] >> 25;
	}

	// Mix between words
	for (isz i = words.len * 2 - 1; i >= 0; i--)
	{
		isz j = i % words.len;
		words[j] -= words[(i + 1) % words.len] * MUL_LCG64;
		words[j] ^= words[j] >> 25;
	}

	// Mix within words
	for (usz i = 0; i < 2; i++)
	{
		usz j = i % words.len;
		words[j] *= MUL_MCG64;
		words[j] ^= words[j] >> 25;
	}

	return ((char*)words.ptr)[:words.len * 8];
}
