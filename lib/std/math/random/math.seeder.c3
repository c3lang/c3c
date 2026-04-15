module std::math::random;
import std::hash::a5hash, std::time;

const ODD_PHI64 @local = 0x9e3779b97f4a7c15;
const MUL_MCG64 @local = 0xf1357aea2e62a9c5;
const MUL_LCG64 @local = 0xd1342543de82ef95;

macro make_seed($Type, char[] input)
{
	$Type return_value;
	seeder(input, @as_char_view(return_value));
	return return_value;
}

// TODO: Make endian independent
<*
 @param [in] input
 @param [inout] out_buffer
*>
fn void seeder(char[] input, char[] out_buffer)
{
	$if env::BIG_ENDIAN:
		$echo("Please note that the seeding function behaves differently on BE architectures.");
	$endif
	// Init words
	sz out_chars = out_buffer.len;
	@stack_mem(550; mem)
	{
		ulong[] words = alloc::alloc_array(mem, ulong, (out_chars + 7) / 8);
		words[..] = ODD_PHI64;
		sz words_len_2 = words.len * 2;

		// Add word at a time
		for (sz i = 0; i < input.len / 8; i++)
		{
			sz j = i % (sz)words.len;
			words[j] -= bitcast(*(char[8]*)&input[i * 8], ulong) * MUL_LCG64;
			words[j] ^= words[j] >> 25;
		}

		// Add rest of the bytes
		sz remaining = input.len - input.len / 8 * 8;
		if (remaining)
		{
			ulong rest = MUL_MCG64;
			mem::copy(&rest, &input[^remaining], remaining);
			words[^1] -= rest * MUL_LCG64;
			words[^1] ^= words[^1] >> 25;
		}

		// Mix between words
		for (sz i = words_len_2 - 1; i >= 0; i--)
		{
			sz j = i % words.len;
			words[j] -= words[(i + 1) % words.len] * MUL_LCG64;
			words[j] ^= words[j] >> 25;
		}

		// Mix within words
		for (sz i = 0; i < words_len_2; i++)
		{
			sz j = i % (sz)words.len;
			words[j] *= MUL_MCG64;
			words[j] ^= words[j] >> 25;
		}
		out_buffer[..] = ((char*)words.ptr)[:out_chars];
	};
}

macro uint hash(value) @local
{
	return (uint)a5hash::hash(&&bitcast(value, char[@sizeof(value)]));
}

fn char[8 * 4] entropy() @if(!env::FREESTANDING_WASM)
{
	void* addr = malloc(1);
	free(addr);
	static uint random_int;
	random_int += 0xedf19156;
	uint[8] entropy_data = {
		hash($$TIME),
		hash(addr),
		hash(&addr),
		hash(&entropy),
		random_int,
		hash(clock::now()),
		hash(&DString.init),
		hash(mem)
	};
	return bitcast(entropy_data, char[8 * 4]);
}

fn char[8 * 4] entropy() @if(env::FREESTANDING_WASM)
{
	static uint random_int;
	random_int += 0xedf19156;
	uint[8] entropy_data = {
		hash($$TIME),
		hash(&entropy),
		random_int,
		hash($$TIME),
		hash(&entropy),
		random_int,
		hash($$TIME),
		hash(&entropy),
	};
	return bitcast(entropy_data, char[8 * 4]);
}