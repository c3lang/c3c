module base64;
// Based on the C2 version.


fault DecodingError
{
	INVALID_CHARACTER
}
const char[64] LUT_ENC = {
	'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H',
	'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P',
	'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X',
	'Y', 'Z', 'a', 'b', 'c', 'd', 'e', 'f',
	'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n',
	'o', 'p', 'q', 'r', 's', 't', 'u', 'v',
	'w', 'x', 'y', 'z', '0', '1', '2', '3',
	'4', '5', '6', '7', '8', '9', '+', '/',
};

const char ERR = 0xFF;

const char[256] LUT_DEC =
{
	[0..255] = ERR,
	['A'] = 0, ['B'] = 1, ['C'] = 2, ['D'] = 3, ['E'] = 4,
	['F'] = 5, ['G'] = 6, ['H'] = 7, ['I'] = 8, ['J'] = 9,
	['K'] = 10, ['L'] = 11, ['M'] = 12, ['N'] = 13, ['O'] = 14,
	['P'] = 15, ['Q'] = 16, ['R'] = 17, ['S'] = 18, ['T'] = 19,
	['U'] = 20, ['V'] = 21, ['W'] = 22, ['X'] = 23, ['Y'] = 24,
	['Z'] = 25, ['a'] = 26, ['b'] = 27, ['c'] = 28, ['d'] = 29,
	['e'] = 30, ['f'] = 31, ['g'] = 32, ['h'] = 33, ['i'] = 34,
	['j'] = 35, ['k'] = 36, ['l'] = 37, ['m'] = 38, ['n'] = 39,
	['o'] = 40, ['p'] = 41, ['q'] = 42, ['r'] = 43, ['s'] = 44,
	['t'] = 45, ['u'] = 46, ['v'] = 47, ['w'] = 48, ['x'] = 49,
	['y'] = 50, ['z'] = 51, ['0'] = 52, ['1'] = 53, ['2'] = 54,
	['3'] = 55, ['4'] = 56, ['5'] = 57, ['6'] = 58, ['7'] = 59,
	['8'] = 60, ['9'] = 61, ['+'] = 62, ['/'] = 63
};


const char PAD = '=';
const char FIRST = '+';
const char LAST = 'z';

fn void encode(char[] in, char *out)
{
	int j = 0;
	char c = LUT_ENC[1];
	for (int i = 0; i < in.len; i++)
	{
		switch (i % 3)
		{
			case 0:
				out[j++] = LUT_ENC[(in[i] >> 2) & 0x3F];
			case 1:
				out[j++] = LUT_ENC[(in[i - 1] & 0x3) << 4 + ((in[i] >> 4) & 0xF)];
			case 2:
				out[j++] = LUT_ENC[(in[i - 1] & 0xF) << 2 + ((in[i] >> 6) & 0x3)];
				out[j++] = LUT_ENC[in[i] & 0x3F];
		}
	}

	// move back
	usz last = in.len - 1;
	// check the last and add padding
	switch (last % 3)
	{
		case 0:
			out[j++] = LUT_ENC[(in[last] & 0x3) << 4];
			out[j++] = PAD;
			out[j++] = PAD;
		case 1:
			out[j++] = LUT_ENC[(in[last] & 0xF) << 2];
			out[j++] = PAD;
	}
}


fn int! decode(String in, char* out, int* invalid_char_index = null)
{
	int j = 0;

	for (int i = 0; i < in.len; i++)
	{
		char value = in[i];
		if (value == PAD) return j;

		char c = LUT_DEC[in[i]];
		if (c == ERR)
		{
			if (invalid_char_index) *invalid_char_index = i;
			return DecodingError.INVALID_CHARACTER?;
		}


		switch (i % 4)
		{
			case 0:
				out[j] = c << 2;
			case 1:
				out[j++] += c >> 4 & 0x3;
				// if not last char with padding
				if (i < (in.len - 3) || in[(long)(in.len) - 2] != PAD)
				{
					out[j] = (c & 0xF) << 4;
				}
			case 2:
				out[j++] += c >> 2 & 0xF;
				if (i < (in.len - 2) || in[(long)(in.len) - 1] != PAD)
				{
					out[j] = (c & 0x3) << 6;
				}
			case 3:
				out[j++] += c;
		}

	}
	return j;
}

extern fn void printf(char *fmt, ...);

fn void main()
{
	char *helloworld = "Hello World\n";
	char[1000] buffer;
	encode(helloworld[0..12], &buffer);
	printf("Result: %s\n", &buffer);
	char *to_decode = "aGVsbG8gd29ybGRcMA==";
	char[*] result = b64"aGVsbG8gd29ybGRcMA==";
	decode((String)to_decode[0..19], &buffer)!!;
	printf("Result: %s\n", &buffer);
	printf("Result direct: %.*s\n", 13, &result);
}