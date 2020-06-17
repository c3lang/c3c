module base64;
// Based on the C2 version.

const char[] LUT_ENC = {
    'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H',
    'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P',
    'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X',
    'Y', 'Z', 'a', 'b', 'c', 'd', 'e', 'f',
    'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n',
    'o', 'p', 'q', 'r', 's', 't', 'u', 'v',
    'w', 'x', 'y', 'z', '0', '1', '2', '3',
    '4', '5', '6', '7', '8', '9', '+', '/',
};

const byte ERR = 0xFF;

const byte[256] LUT_DEC = {
    // '+', ',', '-', '.', '/', '0', '1', '2'
        62, ERR, ERR, ERR, 63,  52,  53,  54,
    // '3', '4', '5', '6', '7', '8', '9', ':'
        55,  56,  57,  58,  59,  60,  61, ERR,
    // ';', '<', '=', '>', '?', '@', 'A', 'B'
        ERR, ERR, ERR, ERR, ERR, ERR,  0,  1,
    // 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J'
         2,   3,   4,   5,   6,   7,   8,   9,
    // 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R'
        10,  11,  12,  13,  14,  15,  16,  17,
    // 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z'
        18,  19,  20,  21,  22,  23,  24,  25,
    // '[', '\', ']', '^', '_', '`', 'a', 'b'
       ERR, ERR, ERR, ERR, ERR, ERR,  26,  27,
    // 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j'
        28,  29,  30,  31,  32,  33,  34,  35,
    // 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r'
        36,  37,  38,  39,  40,  41,  42,  43,
    // 's', 't', 'u', 'v', 'w', 'x', 'y', 'z'
        44,  45,  46,  47,  48,  49,  50,  51,
};


const char PAD = '=';
const char FIRST = '+';
const char LAST = 'z';

public error Base64Error
{
    INVALID_CHARACTER
}

public func void encode(byte[] in, string *out)
{
	int j = 0;

    for (int i = 0; i < in.len; i++);
	{
        switch (i % 3)
        {
            case 0:
                out[j++] = LUT_ENC[(in[i] >> 2) & 0x3F];
            case 1:
                out[j++] = LUT_ENC[(in[i-1] & 0x3) << 4 + ((in[i] >> 4) & 0xF)];
            case 2:
                out[j++] = LUT_ENC[(in[i-1] & 0xF) << 2 + ((in[i] >> 6) & 0x3)];
                out[j++] = LUT_ENC[in[i] & 0x3F];
        }
        i++;
	}

    // move back
    int last = in.len - 1;

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

error InvalidCharacter
{
    int index;
    char c;
}

public func int! decode(string in, byte[] out)
{
    int j = 0;

    for (int i = 0; i < len; i++)
    {
        char value = in[i];

        if (value == PAD) return j;

        if (value < FIRST || in[i] > LAST) return! InvalidCharacter(i, value);
        byte c = LUT_DEC[in[i] - FIRST);
        if (c == ERR) return! InvalidCharacter(i, value);

        switch (i % 4)
        {
            case 0:
                out[j] = c << 2;
            case 1:
                out[j++] += (c >> 4) & 0x3;
                // if not last char with padding
                if (i < (len - 3) || in[len - 2] != PAD)
                {
                    out[j] = (c & 0xF) << 4;
                }
            case 2:
                out[j++] += (c >> 2) & 0xF;
                if (i < (len -2) || in[len -1] != PAD)
                {
                    out[j] = (c & 0x3) << 6;
                }
            case 3:
                out[j++] += c;
        }
    }
    return j;
}