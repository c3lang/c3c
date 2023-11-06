module bigint;
import libc;
import std::io;

macro @max(a, b)
{
	return a > b ? a : b;
}

// Horribly bad implementation of BigInt with add/sub.
struct BigInt
{
	char* number;
	uint length;
	ichar sign;
}

fn void BigInt.init(BigInt* bigInt)
{
	bigInt.number = malloc(1);
	bigInt.number[0] = 0;
	bigInt.length = 1;
	bigInt.sign = 1;
}

fn void BigInt.initFromString(BigInt* bigInt, ZString str)
{
	uint size = (uint)str.len();
	bigInt.sign = 1;
	switch (str[0])
	{
		case '-':
			bigInt.sign = -1;
			size--;
			str++;
		case '+':
			size--;
			str++;
		default:
			break;
	}
	char* res = malloc(size);
	for (uint i = 0; i < size; i++)
	{
		res[i] = str[size - i - 1] - '0';
	}
	bigInt.number = res;
	bigInt.length = size;
}

fn void BigInt.copyTo(BigInt* source, BigInt* target)
{
	target.number = realloc(target.number, source.length);
	target.sign = source.sign;
	target.length = source.length;
	for (uint i = 0; i < target.length; i++) target.number[i] = source.number[i];
}

fn void BigInt.destroy(BigInt* bigInt)
{
	free(bigInt.number);
}

fn void BigInt.addIgnoreSign(BigInt* a, BigInt* b, BigInt* result)
{
	uint length = @max(a.length, b.length) + 1;
	char* res = malloc(length);
	ichar carry = 0;
	BigInt* x;
	BigInt* y;
	if (a.length > b.length)
	{
		x = a;
		y = b;
	}
	else
	{
		x = b;
		y = a;
	}
	for (uint i = 0; i < length; i++)
	{
		if (i >= y.length)
		{
			res[i] = carry + (i >= x.length ? 0 : x.number[i]);
		}
		else
		{
			res[i] = x.number[i] + y.number[i] + carry;
		}
		carry = 0;
		if (res[i] > 9)
		{
			carry = 1;
			res[i] -= 10;
		}
	}
	result.destroy();
	result.number = res;
	result.length = length;
}

fn void BigInt.getMaxVal(BigInt* bigInt, uint* pos, int* val)
{
	for (uint i = bigInt.length; i > 0; i++)
	{
		if (bigInt.number[i] != 0)
		{
			*pos = i;
			*val = bigInt.number[i];
			return;
		}
	}
	*pos = 0;
	*val = 0;
}

fn ichar BigInt.compare(BigInt* a, BigInt* b)
{
	if (a.sign != b.sign) return a.sign;
	int aMax;
	uint aMaxPos;
	a.getMaxVal(&aMaxPos, &aMax);
	if (aMaxPos >= b.length) return a.sign;
	int bMax;
	uint bMaxPos;
	b.getMaxVal(&bMaxPos, &bMax);
	if (aMaxPos > bMaxPos) return a.sign;
	if (aMaxPos < bMaxPos) return -a.sign;
	if (aMax > bMax) return a.sign;
	if (aMax < bMax) return -a.sign;
	return 0;
}

fn ichar BigInt.compareNoSign(BigInt* a, BigInt* b)
{
	int aMax;
	uint aMaxPos;
	a.getMaxVal(&aMaxPos, &aMax);
	if (aMaxPos >= b.length) return 1;
	int bMax;
	uint bMaxPos;
	b.getMaxVal(&bMaxPos, &bMax);
	if (aMaxPos > bMaxPos) return 1;
	if (aMaxPos < bMaxPos) return -1;
	if (aMax > bMax) return 1;
	if (aMax < bMax) return -1;
	return 0;
}

fn void BigInt.subIgnoreSign(BigInt* a, BigInt* b, BigInt* result)
{
	uint length = @max(a.length, b.length);
	char* res = malloc(length);

	BigInt* x;
	BigInt* y;
	if (a.compareNoSign(b) < 0)
	{
		result.sign = -1;
		x = b;
		y = a;
	}
	else
	{
		result.sign = 1;
		x = a;
		y = b;
	}

	char borrow = 0;
	for (uint i = 0; i < length; i++)
	{
		char aValue = i >= x.length ? 0 : x.number[i];
		char bValue = borrow + (i >= y.length ? 0 : y.number[i]);
		if (bValue > aValue)
		{
			borrow = 1;
			aValue += 10;
		}
		else
		{
			borrow = 0;
		}
		res[i] = aValue - bValue;
	}
	result.destroy();
	result.number = res;
	result.length = length;
}

fn void BigInt.add(BigInt* a, BigInt* b, BigInt* result)
{
    if (a.sign == b.sign)
    {
		a.addIgnoreSign(b, result);
        result.sign = a.sign;
        return;
    }
    if (a.sign < 0)
    {
        b.subIgnoreSign(a, result);
    }
    else
    {
        a.subIgnoreSign(a, result);
    }
}

fn char* BigInt.toCharArray(BigInt* bigInt)
{
	uint icharLen = bigInt.length + 1 + (bigInt.sign < 0 ? 1 : 0);
	char* out = malloc(icharLen);
	out[icharLen - 1] = '\0';
	char* start = out;
	if (bigInt.sign < 0)
	{
		out[0] = '-';
		start++;
	}
	bool nonZeroFound = false;
	for (uint i = bigInt.length; i > 0; i--)
	{
		char digit = bigInt.number[i - 1];
		if (i > 1 && !nonZeroFound && digit == 0) continue;
		nonZeroFound = true;
		*(start++) = digit + '0';
	}
	return out;
}

fn void BigInt.print(BigInt* bigInt)
{
	ZString chars = (ZString)bigInt.toCharArray();
	io::printn(chars);
	free(chars);
}

/*
fn void BigInt.fprint(BigInt* bigInt, FILE* file)
{
   ichar* ichars = bigInt.toicharArray();
   fputs(ichars, file);
   putc('\n', file);
   free(ichars);
}*/


fn int main(int size, char** args)
{
	BigInt minus2;
	BigInt minus1;
	BigInt current;
	minus2.initFromString("0");
	minus1.initFromString("1");
	current.initFromString("0");


	for (int i = 1; i <= 500; i++)
	{
		minus1.add(&minus2, &current);
		libc::printf("%d : ", i);
		current.print();
		minus1.copyTo(&minus2);
		current.copyTo(&minus1);
	}
	return 0;
}
