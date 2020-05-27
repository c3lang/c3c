module bigint;

macro max(a, b)
{
	return a > b ? a : b;
}

// Horribly bad implementation of BigInt with add/sub.
public struct BigInt @opaque
{
	byte* number;
	uint length;
	char sign;
}

public func void BigInt.init(BigInt* bigInt)
{
	bigInt.number = malloc(1);
	bigInt.number[0] = 0;
	bigInt.length = 1;
	bigInt.sign = 1;
}

public func void BigInt.initFromString(BigInt* bigInt, char* str)
{
	uint size = strlen(str);
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

public func void BigInt.copyTo(BigInt* source, BigInt* target)
{
	target.number = realloc(target.number, source.length);
	target.sign = source.sign;
	target.length = source.length;
	for (uint i = 0; i < target.length; i++) target.number[i] = source.number[i];
}

public func void BigInt.destroy(BigInt* bigInt)
{
	free(bigInt.number);
}

func void BigInt.addIgnoreSign(BigInt* a, BigInt* b, BigInt* result)
{
	uint length = @max(a.length, b.length) + 1;
	byte* res = malloc(length);
	char carry = 0;
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

public func void BigInt.getMaxVal(BigInt* bigInt, uint* pos, int* val)
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

public func char BigInt.compare(BigInt* a, BigInt* b)
{
	if (a.sign != b.sign) return a.sign;
	byte aMax;
	uint aMaxPos;
	a.getMaxVal(&aMaxPos, &aMax);
	if (aMaxPos >= b.length) return a.sign;
	byte bMax;
	uint bMaxPos;
	b.getMaxVal(&bMaxPos, &bMax);
	if (aMaxPos > bMaxPos) return a.sign;
	if (aMaxPos < bMaxPos) return -a.sign;
	if (aMax > bMax) return a.sign;
	if (aMax < bMax) return -a.sign;
	return 0;
}

public func char BigInt.compareNoSign(BigInt* a, BigInt* b)
{
	byte aMax;
	uint aMaxPos;
	a.getMaxVal(&aMaxPos, &aMax);
	if (aMaxPos >= b.length) return 1;
	byte bMax;
	uint bMaxPos;
	b.getMaxVal(&bMaxPos, &bMax);
	if (aMaxPos > bMaxPos) return 1;
	if (aMaxPos < bMaxPos) return -1;
	if (aMax > bMax) return 1;
	if (aMax < bMax) return -1;
	return 0;
}

func void BigInt.subIgnoreSign(BigInt* a, BigInt* b, BigInt* result)
{
	uint length = @max(a.length, b.length);
	byte* res = malloc(length);

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

	byte borrow = 0;
	for (uint i = 0; i < length; i++)
	{
		byte aValue = i >= x.length ? 0 : x.number[i];
		byte bValue = borrow + (i >= y.length ? 0 : y.number[i]);
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

public func void BigInt.add(BigInt* a, BigInt* b, BigInt* result)
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

public func char* BigInt.toCharArray(BigInt* bigInt)
{
	uint charLen = bigInt.length + 1 + (bigInt.sign < 0 ? 1 : 0);
	byte* out = malloc(charLen);
	out[charLen - 1] = '\0';
	byte* start = out;
	if (bigInt.sign < 0)
	{
		out[0] = '-';
		start++;
	}
	bool nonZeroFound = false;
	for (uint i = bigInt.length; i > 0; i--)
	{
		byte digit = bigInt.number[i - 1];
		if (i > 1 && !nonZeroFound && digit == 0) continue;
		nonZeroFound = true;
		*(start++) = digit + '0';
	}
	return out;
}

public func void BigInt.print(BigInt* bigInt)
{
	char* chars = bigInt.toCharArray();
	puts(chars);
	free(chars);
}

/*
public func void BigInt.fprint(BigInt* bigInt, FILE* file)
{
   char* chars = bigInt.toCharArray();
   fputs(chars, file);
   putc('\n', file);
   free(chars);
}*/


public func int main(int size, char** args)
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
		printf("%d : ", i);
		current.print();
		minus1.copyTo(&minus2);
		current.copyTo(&minus1);
	}
	return 0;
}
