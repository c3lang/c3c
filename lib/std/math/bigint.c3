<*
Untested new big int implementation, missing unit tests. Etc
*>
module std::math::bigint;
import std::math::random;
import std::io;

const MAX_LEN = 4096 * 2 / 32;

const BigInt ZERO = { .len = 1 };
const BigInt ONE = { .len = 1, .data[0] = 1 };

struct BigInt (Printable)
{
	uint[MAX_LEN] data;
	uint len;
}

fn BigInt from_int(int128 val)
{
	BigInt b @noinit;
	b.init(val);
	return b;
}

fn BigInt* BigInt.init(&self, int128 value)
{
	self.data[..] = 0;
	int128 tmp = value;
	uint len = 0;
	while (tmp != 0 && len < MAX_LEN)
	{
		self.data[len] = (uint)(tmp & 0xFFFFFFFF);
		tmp >>= 32;
		len++;
	}
	assert(value < 0 || tmp == 0 || !self.is_negative());
	assert(value >= 0 || tmp == -1 || self.is_negative());
	self.len = len;
	self.reduce_len();
	return self;
}

fn BigInt* BigInt.init_with_u128(&self, uint128 value)
{
	self.data[..] = 0;
	uint128 tmp = value;
	uint len = 0;
	while (tmp != 0)
	{
		self.data[len] = (uint)(tmp & 0xFFFFFFFF);
		tmp >>= 32;
		len++;
	}
	assert(!self.is_negative());
	self.len = max(len, 1);
	return self;
}

<*
 @require values.len <= MAX_LEN
*>
fn BigInt* BigInt.init_with_array(&self, uint[] values)
{
	self.data[..] = 0;

	if (values.len == 0)
	{
		self.len = 1;
		return self;
	}

	self.len = values.len;

	foreach_r(i, val : values)
	{
		self.data[values.len - 1 - i] = val;
	}
	while (self.len > 1 && self.data[self.len - 1] == 0)
	{
		self.len--;
	}
	return self;
}

fn BigInt*? BigInt.init_string_radix(&self, String value, int radix)
{
	isz len = value.len;
	isz limit = value[0] == '-' ? 1 : 0;
	*self = ZERO;
	BigInt multiplier = ONE;
	BigInt radix_big = from_int(radix);
	for (isz i = len - 1; i >= limit; i--)
	{
		int pos_val = value[i];
		switch (pos_val)
		{
			case '0'..'9':
				pos_val -= '0';
			case 'A'..'Z':
				pos_val -= 'A' - 10;
			case 'a'..'z':
				pos_val -= 'a' - 10;
			default:
				return string::MALFORMED_INTEGER?;
		}
		if (pos_val >= radix) return string::MALFORMED_INTEGER?;
		if (limit == 1) pos_val = -pos_val;
		self.add_this(multiplier.mult(from_int(pos_val)));
		if (i - 1 >= limit)
		{
			multiplier.mult_this(radix_big);
		}
	}
	switch
	{
		case limit && !self.is_negative():
			return string::INTEGER_OVERFLOW?;
		case !limit && self.is_negative():
			return string::INTEGER_OVERFLOW?;
	}
	return self;
}

fn bool BigInt.is_negative(&self)
{
	return self.data[MAX_LEN - 1] & 0x80000000 != 0;
}

fn BigInt BigInt.add(self, BigInt other) @operator(+)
{
	self.add_this(other);
	return self;
}

fn void BigInt.add_this(&self, BigInt other) @operator(+=)
{
	bool sign = self.is_negative();
	bool sign_arg = other.is_negative();

	self.len = max(self.len, other.len);

	ulong carry = 0;
	for (uint i = 0; i < self.len; i++)
	{
		ulong sum = (ulong)self.data[i] + (ulong)other.data[i] + carry;
		carry = sum >> 32;
		self.data[i] = (uint)(sum & 0xFFFFFFFF);
	}

	if (carry != 0 && self.len < MAX_LEN)
	{
		self.data[self.len++] = (uint)carry;
	}

	self.reduce_len();

	assert(sign != sign_arg || sign == self.is_negative(), "Overflow in addition");
}

fn void BigInt.reduce_len(&self) @private
{
	self.len = max(find_length(&self.data, self.len), 1);
}

macro uint find_length(uint* data, uint length)
{
	while (length > 1 && data[length - 1] == 0)
	{
		--length;
	}
	return length;
}

fn BigInt BigInt.mult(self, BigInt bi2) @operator(*)
{
	self.mult_this(bi2);
	return self;
}

fn void BigInt.mult_this(&self, BigInt bi2) @operator(*=)
{
	if (bi2.is_zero())
	{
		*self = ZERO;
		return;
	}
	if (bi2.is_one()) return;

	BigInt res = ZERO;

	bool negative_sign = false;

	if (self.is_negative())
	{
		self.negate();
		negative_sign = !negative_sign;
	}
	if (bi2.is_negative())
	{
		bi2.negate();
		negative_sign = !negative_sign;
	}

	// multiply the absolute values
	for (uint i = 0; i < self.len; i++)
	{
		if (self.data[i] == 0) continue;
		ulong mcarry = 0;
		for (int j = 0, int k = i; j < bi2.len; j++, k++)
		{
			// k = i + j
			ulong bi1_val = (ulong)self.data[i];
			ulong bi2_val = (ulong)bi2.data[j];
			ulong res_val = (ulong)res.data[k];
			ulong val = (bi1_val * bi2_val) + res_val + mcarry;
			res.data[k] = (uint)(val & 0xFFFFFFFF);
			mcarry = val >> 32;
		}

		if (mcarry != 0)
		{
			res.data[i + bi2.len] = (uint)mcarry;
		}
	}

	res.len = min(self.len + bi2.len, (uint)MAX_LEN);

	res.reduce_len();

	// overflow check (result is -ve)
	assert(!res.is_negative(), "Multiplication overflow");

	if (negative_sign)
	{
		res.negate();
	}
	*self = res;
}

fn void BigInt.negate(&self)
{
	if (self.is_zero()) return;

	bool was_negative = self.is_negative();
	// 1's complement
	for (uint i = 0; i < MAX_LEN; i++)
	{
		self.data[i] = (uint)~(self.data[i]);
	}
	// add one to result of 1's complement
	ulong carry = 1;
	int index = 0;

	while (carry != 0 && index < MAX_LEN)
	{
		ulong val = self.data[index];
		val++;

		self.data[index] = (uint)(val & 0xFFFFFFFF);
		carry = val >> 32;
		index++;
	}

	assert(self.is_negative() != was_negative, "Overflow in negation");

	self.len = MAX_LEN;
	self.reduce_len();
}

macro bool BigInt.is_zero(&self) => self.len == 1 && self.data[0] == 0;

fn BigInt BigInt.sub(self, BigInt other) @operator(-)
{
	self.sub_this(other);
	return self;
}

fn BigInt* BigInt.sub_this(&self, BigInt other) @operator(-=)
{
	self.len = max(self.len, other.len);

	bool sign = self.is_negative();
	bool sign_arg = other.is_negative();

	long carry_in = 0;
	for (int i = 0; i < self.len; i++)
	{
		long diff = (long)self.data[i] - (long)other.data[i] - carry_in;
		self.data[i] = (uint)(diff & 0xFFFFFFFF);
		carry_in = diff < 0 ? 1 : 0;
	}

	// roll over to negative
	if (carry_in != 0)
	{
		for (uint i = self.len; i < MAX_LEN; i++)
		{
			self.data[i] = 0xFFFFFFFF;
		}
		self.len = MAX_LEN;
	}

	self.reduce_len();

	// overflow check

	assert(sign == sign_arg || sign == self.is_negative(), "Overflow in subtraction");
	return self;
}

fn int BigInt.bitcount(&self)
{
	self.reduce_len();
	uint val = self.data[self.len - 1];
	uint mask = 0x80000000;
	int bits = 32;

	while (bits > 0 && (val & mask) == 0)
	{
		bits--;
		mask >>= 1;
	}
	bits += (self.len - 1) << 5;
	return bits;
}

fn BigInt BigInt.unary_minus(&self) @operator(-)
{
	if (self.is_zero()) return *self;
	BigInt result = *self;
	result.negate();
	return result;
}


macro BigInt BigInt.div(self, BigInt other) @operator(/)
{
	self.div_this(other);
	return self;
}

fn void BigInt.div_this(&self, BigInt other) @operator(/=)
{
	bool negate_answer = self.is_negative();

	if (negate_answer)
	{
		self.negate();
	}
	if (other.is_negative())
	{
		negate_answer = !negate_answer;
		other.negate();
	}

	if (self.less_than(other))
	{
		*self = ZERO;
	}

	BigInt quotient = ZERO;
	BigInt remainder = ZERO;

	if (other.len == 1)
	{
		self.single_byte_divide(&other, &quotient, &remainder);
	}
	else
	{
		self.multi_byte_divide(&other, &quotient, &remainder);
	}
	if (negate_answer)
	{
		quotient.negate();
	}
	*self = quotient;
}

fn BigInt BigInt.mod(self, BigInt bi2) @operator(%)
{
	self.mod_this(bi2);
	return self;
}

fn void BigInt.mod_this(&self, BigInt bi2) @operator(%=)
{
	if (bi2.is_negative())
	{
		bi2.negate();
	}

	bool negate_answer = self.is_negative();
	if (negate_answer)
	{
		self.negate();
	}

	if (self.less_than(bi2))
	{
		if (negate_answer) self.negate();
		return;
	}

	BigInt quotient = ZERO;
	BigInt remainder = ZERO;

	if (bi2.len == 1)
	{
		self.single_byte_divide(&bi2, &quotient, &remainder);
	}
	else
	{
		self.multi_byte_divide(&bi2, &quotient, &remainder);
	}
	if (negate_answer)
	{
		remainder.negate();
	}
	*self = remainder;
}

fn void BigInt.bit_negate_this(&self)
{
	foreach (&r : self.data) *r = ~*r;

	self.len = MAX_LEN;
	self.reduce_len();
}

fn BigInt BigInt.bit_negate(self) @operator(~)
{
	self.bit_negate_this();
	return self;
}

fn BigInt BigInt.shr(self, int shift) @operator(>>)
{
	self.shr_this(shift);
	return self;
}

fn void BigInt.shr_this(self, int shift) @operator(>>=)
{
	self.len = shift_right(&self.data, self.len, shift);
}

fn BigInt BigInt.shl(self, int shift) @operator(<<)
{
	self.shl_this(shift);
	return self;
}

macro bool BigInt.equals(&self, BigInt other) @operator(==)
{
	if (self.len != other.len) return false;
	return self.data[:self.len] == other.data[:self.len];
}

macro bool BigInt.greater_than(&self, BigInt other)
{
	if (self.is_negative() && !other.is_negative()) return false;
	if (!self.is_negative() && other.is_negative()) return true;
	int pos;
	// same sign
	int len = max(self.len, other.len);
	for (pos = len - 1; pos >= 0 && self.data[pos] == other.data[pos]; pos--);
	return pos >= 0 && self.data[pos] > other.data[pos];
}
macro bool BigInt.less_than(&self, BigInt other)
{
	if (self.is_negative() && !other.is_negative()) return true;
	if (!self.is_negative() && other.is_negative()) return false;

	// same sign
	int len = max(self.len, other.len);
	int pos;
	for (pos = len - 1; pos >= 0 && self.data[pos] == other.data[pos]; pos--);
	return pos >= 0 && self.data[pos] < other.data[pos];
}

fn bool BigInt.is_odd(&self)
{
	return (self.data[0] & 0x1) != 0;
}


fn bool BigInt.is_one(&self)
{
	return self.len == 1 && self.data[0] == 1;
}


macro bool BigInt.greater_or_equal(&self, BigInt other)
{
	return self.greater_than(other) || self.equals(other);
}

macro bool BigInt.less_or_equal(&self, BigInt)
{
	return self.less_than(other) || self.equals(other);
}

fn BigInt BigInt.abs(&self)
{
	return self.is_negative() ? self.unary_minus() : *self;
}

fn usz? BigInt.to_format(&self, Formatter* format) @dynamic
{
	@stack_mem(4100; Allocator mem)
	{
	   return format.print(self.to_string_with_radix(10, mem));
	};
}

fn String BigInt.to_string(&self, Allocator allocator) @dynamic
{
	return self.to_string_with_radix(10, allocator);
}

<*
 @require radix > 1 && radix <= 36 : "Radix must be 2-36"
*>
fn String BigInt.to_string_with_radix(&self, int radix, Allocator allocator)
{
	if (self.is_zero()) return "0".copy(allocator);

	const char[*] CHARS = "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
	@stack_mem(4100; Allocator mem)
	{
		BigInt a = *self;
		DString str;
		str.init(mem, 4096);
		bool negative = self.is_negative();
		if (negative)
		{
			a.negate();
		}
		BigInt quotient = ZERO;
		BigInt remainder = ZERO;
		BigInt big_radix = from_int(radix);

		while (!a.is_zero())
		{
			a.single_byte_divide(&big_radix, &quotient, &remainder);

			if (remainder.data[0] < 10)
			{
				str.append((char)(remainder.data[0] + '0'));
			}
			else
			{
				str.append(CHARS[remainder.data[0] - 10]);
			}
			a = quotient;
		}
		if (negative) str.append("-");
		str.reverse();
		return str.copy_str(allocator);
	};
}

<*
 @require !exp.is_negative() : "Positive exponents only"
*>
fn BigInt BigInt.mod_pow(&self, BigInt exp, BigInt mod)
{
	BigInt result_num = ONE;

	bool was_neg = self.is_negative();
	BigInt num = *self;
	if (was_neg)
	{
		num.negate();
	}

	if (mod.is_negative())
	{
		mod.negate();
	}

	num.mod_this(mod);

	// calculate constant = b^(2k) / m
	BigInt constant = ZERO;

	uint i = mod.len << 1;
	constant.data[i] = 0x00000001;
	constant.len = i + 1;

	constant.div_this(mod);
	int total_bits = exp.bitcount();
	int count = 0;

	// perform squaring and multiply exponentiation
	for (int pos = 0; pos < exp.len; pos++)
	{
		uint mask = 0x01;
		for (int index = 0; index < 32; index++)
		{
			if (exp.data[pos] & mask != 0)
			{
				result_num = barrett_reduction(result_num.mult(num), mod, constant);
			}

			mask <<= 1;

			num = barrett_reduction(num.mult(num), mod, constant);

			if (num.len == 1 && num.data[0] == 1)
			{
				if (was_neg && (exp.data[0] & 0x1) != 0)
				{
					//odd exp
					result_num.negate();
				}
				return result_num;
			}
			count++;
			if (count == total_bits) break;
		}
	}

	if (was_neg && exp.is_odd())
	{
		//odd exp
		result_num.negate();
	}
	return result_num;
}

<*
 Fast calculation of modular reduction using Barrett's reduction.
 Requires x < b^(2k), where b is the base.  In this case, base is
 2^32 (uint).
*>
fn BigInt barrett_reduction(BigInt x, BigInt n, BigInt constant)
{
	int k = n.len;
	int k_plus_one = k + 1;
	int k_minus_one = k - 1;

	BigInt q1  = ZERO;
	// q1 = x / b^(k-1)
	q1.len = x.len - k_minus_one;
	if (q1.len == 0)
	{
		q1.len = 1;
	}
	else
	{
		q1.data[:q1.len] = x.data[k_minus_one:q1.len];
	}

	BigInt q2 = q1.mult(constant);
	BigInt q3 = ZERO;

	// q3 = q2 / b^(k+1)
	int length = q2.len - k_plus_one;
	assert(length >= 0);
	if (length)
	{
		q3.data[:length] = q2.data[k_plus_one:length];
		q3.len = length;
	}
	else
	{
		q3.len = 1;
	}

	// r1 = x mod b^(k+1)
	// i.e. keep the lowest (k+1) words
	BigInt r1 = ZERO;
	int length_to_copy = (x.len > k_plus_one) ? k_plus_one : x.len;
	assert(length_to_copy >= 0);
	r1.data[:length_to_copy] = x.data[0:length_to_copy];
	r1.len = length_to_copy;

	// r2 = (q3 * n) mod b^(k+1)
	// partial multiplication of q3 and n

	BigInt r2 = ZERO;
	for (int i = 0; i < q3.len; i++)
	{
		if (q3.data[i] == 0) continue;

		ulong mcarry = 0;
		int t = i;
		for (int j = 0; j < n.len && t < k_plus_one; j++, t++)
		{
			// t = i + j
			ulong val = (ulong)q3.data[i] * n.data[j] + r2.data[t] + mcarry;

			r2.data[t] = (uint)(val & 0xFFFFFFFF);
			mcarry = val >> 32;
		}

		if (t < k_plus_one)
		{
			r2.data[t] = (uint)mcarry;
		}
	}

	r2.len = k_plus_one;
	r2.reduce_len();

	r1.sub_this(r2);
	if (r1.is_negative())
	{
		BigInt val = ZERO;
		val.data[k_plus_one] = 0x00000001;
		val.len = k_plus_one + 1;
		r1.add_this(val);
	}

	while (r1.greater_or_equal(n))
	{
		r1.sub_this(n);
	}

	return r1;
}

fn BigInt BigInt.sqrt(&self)
{
	uint num_bits = self.bitcount();

	num_bits = num_bits & 0x1 != 0 ? num_bits >> 1 + 1 : num_bits >> 1;

	uint byte_pos = num_bits >> 5;
	int bit_pos = num_bits & 0x1F;

	uint mask;
	BigInt result = ZERO;

	if (bit_pos == 0)
	{
		mask = 0x80000000;
	}
	else
	{
		mask = 1U << bit_pos;
		byte_pos++;
	}
	result.len = byte_pos;

	for (int i = byte_pos - 1; i >= 0; i--)
	{
		while (mask != 0)
		{
			// guess
			result.data[i] ^= mask;

			// undo the guess if its square is larger than this
			if (result.mult(result).greater_than(*self))
			{
				result.data[i] ^= mask;
			}
			mask >>= 1;
		}
		mask = 0x80000000;
	}
	return result;
}

fn BigInt BigInt.bit_and(self, BigInt bi2) @operator(&)
{
	self.bit_and_this(bi2);
	return self;
}

fn void BigInt.bit_and_this(&self, BigInt bi2)
{
	uint len = max(self.len, bi2.len);
	foreach (i, &ref : self.data[:len])
	{
		*ref = *ref & bi2.data[i];
	}
	self.len = MAX_LEN;

	self.reduce_len();
}

fn BigInt BigInt.bit_or(self, BigInt bi2) @operator(|)
{
	self.bit_or_this(bi2);
	return self;
}

fn void BigInt.bit_or_this(&self, BigInt bi2)
{
	uint len = max(self.len, bi2.len);
	foreach (i, &ref : self.data[:len])
	{
		*ref = *ref | bi2.data[i];
	}
	self.len = MAX_LEN;

	self.reduce_len();
}

fn BigInt BigInt.bit_xor(self, BigInt bi2) @operator(^)
{
	self.bit_xor_this(bi2);
	return self;
}

fn void BigInt.bit_xor_this(&self, BigInt bi2)
{
	uint len = max(self.len, bi2.len);
	foreach (i, &ref : self.data[:len])
	{
		*ref = *ref ^ bi2.data[i];
	}
	self.len = MAX_LEN;

	self.reduce_len();
}

fn void BigInt.shl_this(&self, int shift) @operator(<<=)
{
	self.len = shift_left(&self.data, self.len, shift);
}

fn BigInt BigInt.gcd(&self, BigInt other)
{
	BigInt x = self.abs();
	BigInt y = other.abs();
	BigInt g = y;
	while (!x.is_zero())
	{
		g = x;
		x = y.mod(x);
		y = g;
	}
	return g;
}

fn BigInt BigInt.lcm(&self, BigInt other)
{
	BigInt x = self.abs();
	BigInt y = other.abs();
	BigInt g = y.mult(x);
	return g.div(x.gcd(y));
}

<*
 @require bits >> 5 < MAX_LEN : "Required bits > maxlength"
*>
fn void BigInt.randomize_bits(&self, Random random, int bits)
{
	int dwords = bits >> 5;
	int rem_bits = bits & 0x1F;

	if (rem_bits != 0)
	{
		dwords++;
	}

	for (int i = 0; i < dwords; i++)
	{
		self.data[i] = random.next_int();
	}

	for (int i = dwords; i < MAX_LEN; i++)
	{
		self.data[i] = 0;
	}

	if (rem_bits != 0)
	{
		uint mask = (uint)(0x01 << (rem_bits - 1));
		self.data[dwords - 1] |= mask;

		mask = (uint)(0xFFFFFFFF >> (32 - rem_bits));
		self.data[dwords - 1] &= mask;
	}
	else
	{
		self.data[dwords - 1] |= 0x80000000;
	}

	self.len = (int)dwords;

	if (self.len == 0)
	{
		self.len = 1;
	}
}

module std::math::bigint @private;


<*
 @param [&out] quotient
 @param [&in] bi2
 @param [&out] remainder
*>
fn void BigInt.single_byte_divide(&self, BigInt* bi2, BigInt* quotient, BigInt* remainder)
{
	uint[MAX_LEN] result;
	int result_pos = 0;

	// copy dividend to reminder
	*remainder = *self;

	while (remainder.len > 1 && remainder.data[remainder.len - 1] == 0)
	{
		remainder.len--;
	}

	ulong divisor = bi2.data[0];
	int pos = remainder.len - 1;
	ulong dividend = remainder.data[pos];

	if (dividend >= divisor)
	{
		ulong q = dividend / divisor;
		result[result_pos++] = (uint)q;
		remainder.data[pos] = (uint)(dividend % divisor);
	}
	pos--;

	while (pos >= 0)
	{
		dividend = (ulong)remainder.data[pos + 1] << 32 + remainder.data[pos];
		ulong q = dividend / divisor;
		result[result_pos++] = (uint)q;

		remainder.data[pos + 1] = 0;
		remainder.data[pos--] = (uint)(dividend % divisor);
	}

	quotient.len = result_pos;
	uint j = 0;
	for (int i = result_pos - 1; i >= 0; i--, j++)
	{
		quotient.data[j] = result[i];
	}

	quotient.data[j..] = 0;
	quotient.reduce_len();
	remainder.reduce_len();
}

<*
 @param [&out] quotient
 @param [&in] other
 @param [&out] remainder
*>
fn void BigInt.multi_byte_divide(&self, BigInt* other, BigInt* quotient, BigInt* remainder)
{
	uint[MAX_LEN] result;
	uint[MAX_LEN] r;
	uint[MAX_LEN] dividend_part;

	int remainder_len = self.len + 1;

	uint mask = 0x80000000;
	uint val = other.data[other.len - 1];
	int shift = 0;
	int result_pos = 0;

	while (mask != 0 && (val & mask) == 0)
	{
		shift++;
		mask >>= 1;
	}

	r[:self.len] = self.data[:self.len];

	remainder_len = shift_left(&r, remainder_len, shift);

	BigInt bi2 = other.shl(shift);

	int j = remainder_len - bi2.len;
	int pos = remainder_len - 1;

	ulong first_divisor_byte = bi2.data[bi2.len - 1];
	ulong second_divisor_byte = bi2.data[bi2.len - 2];

	int divisor_len = bi2.len + 1;

	while (j > 0)
	{
		ulong dividend = (ulong)r[pos] << 32 + r[pos - 1];

		ulong q_hat = dividend / first_divisor_byte;
		ulong r_hat = dividend % first_divisor_byte;

		bool done = false;
		while (!done)
		{
			done = true;

			if (q_hat == 0x10000000 || (q_hat * second_divisor_byte) > (r_hat << 32 + r[pos - 2]))
			{
				q_hat--;
				r_hat += first_divisor_byte;

				if (r_hat < 0x10000000) done = false;
			}
		}

		for (int h = 0; h < divisor_len; h++)
		{
			dividend_part[h] = r[pos - h];
		}

		BigInt kk = { dividend_part, divisor_len };
		BigInt ss = bi2.mult(from_int(q_hat));

		while (ss.greater_than(kk))
		{
			q_hat--;

			ss.sub_this(bi2);
		}
		BigInt yy = kk.sub(ss);

		for (int h = 0; h < divisor_len; h++)
		{
			r[pos - h] = yy.data[bi2.len - h];
		}

		result[result_pos++] = (uint)q_hat;

		pos--;
		j--;

	}

	quotient.len = result_pos;
	uint y = 0;

	for (int x = quotient.len - 1; x >= 0; x--, y++)
	{
		quotient.data[y] = result[x];
	}

	for (; y < MAX_LEN; y++)
	{
		quotient.data[y] = 0;
	}

	quotient.reduce_len();

	remainder.len = shift_right(&r, remainder_len, shift);

	remainder.data[:remainder.len] = r[:remainder.len];

	remainder.data[y..] = 0;
}

fn int shift_left(uint* data, int len, int shift_val) @inline
{
	int shift_amount = 32;
	int buf_len = len;

	while (buf_len > 1 && data[buf_len - 1] == 0) buf_len--;

	for (int count = shift_val; count > 0;)
	{
		if (count < shift_amount) shift_amount = count;

		ulong carry = 0;
		for (int i = 0; i < buf_len; i++)
		{
			ulong val = (ulong)data[i] << shift_amount;
			val |= carry;

			data[i] = (uint)(val & 0xFFFFFFFF);
			carry = val >> 32;
		}

		if (carry != 0)
		{
			if (buf_len + 1 <= len)
			{
				data[buf_len++] = (uint)carry;
			}
		}
		count -= shift_amount;
	}
	return buf_len;
}

fn int shift_right(uint* data, int len, int shift_val) @inline
{
	int shift_amount = 32;
	int inv_shift = 0;
	int buf_len = len;

	while (buf_len > 1 && data[buf_len - 1] == 0)
	{
		buf_len--;
	}

	for (int count = shift_val; count > 0;)
	{
		if (count < shift_amount)
		{
			shift_amount = count;
			inv_shift = 32 - shift_amount;
		}

		ulong carry = 0;
		for (int i = buf_len - 1; i >= 0; i--)
		{
			ulong val = (ulong)data[i] >> shift_amount;
			val |= carry;

			carry = (ulong)data[i] << inv_shift;
			data[i] = (uint)(val);
		}

		count -= shift_amount;
	}
	return find_length(data, buf_len);
}
