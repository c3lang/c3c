module std::bits;

/**
 * @require types::is_intlike($typeof(i)) `The input must be an integer or integer vector`
 **/
macro popcount(i) @operator(intvec)
{
	return $$popcount(i);
}

/**
 * @require types::is_intlike($typeof(i)) `The input must be an integer or integer vector`
 **/
macro reverse(i)
{
	return $$bitreverse(i);
}

/**
 * @require types::is_intlike($typeof(i)) `The input must be an integer or integer vector`
 **/
macro bswap(i) @builtin
{
	return $$bswap(i);
}

/**
 * @require types::is_intlike($typeof(i)) `The input must be an integer or integer vector`
 **/
macro ctz(i) @operator(intvec) @builtin
{
	return $$ctz(i);
}

/**
 * @require types::is_intlike($typeof(i)) `The input must be an integer or integer vector`
 **/
macro clz(i) @operator(intvec) @builtin
{
	return $$clz(i);
}

/**
 * @require types::is_intlike($typeof(hi)) && types::is_intlike($typeof(lo)) && types::is_intlike($typeof(shift)) `The input must be an integer or integer vector`
 * @require types::@has_same(hi, lo, shift) `Hi, low and shift arguments must have the same type`
 **/
macro fshl(hi, lo, shift) @builtin
{
	return $$fshl(hi, lo, ($typeof(hi))shift);
}

/**
 * @require types::is_intlike($typeof(hi)) && types::is_intlike($typeof(lo)) && types::is_intlike($typeof(shift)) `The input must be an integer or integer vector`
 * @require types::@has_same(hi, lo, shift) `Hi, low and shift arguments must have the same type`
 **/
macro fshr(hi, lo, shift) @builtin
{
	return $$fshr(hi, lo, ($typeof(hi))shift);
}

/**
 * @require types::is_intlike($typeof(i)) && types::is_intlike($typeof(shift)) `The input must be an integer or integer vector`
 * @require types::@has_same(i, shift) `The shift value must have the same type as shifted types`
 **/
macro rotl(i, shift) @operator(intvec) @builtin
{
	return $$fshl(i, i, shift);
}

/**
 * @require types::is_intlike($typeof(i)) && types::is_intlike($typeof(shift)) `The input must be an integer or integer vector`
 * @require types::@has_same(i, shift) `The shift value must have the same type as shifted types`
 **/
macro rotr(i, shift) @operator(intvec) @builtin
{
	return $$fshr(i, i, shift);
}

