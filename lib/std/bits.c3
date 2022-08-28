module std::bits;

/**
 * @require types::is_intlike($typeof(i)) `The input must be an integer or integer vector`
 **/
macro popcount(i)
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
macro ctz(i) @builtin
{
	return $$ctz(i);
}

/**
 * @require types::is_intlike($typeof(i)) `The input must be an integer or integer vector`
 **/
macro clz(i) @builtin
{
	return $$clz(i);
}

/**
 * @require types::is_intlike($typeof(hi)) && types::is_intlike($typeof(lo)) && types::is_intlike($typeof(shift)) `The input must be an integer or integer vector`
 * @require types::is_same_type(hi, lo) `Hi and low arguments must have the same type`
 * @require types::is_same_type(hi, shift) `The shift value must have the same type as shifted types`
 **/
macro fshl(hi, lo, shift) @builtin
{
	return $$fshl(hi, lo, ($typeof(hi))shift);
}

/**
 * @require types::is_intlike($typeof(hi)) && types::is_intlike($typeof(lo)) && types::is_intlike($typeof(shift)) `The input must be an integer or integer vector`
 * @require types::is_same_type(hi, lo) `Hi and low arguments must have the same type`
 * @require types::is_same_type(hi, shift) `The shift value must have the same type as shifted types`
 **/
macro fshr(hi, lo, shift) @builtin
{
	return $$fshr(hi, lo, ($typeof(hi))shift);
}

/**
 * @require types::is_intlike($typeof(i)) && types::is_intlike($typeof(shift)) `The input must be an integer or integer vector`
 * @require types::is_same_type(i, shift) `The shift value must have the same type as shifted types`
 **/
macro rotl(i, shift) @builtin
{
	return $$fshl(i, i, shift);
}

/**
 * @require types::is_intlike($typeof(i)) && types::is_intlike($typeof(shift)) `The input must be an integer or integer vector`
 * @require types::is_same_type(i, shift) `The shift value must have the same type as shifted types`
 **/
macro rotr(i, shift) @builtin
{
	return $$fshr(i, i, shift);
}

