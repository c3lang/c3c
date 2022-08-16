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
