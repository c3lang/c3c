module std::crypto::dh;
import std::math::bigint;

fn BigInt generate_secret(BigInt p, BigInt x, BigInt y)
{
	return y.mod_pow(x, p);
}

fn BigInt public_key(BigInt p, BigInt g, BigInt x)
{
	return g.mod_pow(x, p);
}
