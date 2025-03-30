module std::math::uuid;
import std::math::random @public;
import std::io;

typedef Uuid (Printable) = char[16];

<*
Generate a version 4 UUID from the default random.
*>
fn Uuid generate()
{
	random::init_default_random();
	return generate_from_random(&random::default_random);
}

<*
Generate a version 4 UUID from the given random.
*>
fn Uuid generate_from_random(Random random)
{
	Uuid uuid;
	random.next_bytes(&(char[16])uuid);
	uuid[6] = (uuid[6] & 0b0000_1111) | 0b0100_0000;
	uuid[8] = (uuid[8] & 0b0011_1111) | 0b1000_0000;
	return uuid;
}

fn usz? Uuid.to_format(&self, Formatter* formatter) @dynamic
{
	return formatter.printf("%02x%02x%02x%02x-%02x%02x-%02x%02x-%02x%02x-%02x%02x%02x%02x%02x%02x",
		(*self)[0], (*self)[1], (*self)[2], (*self)[3],
		(*self)[4], (*self)[5],
		(*self)[6], (*self)[7],
		(*self)[8], (*self)[9],
		(*self)[10], (*self)[11], (*self)[12], (*self)[13], (*self)[14], (*self)[15]);
}

fn String Uuid.to_string(&self, Allocator allocator)
{
	return string::format(allocator, "%s", *self);
}