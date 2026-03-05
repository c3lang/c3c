module std::collections::pair <Type1, Type2>;
import std::io;

struct Pair (Printable)
{
	Type1 first;
	Type2 second;
}

fn usz? Pair.to_format(&self, Formatter* f) @dynamic
{
	return f.printf("{ %s, %s }", self.first, self.second);
}

<*
 @param [&out] a
 @param [&out] b
 @require $defined(*a = self.first) : "You cannot assign the first value to a"
 @require $defined(*b = self.second) : "You cannot assign the second value to b"
*>
macro void Pair.unpack(&self, a, b)
{
	*a = self.first;
	*b = self.second;
}

fn bool Pair.equal(self, Pair other) @operator(==) @if (types::has_equals(Type1) &&& types::has_equals(Type2))
{
	return self.first == other.first && self.second == other.second;
}

module std::collections::triple <Type1, Type2, Type3>;
import std::io;

struct Triple (Printable)
{
	Type1 first;
	Type2 second;
	Type3 third;
}

fn usz? Triple.to_format(&self, Formatter* f) @dynamic
{
	return f.printf("{ %s, %s, %s }", self.first, self.second, self.third);
}
<*
 @param [&out] a
 @param [&out] b
 @param [&out] c
 @require $defined(*a = self.first) : "You cannot assign the first value to a"
 @require $defined(*b = self.second) : "You cannot assign the second value to b"
 @require $defined(*c = self.third) : "You cannot assign the second value to c"
*>
macro void Triple.unpack(&self, a, b, c)
{
	*a = self.first;
	*b = self.second;
	*c = self.third;
}

fn bool Triple.equal(self, Triple other) @operator(==) @if (types::has_equals(Type1) &&& types::has_equals(Type2) &&& types::has_equals(Type3))
{
	return self.first == other.first && self.second == other.second && self.third == other.third;
}

module std::collections::tuple <Type1, Type2>;

struct Tuple @deprecated("Use 'Pair' instead")
{
	Type1 first;
	Type2 second;
}