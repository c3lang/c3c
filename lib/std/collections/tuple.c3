module std::collections::pair{Type1, Type2};

struct Pair
{
	Type1 first;
	Type2 second;
}

<*
 @param [&out] a
 @param [&out] b
 @require $assignable(self.first, $typeof(*a)) : "You cannot assign the first value to a"
 @require $assignable(self.second, $typeof(*b)) : "You cannot assign the second value to b"
*>
macro void Pair.unpack(&self, a, b)
{
	*a = self.first;
	*b = self.second;
}

module std::collections::triple{Type1, Type2, Type3};

struct Triple
{
	Type1 first;
	Type2 second;
	Type3 third;
}

<*
 @param [&out] a
 @param [&out] b
 @param [&out] c
 @require $assignable(self.first, $typeof(*a)) : "You cannot assign the first value to a"
 @require $assignable(self.second, $typeof(*b)) : "You cannot assign the second value to b"
 @require $assignable(self.third, $typeof(*c)) : "You cannot assign the second value to c"
*>
macro void Triple.unpack(&self, a, b, c)
{
	*a = self.first;
	*b = self.second;
	*c = self.third;
}

module std::collections::tuple{Type1, Type2};

struct Tuple @deprecated("Use 'Pair' instead")
{
	Type1 first;
	Type2 second;
}
