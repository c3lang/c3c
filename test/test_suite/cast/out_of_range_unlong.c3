fn int main()
{
	ulong x = ulong.max - 1; // #error: This expression (0xFFFFFFFFFFFFFFFF) will be implicitly converted to a signed type due to the right-hand side being signed
	ulong x1 = -1 + ulong.max; // #error: converted to a signed type due to the left-hand side being signed, but the value does
	return 0;
}