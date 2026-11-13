module std::test;
fn void main()
{
	int a = 1;
	int b = 2;
	$$memcpy(&a, &b, (usz)4, false, (usz)3, (usz)0); // #error: power-of-two alignment
	$$memcpy(&a, &b, (usz)4, false, (usz)4, (usz)6); // #error: power-of-two alignment
	$$memcpy_inline(&a, &b, (usz)4, false, (usz)5, (usz)0); // #error: power-of-two alignment
	$$memmove(&a, &b, (usz)4, false, (usz)0, (usz)12); // #error: power-of-two alignment
	$$memset(&a, (char)0, (usz)4, false, (usz)3); // #error: power-of-two alignment
	$$memset_inline(&a, (char)0, (usz)4, false, (usz)7); // #error: power-of-two alignment
	$$memcpy(&a, &b, (usz)4, false, (usz)4, (usz)0);
	$$memset(&a, (char)0, (usz)4, false, (usz)0);
}
