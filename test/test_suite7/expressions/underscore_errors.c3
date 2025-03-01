fn void main()
{
	int a = 1___0; // #error: consecutive
	int a = 1_0;
	float b = 1_0.3__4; // #error: consecutive
	float b = 1_0.3_4;
	int a2 = 0x1___0; // #error: consecutive
	int a2 = 0x1_0;
	float b2 = 0x1_0.3__4; // #error: consecutive
	float b2 = 0x1_0.3_4;
	int a3 = 0b1___0; // #error: consecutive
	int a3 = 0b1_0;
	int a3 = 0o1___0; // #error: consecutive
	int a3 = 0o1_0;
}