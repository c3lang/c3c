fn void main()
{
	int** y;
	int* z;
	void** x = y;
	void** z2 = z; // #error: Implicitly casting
}