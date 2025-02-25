fn void foo()
{
	int[] array;
	int* ptr = array;
	void* ptr2 = &array;
	int* ptr3 = &array; // #error: is not permitted
}