fn void main()
{
	ZString hello;
	String s = (String)hello; // #error: It is not possible to cast a ZString to a Strin

	char* foo = malloc(100);
    char[] bar = (char[])foo; // #error: However, you may slice the pointer

    int[4] arr = { 1, 2, 3, 4 };
    int[] slice1 = &arr;
    int* int_ptr = slice1;
    int[4] arr2 = (int[4])*int_ptr; // #error: you can could cast the pointer to the array
}