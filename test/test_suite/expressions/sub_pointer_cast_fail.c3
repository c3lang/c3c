fn int main()
{
	int ctrl = 0;
	char* op;
	if (ctrl >= 32)
	{
		uint len = (ctrl >> 5) - 1;
        char* ref = op - ((ctrl & 31) << 8) - 1; // #error: an explicit cast to 'isz'
	}
	return 0;
}