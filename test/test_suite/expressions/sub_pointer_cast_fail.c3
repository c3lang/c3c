fn int main()
{
	int ctrl = 0;
	char* op;
	if (ctrl >= 32)
	{
		int len = (ctrl >> 5) - 1;
        char* ref = op - ((ctrl & 31) << 8) - 1; // #error: an explicit cast to 'sz'
	}
	return 0;
}