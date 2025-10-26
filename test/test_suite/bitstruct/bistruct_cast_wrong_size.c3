module bug_repro;

bitstruct Edge_Flags : char
{
	bool left;
	bool top;
	bool right;
	bool bottom;
}

fn int main()
{
	Edge_Flags edges;
	Edge_Flags a = edges & (Edge_Flags)(1 << 8); // #error: The value '256' does not fit in the container type of the bitstruct
	int x = 0;
	Edge_Flags ab = edges & (Edge_Flags)x; // #error: 'int'
	return 0;
}