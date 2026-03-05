import std;
fn void main()
{
	Win32_UINT v1 = 1;
	int[<4>] v2 = 2;
	bool[<4>] vb = (v1 == v2); // #error: 'Win32_UINT' (uint) to 'int[<4>]' is not permitted, but you may do an explicit cast by placing
}
