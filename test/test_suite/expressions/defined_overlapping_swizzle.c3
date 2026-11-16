fn void main()
{
	int[<3>] v;
	bool b = $defined(v.xx = { 1, 2 }); // #error: You cannot use swizzling to assign to multiple elements if they are overlapping
}
