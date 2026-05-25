// #target: wasm32
fn int main()
{
	int x;
	int r = ((x + 31) >> 5) - 2u; // #error:  'int' cannot implicitly be narrowed to 'uint', but you may use a cast.
	return 0;
}