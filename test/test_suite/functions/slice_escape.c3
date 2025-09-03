fn int* test()
{
	int[3] x = { 1, 2, 4 };
	return (int[])&x; // #error: A pointer to a local variable will be invalid once the function returns. Allocate the data on the heap or temp memory to return a pointer
}
fn int main(String[] args)
{
	MyArray a;

	return 0;
}
struct MyArray
{
	int[] val;
}
fn int* MyArray.get_ref(self, usz idx) => &self.val[idx];