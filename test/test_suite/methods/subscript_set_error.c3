fn int main(String[] args)
{
	MyArray a = {{1, 4, 3, 6, 5}};
	a[0] = a[1]; // #error: Assigning to a subscript of 'MyArray' is not possible
	return 0;
}

struct MyArray
{
	int[] val;
}

fn int MyArray.get(self, usz idx) @operator([]) => self.val[idx];
fn usz MyArray.len(self) @operator(len) => self.val.len;