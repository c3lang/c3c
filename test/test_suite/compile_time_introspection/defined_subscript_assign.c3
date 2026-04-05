fn int main(String[] args)
{
	MyArray a = {{1, 4, 3, 6, 5}};
	$assert(!$defined(a[0] = 2));
	return 0;
}

struct MyArray
{
	int[] val;
}

fn int MyArray.get(self, sz idx) @operator([]) => self.val[idx];
fn sz MyArray.len(self) @operator(len) => self.val.len;