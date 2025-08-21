import std;
fn int main(String[] args)
{
	int[1024 * 1024] x; // #error: The size of this local variable (4096 Kb) exceeds the maximum allowed stack object size (128 Kb)
    return 0;
}
