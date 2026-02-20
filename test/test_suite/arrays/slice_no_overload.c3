import std;

fn int main()
{
	List {int} int_list;
	int[] int_slice = int_list[..]; // #error:  You cannot slice 'List{int}'
	return 0;
}