import sourcelib, sourcelib::extra;

fn int main()
{
	return sourcelib::add(2, 3) == 5 && extra::twice(21) == 42 ? 0 : 1;
}
