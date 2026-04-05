import std;
struct Overalign
{
	double[<33>] x;
}

alias Olist = List{Overalign};

fn void main()
{
	Olist l;
	Overalign y;
	for (int i = 0; i < 1000; i++)
	 {
	 io::printfn("Pushing %d", i);
	 l.push(y);
	 if (i > 3) io::printfn("Diff %d", (sz)l.get_ref(i) - (sz)l.get_ref(i - (sz)1));
	 }
}
