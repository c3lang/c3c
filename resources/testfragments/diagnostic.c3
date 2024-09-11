import std;
struct Overalign
{
	double[<33>] x;
}

def Olist = List(<Overalign>);

fn void main()
{
	Olist l;
	Overalign y;
	for (int i = 0; i < 1000; i++)
	 {
	 io::printfn("Pushing %d", i);
	 l.push(y);
	 if (i > 3) io::printfn("Diff %d", (usz)l.get_ref(i) - (usz)l.get_ref(i - (usz)1));
	 }
}
