import std;
struct Word {int x;}
typedef Byte = inline ushort;

macro Word Word.add0(self, int other) @operator(+) => { self.x + (int)other };
macro Word Word.add2(self, long other) @operator(+) => { self.x + (int)other };

fn int main()
{
	Word a = {1};
	Byte b = 2;
	Word c = a + b; // #error: has more than one candidate method
	return 0;
}
