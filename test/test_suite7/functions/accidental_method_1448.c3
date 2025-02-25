import std;
def MaybeInt = maybe::Maybe(<int>);

def Func = fn void (args...);

fn int main() {
	Func t;
	MaybeInt m = maybe::EMPTY(<int>);
	io::printfn("%s %s", m.value, m.has_value);
	return 0;
}