// #deprecation: no
import std;
alias MaybeInt = maybe::Maybe{int};

alias Func = fn void (args...);

fn int main() {
	Func t;
	MaybeInt m = maybe::EMPTY{int};
	io::printfn("%s %s", m.value, m.has_value);
	return 0;
}