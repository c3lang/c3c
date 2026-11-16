// #safe: yes
// #opt: --infinite-loop-detect=yes
module test;

// Run by --compare-run: loops with work or an exit do not panic, the empty `for (;;) {}` does.
fn int main()
{
	int x;
	for (;; x++) { if (x > 5) break; }
	do {} while (false);
	while (false) {}
	for (;;) {}
}
