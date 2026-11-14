module compiler_rt::gen;
faultdef BAD;
fn int? may(int x) { if (x < 0) return BAD~; return x; }
fn int sum(int[] a) @weak @cname("__c3_test_sum")
{
	int s;
	foreach (i, v : a) s += v * (int)i;
	foreach_r (v : a) s -= v;
	int y = may(s) ?? 3;
	defer s++;
	switch (s) { case 1: s = 2; default: s = 3; }
	return s + y;
}
module test;
fn void main() {}
