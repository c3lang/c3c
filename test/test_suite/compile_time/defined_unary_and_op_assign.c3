
struct Foo { int x; }
bitstruct Bs : char { bool a : 0; }
enum Color { RED, GREEN }
faultdef BAD;
fn int rf() => 1;
fn void main()
{
	Foo s; int x; float f; int* p; bool[<2>] bv; int[] sl; Color cc; String str; int[3] arr; any a; fault ft; Bs bs; int[<2>] iv;
	$assert !$defined(-s);
	$assert !$defined(+s);
	$assert !$defined(-p);
	$assert !$defined(-sl);
	$assert !$defined(-str);
	$assert !$defined(!s);
	$assert !$defined(!arr);
	$assert !$defined(++s);
	$assert !$defined(s++);
	$assert !$defined(--s);
	$assert !$defined(++arr);
	$assert !$defined(++a);
	$assert !$defined(ft++);
	$assert !$defined(++rf());
	$assert !$defined(x += s);
	$assert !$defined(x <<= f);
	$assert !$defined(f %= s);
	$assert !$defined(s += 1);
	$assert !$defined(p += 1.0);
	$assert !$defined(bs |= 1);
	$assert !$defined(rf() += 1);
	$assert $defined(-x);
	$assert $defined(-f);
	$assert $defined(+x);
	$assert $defined(!x);
	$assert $defined(!p);
	$assert $defined(!bv);
	$assert $defined(-iv);
	$assert $defined(++x);
	$assert $defined(x++);
	$assert $defined(++p);
	$assert $defined(++cc);
	$assert $defined(x += 1);
	$assert $defined(p += 1);
	$assert $defined(f -= 1.5);
	$assert $defined(x <<= 1);
}
