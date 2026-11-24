struct Aa { int[4] a; }
macro Aa[] c($nodes, int $target)
{
	Aa[] $out = {};
	var $next = $nodes[0].a;
	$next[0] = $target;
	$out +++= (Aa){ .a = $next };
	return $out;
}
macro Aa[] d($nodes)
{
	Aa[] $out = {};
	$out +++= (Aa){ $nodes[0].a };
	return $out;
}
macro int b(String[] $words)
{
	Aa[] $nodes = { (Aa){} };
	$foreach $word : $words:
		var $next = $nodes[0].a[0];
		$if $next == 0:
			$nodes = c($nodes, (int)$nodes.len);
		$endif
		$nodes = d($nodes);
	$endforeach
	return $nodes[0].a[0];
}
macro int alias_check()
{
	Aa[] $n = { (Aa){} };
	var $x = $n[0].a;
	$x[0] = 7;
	Aa $s = (Aa){ .a = $x };
	$x[1] = 8;
	return $n[0].a[0] * 100 + $s.a[1] * 10 + $s.a[0];
}
const int A = b({ "", "", "" });
const int B = alias_check();
fn void main() { $assert A == 1; $assert B == 7; }
