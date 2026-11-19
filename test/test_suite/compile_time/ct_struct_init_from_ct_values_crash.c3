struct Aa
{
	int[4] a;
}
macro Aa[] c($nodes, int $idx, int $c, int $target)
{
	Aa[] $out = {};
	var $next = $nodes[0].a;
	$next[0] = $target;
	$out +++= (Aa){ .a = $next };
	return $out;
}
macro Aa[] d($nodes, int $idx)
{
	Aa[] $out = {};
	$out +++= (Aa){ $nodes[0].a };
	return $out;
}
macro int b(String[] $words) // #error: Missing return in macro
{
	Aa[] $nodes = { (Aa){} };
	$foreach $word : $words:
		var $next = $nodes[0].a[0];
		$if $next == 0: $next = (int)$nodes.len;
			$nodes = c($nodes, 0, 0,$next);
		$endif
		$nodes = d($nodes, 0);
	$endforeach
}
const int A = b({ "", "", "" });
