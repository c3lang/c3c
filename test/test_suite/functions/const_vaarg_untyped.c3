macro foo2(int $a, $i...) // #error: Untyped constant vaargs are not supported. Use raw macro vaargs instead
{
	$foreach $ab : $i:
		$echo $ab;
	$endforeach
}
